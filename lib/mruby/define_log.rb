require "mruby/build/define_list"

module MRuby
  # The log a build opens with: every define each target compiles with, who
  # carries it (the config, a compiler's list, a gem's own compiler) and the
  # file and line that wrote it, read from the origins a DefineList records.
  module DefineLog
    COMPILERS = MRuby::Build::COMPILERS

    def self.print
      MRuby.targets.each_value do |build|
        next if build.internal? || build.define_log_disabled?

        rows = collect(build)
        next if rows.empty?

        puts "Defines of '#{build.name}':"
        define_width = rows.keys.map(&:size).max
        owner_width = rows.values.map {|row| row[:owners].join(", ").size}.max
        rows.sort.each do |define, row|
          line = format("  %-#{define_width}s  %-#{owner_width}s  %s",
                        define, row[:owners].join(", "), row[:origins].join(", "))
          line << "  #{row[:mark]}" if row[:mark]
          puts line
        end
        puts
      end
    end

    def self.collect(build)
      rows = {}

      note(rows, build.defines, build.defines, "conf")
      COMPILERS.each do |name|
        compiler = build.send(name)
        note(rows, compiler.defines, compiler.defines, name)
        note(rows, compiler.internal_defines, compiler.internal_defines,
             "#{name} internal")
      end
      # A gem's compilers start as clones of the build's, so what the gem
      # itself wrote is what its list holds beyond the build's.
      build.gems.each do |gem|
        COMPILERS.each do |name|
          gem_list = gem.send(name).defines
          extra = gem_list.flatten.map(&:to_s) -
                  build.send(name).defines.flatten.map(&:to_s)
          note(rows, extra, gem_list, "#{gem.name} #{name}") unless extra.empty?
        end
      end

      mark_losses(build, rows)
      rows.each_value {|row| row[:owners] = fold_compilers(row[:owners])}
      rows
    end

    # When one name is held with two values, the rows alone do not say which
    # one an object compiles with, so the losing rows are marked. A compile
    # line is `[defines, internal_defines, build.defines]` of the compiler
    # the object is built with (`Compiler#all_flags`), the build's own or a
    # gem's copy, and the last -D of a name is the one in effect: the winner
    # of a context is the last entry of the name there. A row that never wins
    # is marked with what beats it; a row beaten only in some contexts, a gem
    # redefining a build-wide name for its own objects, is marked with where.
    def self.mark_losses(build, rows)
      contested = rows.keys.group_by {|d| DefineList.define_name(d)}
                      .select {|_, defines| defines.uniq.size > 1}
      return if contested.empty?

      present = Hash.new {|h, k| h[k] = []}  # define => context labels
      winner = {}                            # [name, context label] => define
      contexts(build).each do |label, compiler|
        [compiler.defines, compiler.internal_defines, build.defines]
          .flatten.each do |value|
          define = value.to_s
          name = DefineList.define_name(define)
          next unless contested.key?(name)
          present[define] |= [label]
          winner[[name, label]] = define
        end
      end

      contested.each do |name, defines|
        defines.each do |define|
          losses = present[define].reject {|label| winner[[name, label]] == define}
          next if losses.empty?
          rows[define][:mark] = format_losses(losses, present[define], name, winner)
        end
      end
    end

    # Every list an object's -D flags can be assembled from: the build's own
    # compilers and each gem's copies of them.
    def self.contexts(build)
      contexts = {}
      COMPILERS.each {|name| contexts[name] = build.send(name)}
      build.gems.each do |gem|
        COMPILERS.each {|name| contexts["#{gem.name} #{name}"] = gem.send(name)}
      end
      contexts
    end

    def self.format_losses(losses, presence, name, winner)
      groups = losses.group_by {|label| winner[[name, label]]}
      everywhere = losses.size == presence.size && groups.size == 1
      marks = groups.map do |win, labels|
        everywhere ? "#{win} wins" : "#{win} wins for #{fold_compilers(labels).join(', ')}"
      end
      "[#{marks.join('; ')}]"
    end

    def self.note(rows, values, list, owner)
      origins = list.is_a?(DefineList) ? list.origins : {}
      values.flatten.each do |value|
        define = value.to_s
        # One MRBGEM_*_VERSION per gem, written by the machinery
        # (`GemList#check`), not by anything a person configured.
        next if define.start_with?("MRBGEM_")
        row = (rows[define] ||= {owners: [], origins: []})
        row[:owners] |= [owner]
        row[:origins] |= (origins[define] || ["(unknown)"])
      end
    end

    # "cc, cxx, objc, asm" is every compiler; say so.
    def self.fold_compilers(owners)
      [nil, " internal"].each do |suffix|
        set = COMPILERS.map {|name| "#{name}#{suffix}"}
        if (set - owners).empty?
          owners = (owners - set[1..]).map {|o| o == set[0] ? "compilers#{suffix}" : o}
        end
      end
      owners
    end
  end
end
