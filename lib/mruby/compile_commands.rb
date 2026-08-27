require "json"

module MRuby
  # The compilation database (+compile_commands.json+) of a build, made from
  # what the build already writes about itself.
  #
  # Every compile leaves two files beside its output: +<output>.flags+, the
  # command line it ran with (see +MRuby::Command::Compiler#run+), and
  # +<output>.d+, the files it read, whose first name is the source it
  # compiled. The two together are an entry of the database, so nothing has
  # to watch the build run to write one. A tool that traces the compiles
  # instead, +bear+ or +compiledb+ or an +strace+ wrapper, is not needed and
  # is not asked for.
  #
  # What the database holds is what the build compiled. A source that no
  # build in this directory has reached, a gem the config leaves out or a
  # port for another platform, has no record and so no entry; the +.clangd+
  # and +compile_flags.txt+ of the tree are what answer for those.
  class CompileCommands
    # The keys of a +.flags+ record, in the order it writes them.
    RECORD_KEYS = %w[command options flags].freeze

    # Where the tree's own database goes. clangd and the clang tools look
    # for one beside the file they are asked about and in the directories
    # above it, and a source of this tree has the source root above it and
    # no build directory, so the source root is where a database answers
    # for the tree.
    def self.tree_path
      "#{MRUBY_ROOT}/compile_commands.json"
    end

    # Write a database for every build that keeps one, and the tree's from
    # the build that speaks for it.
    #
    # A build's own database sits in its build directory, where a tool told
    # to read that build finds it: what clangd's +compile-commands-dir+
    # option names is a directory, so a cross build's flags are one option
    # away without the config or the environment being changed at all.
    #
    # A build that has compiled nothing is passed over rather than handed an
    # empty database, since its build directory may not even be there yet.
    def self.write
      speaks_for_tree = target
      keepers.each do |build|
        database = new(build)
        next if database.count.zero?

        write_one(database, database.path, "#{database.count} entries")
        next unless build.equal?(speaks_for_tree)

        write_one(database, tree_path, "from '#{build.name}'")
      end
    end

    # Write one database, and report where the file cannot be written rather
    # than raise.
    #
    # A file that cannot be written is not a build that has gone wrong: mruby
    # is built from a read-only checkout as a dependency of something else,
    # and the database is no part of what that build is for. Each file is
    # answered for on its own, so a source root that refuses the tree's copy
    # leaves the builds after it their own.
    def self.write_one(database, path, note)
      database.write(path)
      _pp "GEN", path.relative_path, note
    rescue SystemCallError => e
      warn "#{path.relative_path} not written: #{e.message}"
    end

    # The builds that keep a database: those a config declared, and that it
    # has not told to keep none.
    #
    # The builds a build makes for itself are left out. The +mrbc+ donor
    # compiles the same sources as its owner with the defines of a
    # bootstrap compiler, and it is no target the config asked for.
    def self.keepers
      MRuby.targets.each_value.reject(&:internal?).select(&:compile_commands_enabled?)
    end

    # The build the tree's own database is written from.
    #
    # The config settles it where it says so, since a config with several
    # builds is the only thing that knows which of them a reader of this
    # tree means. Failing that it is the build named +host+, the one that
    # runs on the machine the sources are being read on, and failing that
    # the first the config declares. +MRUBY_CDB_TARGET+ names another for a
    # single run, and has to name one that keeps a database: a build told to
    # keep none has none to copy, and saying so beats writing nothing and
    # leaving the reader to wonder which build answered.
    def self.target(name = ENV["MRUBY_CDB_TARGET"])
      builds = keepers
      if name
        return builds.find { |build| build.name == name } ||
          fail("MRUBY_CDB_TARGET names no build that keeps a compile_commands.json: #{name}")
      end

      builds.find(&:compile_commands_default?) ||
        builds.find { |build| build.name == "host" } ||
        builds.first
    end

    def initialize(build)
      @build = build
    end

    # Where this build's own database goes.
    def path
      "#{@build.build_dir}/compile_commands.json"
    end

    # Write the database to +path+.
    #
    # A file that already holds these entries is left alone, mtime and all,
    # so that a language server watching it does not reindex the tree after
    # a build that compiled nothing. The same database is written to more
    # than one path, so what it holds is worked out once.
    def write(path)
      File.write(path, json) unless File.exist?(path) && File.read(path) == json
    end

    # How many entries the database holds.
    def count
      entries.size
    end

    # The entries for the objects this build has compiled, ordered by the
    # source they compile so that two runs over the same build directory
    # write the same file.
    def entries
      @entries ||= begin
        list = records.map { |outfile| entry(outfile) }.compact
        list.sort_by! { |e| [e["file"], e["output"]] }
        # One source is compiled once here, but a build directory that holds
        # a nested build keeps a record for each. The first is the one the
        # sort settled on, and a database with two answers for a file has
        # none.
        list.uniq! { |e| e["file"] }
        list
      end
    end

    private

    def json
      @json ||= JSON.pretty_generate(entries) << "\n"
    end

    # The objects this build directory holds, named by the record beside
    # them.
    #
    # Every one of them is walked, and not only those the build has just
    # compiled or still declares. An entry earns its place by being true, not
    # by having been built a moment ago: an object whose flags no longer match
    # what its build would compile it with is removed and made again (see
    # +Command::Compiler#discard_foreign_output+), so every object a build
    # declares carries a record of how that build compiles it, and an object
    # left behind by a gem the config has since dropped carries the last true
    # account of how this tree compiled that source. The second is worth more
    # to a language server than the nothing that dropping it would leave.
    #
    # An object that is gone answers for nothing, so its record is passed
    # over rather than believed.
    #
    # The walk stops at the build directory of the +mrbc+ this build makes
    # for itself, which sits inside this one.
    def records
      paths = Dir.glob("#{@build.build_dir}/**/*#{@build.exts.object}.flags").sort
      donor = @build.mrbc_build
      if donor && donor.build_dir.start_with?("#{@build.build_dir}/")
        paths.reject! { |path| path.start_with?("#{donor.build_dir}/") }
      end
      paths.map { |path| path.sub(/\.flags\z/, "") }.select { |o| File.exist?(o) }
    end

    # The entry for an object, or nothing where the record cannot be read or
    # no longer describes a compile that could be run. A source that has been
    # renamed or deleted is still named by the records of the object it used
    # to be compiled to, and answering for a file that is not there helps
    # nobody.
    def entry(outfile)
      record = read_record("#{outfile}.flags")
      return nil unless record
      infile = source_of(outfile)
      return nil unless infile && File.exist?(infile)

      {
        "directory" => MRUBY_ROOT,
        "file" => infile,
        "command" => command_line(record, infile, outfile),
        "output" => outfile,
      }
    end

    # The command line of the compile, put back together the way
    # +MRuby::Command#_run+ builds it. The record keeps the options apart
    # from the flags they carry, and names neither file, so the two names
    # come from the output the record sits beside and the source it read.
    def command_line(record, infile, outfile)
      params = {
        :flags => record["flags"],
        :infile => @build.filename(infile),
        :outfile => @build.filename(outfile),
      }
      "#{record["command"]} #{record["options"] % params}"
    end

    def read_record(path)
      return nil unless File.exist?(path)
      record = {}
      File.foreach(path) do |line|
        key, value = line.chomp.split(": ", 2)
        record[key] = value if value && RECORD_KEYS.include?(key)
      end
      RECORD_KEYS.all? { |key| record.key?(key) } ? record : nil
    end

    # The source the last compile of +outfile+ read, which the compiler
    # names first in the dependency file it wrote:
    #
    #   /build/host/src/array.o: \
    #     /src/array.c \
    #     /include/mruby.h ...
    #
    # A toolchain that writes no dependency file leaves its objects out of
    # the database, rather than have the database guess at a source and
    # answer for a file with the flags of another.
    def source_of(outfile)
      dep = outfile.ext(".d")
      return nil unless File.exist?(dep)
      File.read(dep).gsub("\\\n ", "").lines.first.to_s[/\A\S+:\s+(\S+)/, 1]
    end
  end
end
