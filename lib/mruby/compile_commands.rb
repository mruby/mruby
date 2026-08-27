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

    # Where the database goes. clangd and the clang tools look for it beside
    # the file they are asked about and in the directories above it, so the
    # source root is where it answers for the whole tree.
    def self.path
      "#{MRUBY_ROOT}/compile_commands.json"
    end

    # Write the database of the tree and say so.
    def self.write
      build = target
      count = new(build).write(path)
      _pp "GEN", path.relative_path, "#{count} entries from '#{build.name}'"
    end

    # The build the database describes.
    #
    # A tree builds several targets and a tool that opens one source wants
    # one answer for it, so one target speaks for the tree: the one named
    # +host+ where there is one, and the first the config declares
    # otherwise. +MRUBY_CDB_TARGET+ names another.
    #
    # The builds a build makes for itself are left out. The +mrbc+ donor
    # compiles the same sources as its owner with the defines of a
    # bootstrap compiler, and it is no target the config asked for.
    def self.target(name = ENV["MRUBY_CDB_TARGET"])
      if name
        MRuby.targets[name] or fail "unknown build target: #{name}"
      else
        builds = MRuby.targets.each_value.reject(&:internal?)
        builds.find { |build| build.name == "host" } || builds.first
      end
    end

    def initialize(build)
      @build = build
    end

    # Write the database to +path+ and answer the number of entries in it.
    #
    # A file that already holds these entries is left alone, mtime and all,
    # so that a language server watching it does not reindex the tree after
    # a build that compiled nothing.
    def write(path)
      entries = self.entries
      json = JSON.pretty_generate(entries) << "\n"
      File.write(path, json) unless File.exist?(path) && File.read(path) == json
      entries.size
    end

    # The entries for the objects this build has compiled, ordered by the
    # source they compile so that two runs over the same build directory
    # write the same file.
    def entries
      list = records.map { |outfile| entry(outfile) }.compact
      list.sort_by! { |e| [e["file"], e["output"]] }
      # One source is compiled once here, but a build directory that holds a
      # nested build keeps a record for each. The first is the one the sort
      # settled on, and a database with two answers for a file has none.
      list.uniq! { |e| e["file"] }
      list
    end

    private

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
