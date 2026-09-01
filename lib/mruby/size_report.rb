require "json"
require "rbconfig"
require "mruby/source"

module MRuby
  # The +size.json+ of a build: how many bytes of machine code, data and
  # zero-fill what the build produced carries, written where a later build,
  # here or in another checkout, can be subtracted from it.
  #
  # Measured are the +libmruby+ archive and the executables the build links,
  # each with the object files that go directly into it, so a change in an
  # artifact's size can be read down to the object that carries it. The
  # section sizes come from a +size+ program of the build's own toolchain;
  # a build none can be found for, or whose objects the one found cannot
  # read, keeps its file sizes and carries +null+ sections.
  class SizeReport
    SCHEMA_VERSION = 1

    # A Berkeley-format row of `size` output: text, data and bss, their
    # decimal and hexadecimal totals, and the file the numbers are of,
    # spelled the way it was asked about. Berkeley is what GNU `size` and
    # `llvm-size` write unasked; a program that writes another shape
    # produces no row that names a file we asked about, and answers for
    # nothing rather than for the wrong column.
    BERKELEY_ROW = /\A\s*(\d+)\s+(\d+)\s+(\d+)\s+\d+\s+\h+\s+(.+?)\s*\z/

    # How many files one `size` run is asked about. A build has hundreds of
    # objects, and a command line that names them all at once is longer than
    # some platforms allow.
    BATCH = 100

    # Write a report for every build the config declared. The builds a build
    # makes for itself, the +mrbc+ donor, are no target the config asked for
    # and get none.
    def self.write
      MRuby.targets.each_value do |build|
        next if build.internal?

        report = new(build)
        report.write unless report.empty?
      end
    end

    # The commit the source tree sits at, or nil where neither the
    # repository nor the archive the tree was unpacked from could say.
    def self.commit
      revision = MRuby::Source::MRUBY_FULL_REVISION
      revision unless revision.empty?
    end

    # Whether the tree differs from the commit it sits at, counting what the
    # repository tracks: an untracked file is not part of the commit a clean
    # tree would be rebuilt from either. Nil where there is no repository to
    # ask, including the tree an archive was unpacked into, whose +.revision+
    # names a commit nothing can be compared against.
    def self.dirty
      return @dirty if defined?(@dirty)

      @dirty = begin
        if MRuby::Source::ROOT.join(".git").exist?
          out = IO.popen(MRuby::Source::MRUBY_GIT_REDIRECTS,
                         ["git", "-C", MRuby::Source::ROOT.to_s,
                          "status", "--porcelain", "--untracked-files=no"],
                         err: File::NULL, &:read)
          !out.to_s.strip.empty? if $?.success?
        end
      rescue SystemCallError
        nil
      end
    end

    # The `size` of the toolchain the build compiles with: the C compiler's
    # own spelling with `size` in the compiler's place, so a cross
    # toolchain's prefix carries over (`arm-none-eabi-gcc` names
    # `arm-none-eabi-size`), then `llvm-size`, then plain `size`. Nil where
    # none of them is there to run.
    def self.find_tool(cc)
      # The last word, so that a compiler run through a wrapper such as
      # `ccache` is read past it.
      cc = cc.to_s.split(" ").last.to_s
      dir, base = File.split(cc)
      candidates = []
      # The compiler's name may carry a trailing word of its own, a version
      # (`gcc-13`) or a threading model (`x86_64-w64-mingw32-gcc-posix`), so
      # one is allowed past; a prefix guessed wrong only names a candidate
      # that is not there, and the fallbacks answer.
      if base =~ /\A(.+-)(gcc|g\+\+|cc|c\+\+|clang|clang\+\+)(-[\w.]+)?\z/
        sized = "#{$1}size"
        candidates << (dir == "." ? sized : File.join(dir, sized))
      end
      candidates.push("llvm-size", "size")
      candidates.find { |candidate| executable?(candidate) }
    end

    def self.executable?(command)
      names = [command]
      exe = RbConfig::CONFIG["EXEEXT"].to_s
      names << command + exe unless exe.empty? || command.end_with?(exe)
      if command.include?("/")
        names.any? { |name| File.executable?(name) }
      else
        ENV["PATH"].to_s.split(File::PATH_SEPARATOR).any? do |path|
          names.any? { |name| File.executable?(File.join(path, name)) }
        end
      end
    end

    def initialize(build)
      @build = build
    end

    # Where this build's report goes.
    def path
      "#{@build.build_dir}/size.json"
    end

    # A build that produced nothing to measure is passed over rather than
    # handed an empty report, since its build directory may not even be
    # there yet.
    def empty?
      artifacts.empty?
    end

    # Write the report, and report where the file cannot be written rather
    # than raise: mruby is built from a read-only checkout as a dependency
    # of something else, and the report is no part of what that build is
    # for. A file that already holds these numbers is left alone, mtime and
    # all.
    def write
      json = JSON.pretty_generate(report) << "\n"
      File.write(path, json) unless File.exist?(path) && File.read(path) == json
      _pp "GEN", path.relative_path
    rescue SystemCallError => e
      warn "#{path.relative_path} not written: #{e.message}"
    end

    def report
      {
        "schema_version" => SCHEMA_VERSION,
        "target" => @build.name,
        "commit" => self.class.commit,
        "dirty" => self.class.dirty,
        "artifacts" => artifacts.map { |file| [relative(file), artifact(file)] }.to_h,
      }
    end

    private

    # What the build links or archives: the libmruby archive and the
    # executables of the build and its gems. Only what is actually there is
    # reported; a target that was not asked for this run has no artifact to
    # measure, and mrbtest belongs to the test task, not the build.
    def artifacts
      @artifacts ||= begin
        files = []
        files << @build.libmruby_static if @build.libmruby_enabled?
        bins = @build.bins + @build.gems.map { |gem| gem.bins }.flatten
        bins.uniq.sort.each do |bin|
          files << @build.exefile("#{@build.build_dir}/bin/#{bin}")
        end
        files.select { |file| File.exist?(file) }
      end
    end

    def artifact(file)
      objs = objects(file)
      # The archive is its members and nothing else, so its sections are the
      # sum of theirs, a sum an absent row poisons. An executable is not: the
      # linker drops, pads and adds, so it is measured as the file it is.
      row = if file == @build.libmruby_static
              rows = objs.map { |obj| sections[obj] }
              rows.transpose.map { |column| column.inject(:+) } unless
                rows.empty? || rows.include?(nil)
            else
              sections[file]
            end
      entry(file, row).merge(
        "objects" => objs.map { |obj| [relative(obj), entry(obj, sections[obj])] }.to_h
      )
    end

    # The objects that go directly into +file+, read off the prerequisites
    # of the task that makes it. The libraries an executable links are not
    # repeated under it; the archive answers for its own.
    def objects(file)
      return [] unless Rake::Task.task_defined?(file)

      Rake::Task[file].prerequisites.select do |prereq|
        prereq.end_with?(@build.exts.object) && File.exist?(prereq)
      end.sort
    end

    def entry(file, row)
      {
        "file_size" => File.size(file),
        "text" => row && row[0],
        "data" => row && row[1],
        "bss" => row && row[2],
      }
    end

    def sections
      @sections ||= begin
        files = artifacts.reject { |file| file == @build.libmruby_static }
        artifacts.each { |file| files.concat(objects(file)) }
        measure(files.uniq)
      end
    end

    def measure(files)
      rows = {}
      tool = size_tool
      return rows unless tool && !files.empty?

      wanted = {}
      files.each { |file| wanted[file] = true }
      begin
        files.each_slice(BATCH) do |slice|
          out = IO.popen([tool, *slice], :err => File::NULL, &:read)
          out.to_s.each_line do |line|
            next unless line =~ BERKELEY_ROW

            rows[$4] = [$1.to_i, $2.to_i, $3.to_i] if wanted[$4]
          end
        end
      rescue SystemCallError
        rows.clear
        warn "size.json of '#{@build.name}': #{tool} could not be run, " \
             "so no section sizes are measured"
        return rows
      end
      warn "size.json of '#{@build.name}': #{tool} read none of the " \
           "files, so no section sizes are measured" if rows.empty?
      rows
    end

    def size_tool
      return @size_tool if defined?(@size_tool)

      @size_tool = @build.size || self.class.find_tool(@build.cc.command)
      warn "size.json of '#{@build.name}': no size program found, so no " \
           "section sizes are measured" unless @size_tool
      @size_tool
    end

    def relative(file)
      file.delete_prefix("#{@build.build_dir}/")
    end
  end
end
