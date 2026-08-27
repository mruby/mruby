require "mruby/core_ext"
require "mruby/build/load_gems"
require "mruby/build/command"
autoload :Find, "find"

module MRuby
  autoload :Gem, "mruby/gem"
  autoload :Lockfile, "mruby/lockfile"
  autoload :Presym, "mruby/presym"

  INSTALL_PREFIX = ENV['PREFIX'] || ENV['INSTALL_PREFIX'] || '/usr/local'
  INSTALL_DESTDIR = ENV['DESTDIR'] || ''

  class << self
    def targets
      @targets ||= {}
    end

    def each_target(&block)
      return to_enum(:each_target) if block.nil?
      @targets.each do |key, target|
        target.instance_eval(&block)
      end
    end

    # Bind every cross build to the build it borrows `mrbc` from.
    #
    # A cross build cannot settle this as it is declared, because the `host`
    # it would borrow from may be written after it, and `Build.new` reopens a
    # name already taken rather than initialising it afresh: a build generated
    # then to fill the gap would swallow the `host` the config goes on to
    # declare. So the question is asked here instead, once from the Rakefile,
    # where the config has been read whole and the set of targets is final.
    # This still runs before the gems are set up, both because they ask for
    # `mrbcfile` as they go and because the answer turns on defines, which the
    # config alone has written this early.
    def resolve_mrbc_hosts
      mrbc_builds = {}
      targets.values.grep(CrossBuild).each{|target| target.bind_mrbc_host(mrbc_builds)}
    end
  end

  class Toolchain
    class << self
      attr_accessor :toolchains

      def guess
        if cc = ENV["CC"] || ENV["CXX"]
          return "clang" if cc.include?("clang")
        else
          return "clang" if RUBY_PLATFORM =~ /darwin|(?:free|open)bsd/
          return "gcc" if RUBY_PLATFORM.include?("cygwin")
          return "visualcpp" if ENV.include?("VisualStudioVersion")
          return "visualcpp" if ENV.include?("VSINSTALLDIR")
        end
        "gcc"
      end
    end

    def initialize(name, &block)
      @name, @initializer = name.to_s, block
      MRuby::Toolchain.toolchains[@name] = self
    end

    def setup(conf, params={})
      conf.instance_exec(conf, params, &@initializer)
    end

    self.toolchains = {}
  end

  class Build
    class << self
      attr_accessor :current

      def mruby_config_path
        path = ENV['MRUBY_CONFIG'] || ENV['CONFIG']
        if path.nil? || path.empty?
          path = if Dir.pwd != MRUBY_ROOT && File.file?("./build_config.rb")
            "./build_config.rb"
          else
            "#{MRUBY_ROOT}/build_config/default.rb"
          end
        elsif !File.file?(path) && !Pathname.new(path).absolute?
          f = "#{MRUBY_ROOT}/build_config/#{path}.rb"
          path = File.exist?(f) ? f : File.extname(path).empty? ? f : path
        end
        path
      end

      def install_dir
        @install_dir ||= ENV['INSTALL_DIR'] || "#{MRUBY_ROOT}/bin"
      end
    end

    include Rake::DSL
    include LoadGems
    attr_accessor :name, :bins, :exts, :file_separator, :build_dir, :gem_clone_dir, :defines, :libdir_name
    attr_reader :products, :libmruby_core_objs, :libmruby_objs, :gems, :toolchains, :presym, :mrbc_build, :gem_dir_to_repo_url
    attr_reader :build_root
    attr_reader :install_excludes, :port_names

    alias libmruby libmruby_objs

    COMPILERS = %w(cc cxx objc asm)
    COMMANDS = COMPILERS + %w(linker archiver yacc gperf git exts mrbc)
    attr_block MRuby::Build::COMMANDS

    Exts = Struct.new(:object, :executable, :library, :presym_preprocessed)

    # `rake -m` resolves the dependencies of several outputs at once, and each
    # of them compares its own record, so the report is guarded to print once.
    FLAGS_CHANGE_LOCK = Mutex.new

    def initialize(name='host', build_dir=nil, internal: false, &block)
      @name = name.to_s

      unless current = MRuby.targets[@name]
        if ENV['OS'] == 'Windows_NT'
          @exts = Exts.new('.o', '.exe', '.a', '.pi')
        else
          @exts = Exts.new('.o', '', '.a', '.pi')
        end

        build_dir = build_dir || ENV['MRUBY_BUILD_DIR'] || "#{MRUBY_ROOT}/build"

        @file_separator = '/'
        # The directory every target of this config builds under. `@build_dir`
        # is this target's share of it, and the clones of remote gems sit
        # beside that. `MRUBY_BUILD_DIR` may put it anywhere, so this is the
        # one a build maps to keep its own paths out of what it compiles.
        @build_root = build_dir
        @build_dir = "#{build_dir}/#{@name}"
        @gem_clone_dir = "#{build_dir}/repos/#{@name}"
        @libdir_name = (self.kind_of?(MRuby::CrossBuild) ? nil : ENV["MRUBY_SYSTEM_LIBDIR_NAME"]) || "lib"
        @install_prefix = nil
        @install_excludes = []
        @defines = []
        @defines_final = false
        @flags_change_reported = false
        @cc = Command::Compiler.new(self, %w(.c), label: "CC")
        @cxx = Command::Compiler.new(self, %w(.cc .cxx .cpp), label: "CXX")
        @objc = Command::Compiler.new(self, %w(.m), label: "OBJC")
        @asm = Command::Compiler.new(self, %w(.S .asm .s), label: "ASM")
        @linker = Command::Linker.new(self)
        @archiver = Command::Archiver.new(self)
        @yacc = Command::Yacc.new(self)
        @gperf = Command::Gperf.new(self)
        @git = Command::Git.new(self)
        @mrbc = Command::Mrbc.new(self)

        @products = []
        @bins = []
        @gems = MRuby::Gem::List.new
        @libmruby_core_objs = []
        @libmruby_objs = [@libmruby_core_objs]
        @enable_libmruby = true
        @build_mrbtest_lib_only = false
        @cxx_exception_enabled = false
        @cxx_exception_disabled = false
        @cxx_abi_enabled = false
        @enable_bintest = false
        @enable_test = false
        @enable_lock = true
        @enable_benchmark = true
        @enable_compile_commands = true
        @compile_commands_default = false
        @mrbcfile_external = false
        @file_prefix_map = nil
        @internal = internal
        @toolchains = []
        @port_names = nil
        @gem_dir_to_repo_url = {}

        MRuby.targets[@name] = current = self
      end

      MRuby::Build.current = current
      begin
        current.instance_eval(&block)
      ensure
        # Before the compilers are copied: `create_mrbc_build` below takes a
        # copy of them, and so does every gem when it is set up.
        current.apply_default_file_prefix_map
        if current.libmruby_enabled? && !current.mrbcfile_external?
          current.create_mrbc_build if current.host? || current.gems["mruby-bin-mrbc"]
        end
        current.presym = Presym.new(current)
      end
    end

    def libmruby_enabled?
      @enable_libmruby
    end

    def disable_libmruby
      @enable_libmruby = false
    end

    def debug_enabled?
      @enable_debug
    end

    def enable_debug
      compilers.each do |c|
        c.internal_defines += %w(MRB_DEBUG)
        c.setup_debug(self)
      end
      @mrbc.compile_options += ' -g'

      @enable_debug = true
    end

    # Have the compilers write +to+ in place of +from+ wherever they write a
    # path of their own: in `__FILE__`, which `mrb_assert` reaches through
    # `assert`, and in the debug information `-g` writes. A compiler that
    # cannot map a path is left alone.
    def file_prefix_map(from, to)
      compilers.each {|c| c.file_prefix_maps[from] = to}
    end

    # Keep the paths of this build out of what it compiles, by mapping the two
    # directories its sources come from: the mruby tree, and the build
    # directory, where the generated sources and the presym headers are.
    #
    # Every build does this much on its own, so a config calls this only to
    # write the two names itself, and by calling it says that the build has
    # no say: the names a config writes are the only ones its output carries.
    #
    # The build directory is written as `build` under the name of the tree,
    # the place it takes when nothing moves it, so that a build with
    # `MRUBY_BUILD_DIR` pointing outside the tree compiles what a build inside
    # the tree compiles.
    #
    # The two maps overlap where the build directory is inside the tree, and
    # both `gcc` and `clang` answer a path both cover with the longer prefix,
    # which is the build directory. Under the names above that is the answer
    # either map gives; a caller that names the two apart is asking for the
    # longer one to win.
    #
    # This says nothing of the paths `mrbc` writes: the file names it records
    # for a backtrace under `enable_debug` are the ones the build passes it,
    # and no compiler flag reaches them.
    def enable_file_prefix_map(source: ".", build: "#{source}/build")
      file_prefix_map(MRUBY_ROOT, source)
      file_prefix_map(@build_root, build)
      @file_prefix_map = true
    end

    # Compile with the paths of this build as they are, which a build that
    # is meant to be debugged from outside the tree wants: a debugger looks
    # for the sources of a mapped build under the mapped names, and finds
    # them only from the directory they were mapped against.
    def disable_file_prefix_map
      @file_prefix_map = false
    end

    # Set target port names for this build.
    # Each gem compiles the first matching ports/<name>/ directory;
    # later names in the list act as fallbacks for gems that don't
    # ship a port for the earlier names.
    #   conf.ports :esp32
    #   conf.ports :rp2040, :posix    # use rp2040 if available, else posix
    def ports(*names)
      @port_names = names.map { |n| n.to_s }
    end

    # Returns the effective port names for this build.
    # If not explicitly set, auto-detects :posix or :win for host builds.
    def effective_ports
      return @port_names if @port_names
      if kind_of?(MRuby::CrossBuild)
        []
      elsif ENV['OS'] == 'Windows_NT' ||
            ('A'..'Z').any? { |v| Dir.exist?("#{v}:") }
        ['win']
      else
        ['posix']
      end
    end

    def disable_lock
      @enable_lock = false
    end

    def lock_enabled?
      Lockfile.enabled? && @enable_lock
    end

    # Whether this build writes a `compile_commands.json` of its own compiles
    # into its build directory.
    def compile_commands_enabled?
      @enable_compile_commands
    end

    # Whether this build is the one the tree's own `compile_commands.json`
    # is written from. A tool opening a source without being told which build
    # to read it as wants one answer, and a config with several builds is the
    # only thing that knows which of them a reader of this tree means.
    def compile_commands_default?
      @compile_commands_default
    end

    # +default:+ makes this build the one the tree's `compile_commands.json`
    # is written from. Two builds claiming it would leave the answer to
    # declaration order, so the second to claim it says so.
    #
    # Almost no configuration needs to call this. Every build keeps its
    # records already, and which one speaks for the tree is settled without
    # being told: a build named `host`, or failing that the first one the
    # configuration declares. A config with several builds says which it means
    # by declaring that one first, which is what `build_config/boxing.rb`
    # does. What is left for +default:+ is the case where the build that
    # should speak cannot be the first one declared -- an order the
    # configuration needs for another reason -- and there is no such config in
    # this tree.
    def enable_compile_commands(default: false)
      @enable_compile_commands = true
      return unless default

      claimed = MRuby.targets.each_value.find do |build|
        !build.equal?(self) && !build.internal? && build.compile_commands_default?
      end
      fail "compile_commands default is already '#{claimed.name}'" if claimed
      @compile_commands_default = true
    end

    def disable_compile_commands
      @enable_compile_commands = false
      @compile_commands_default = false
    end

    def disable_cxx_exception
      if @cxx_exception_enabled or @cxx_abi_enabled
        raise "cxx_exception already enabled"
      end
      @cxx_exception_disabled = true
    end

    def enable_cxx_exception
      return if @cxx_exception_enabled
      return if @cxx_abi_enabled
      if @cxx_exception_disabled
        raise "cxx_exception disabled"
      end
      @cxx_exception_enabled = true
      compilers.each { |c|
        c.internal_defines += %w(MRB_USE_CXX_EXCEPTION)
        c.flags << c.cxx_exception_flag
      }
      linker.command = cxx.command if toolchains.find { |v| v == 'gcc' }
    end

    def cxx_exception_enabled?
      @cxx_exception_enabled
    end

    def cxx_abi_enabled?
      @cxx_abi_enabled
    end

    def enable_cxx_abi
      return if @cxx_abi_enabled
      if @cxx_exception_enabled
        raise "cxx_exception already enabled"
      end
      compilers.each { |c|
        c.internal_defines += %w(MRB_USE_CXX_EXCEPTION MRB_USE_CXX_ABI)
        c.flags << c.cxx_compile_flag
        c.flags = c.flags.flatten - c.cxx_invalid_flags.flatten
      }
      linker.command = cxx.command if toolchains.find { |v| v == 'gcc' }
      @cxx_abi_enabled = true
    end

    def benchmark_enabled?
      @enable_benchmark
    end

    def disable_benchmark
      @enable_benchmark = false
    end

    def compile_as_cxx(src, cxx_src = nil, obj = nil, includes = [])
      #
      # If `cxx_src` is specified, this method behaves the same as before as
      # compatibility mode, but `.d` file is not read.
      #
      # If `cxx_src` is omitted, `.d` file is read by using mruby standard
      # Rake rule (C++ source name is also changed).
      #
      if cxx_src
        obj ||= cxx_src + @exts.object
        dsts = [obj]
        dsts << (cxx_src + @exts.presym_preprocessed)
        defines = []
        include_paths = ["#{MRUBY_ROOT}/src", *includes]
        dsts.each do |dst|
          file dst => cxx_src do |t|
            cxx.run t.name, t.prerequisites.first, defines, include_paths
          end
        end
      else
        cxx_src = "#{build_dir}/#{src.relative_path.to_s.remove_leading_parents}".ext << "-cxx.cxx"
        obj = cxx_src.ext(@exts.object)
      end

      file cxx_src => [src, __FILE__] do |t|
        mkdir_p File.dirname t.name
        IO.write t.name, <<EOS
#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS

#ifndef MRB_USE_CXX_ABI
extern "C" {
#endif
#include "#{File.absolute_path src}"
#ifndef MRB_USE_CXX_ABI
}
#endif
EOS
      end

      obj
    end

    def enable_bintest
      @enable_bintest = true
    end

    def bintest_enabled?
      @enable_bintest
    end

    def toolchain(name=Toolchain.guess, params={})
      name = name.to_s
      tc = Toolchain.toolchains[name] || begin
        path = "#{MRUBY_ROOT}/tasks/toolchains/#{name}.rake"
        fail "Unknown #{name} toolchain" unless File.exist?(path)
        load path
        Toolchain.toolchains[name]
      end
      tc.setup(self, params)
      @toolchains.unshift name
    end

    def primary_toolchain
      @toolchains.first
    end

    def root
      MRUBY_ROOT
    end

    def enable_test
      @enable_test = true
    end
    alias build_mrbtest enable_test

    def test_enabled?
      @enable_test
    end

    def build_mrbc_exec
      gem :core => 'mruby-bin-mrbc' unless @gems['mruby-bin-mrbc']
    end

    def locks
      Lockfile.build(@name)
    end

    def mrbcfile
      return @mrbcfile if @mrbcfile

      if (gem = @gems["mruby-bin-mrbc"])
        @mrbcfile = exefile("#{gem.build.build_dir}/bin/mrbc")
      elsif !host? && (host = MRuby.targets["host"])
        if (gem = host.gems["mruby-bin-mrbc"])
          @mrbcfile = exefile("#{gem.build.build_dir}/bin/mrbc")
        elsif host.mrbcfile_external?
          @mrbcfile = host.mrbcfile
        end
      end
      @mrbcfile || fail("external mrbc or mruby-bin-mrbc gem in current('#{@name}') or 'host' build is required")
    end

    def mrbcfile=(path)
      @mrbcfile = path
      @mrbcfile_external = true
    end

    # Whether this build has a `mrbc` to lend: one it was given, one it
    # generated for itself (`create_mrbc_build` hands that one over through
    # `mrbcfile=`), or one it builds from the gem. This is the question
    # `mrbcfile` asks of `host` on behalf of a native build; a build with
    # `disable_libmruby` and no `mruby-bin-mrbc` answers no.
    def supplies_mrbc?
      mrbcfile_external? || !@gems['mruby-bin-mrbc'].nil?
    end

    def mrbcfile_external?
      @mrbcfile_external
    end

    def compilers
      COMPILERS.map do |c|
        instance_variable_get("@#{c}")
      end
    end

    # Declare that every gem in the build has had its mrbgem.rake run, so the
    # defines gems contribute through `spec.build.defines` are all in.  Called
    # once from the Rakefile, between loading the build config and defining
    # any rule.
    def defines_final!
      @defines_final = true
    end

    # True when this build compiles with -D<name>, whether the build config
    # asked for it, a gem contributed it, or the build added it from one of
    # its own switches.  A gem reads this to configure itself against a
    # capability another gem provides.
    #
    # Until `defines_final!` the answer would depend on how far down the gem
    # list the caller sits, since a gem contributes its defines when its own
    # mrbgem.rake body runs.  Rather than hand back an answer that is right
    # for some gem orders and wrong for others, this refuses to answer at all
    # before then.  `spec.build_settings` is the hook that runs late enough.
    def has_define?(name)
      unless @defines_final
        fail "build.has_define?(#{name.inspect}) cannot be answered while gems " \
             "are still being set up, because a gem contributes its defines " \
             "then. Ask from a `spec.build_settings` block instead."
      end
      name = name.to_s
      # A define may carry a value, as `FOO=1` does, so compare the name and
      # not the value.  The `-D` is the compiler's, added when the flags are
      # assembled, and is no part of the name.
      return true if defines.flatten.any? {|d| d.to_s.split('=', 2).first == name}
      compilers.any? {|c| c.has_define?(name)}
    end

    def define_rules
      [@cc, *(@cxx if cxx_exception_enabled?)].each do |compiler|
        compiler.define_rules(@build_dir, MRUBY_ROOT, @exts.object)
        compiler.define_rules(@build_dir, MRUBY_ROOT, @exts.presym_preprocessed)
      end
    end

    def define_installer_outline(src, dst)
      file dst => src do
        _pp "GEN", src.relative_path, dst.relative_path
        mkdir_p(File.dirname(dst))
        yield dst
      end
      dst
    end

    if ENV['OS'] == 'Windows_NT'
      def define_installer(src)
        dst = "#{self.class.install_dir}/#{File.basename(src)}".pathmap("%X.bat")
        define_installer_outline(src, dst) do
          File.write dst, <<~BATCHFILE
            @echo off
            call "#{File.expand_path(src)}" %*
          BATCHFILE
        end
      end
    else
      def define_installer(src)
        dst = "#{self.class.install_dir}/#{File.basename(src)}"
        define_installer_outline(src, dst) do
          File.unlink(dst) rescue nil
          File.symlink(src.relative_path_from(self.class.install_dir), dst)
        end
      end
    end

    def define_installer_if_needed(bin)
      exe = exefile("#{build_dir}/bin/#{bin}")
      host? ? define_installer(exe) : exe
    end

    def filename(name)
      if name.is_a?(Array)
        name.flatten.map { |n| filename(n) }
      else
        name.gsub('/', file_separator)
      end
    end

    def exefile(name)
      if name.is_a?(Array)
        name.flatten.map { |n| exefile(n) }
      elsif File.extname(name).empty?
        "#{name}#{exts.executable}"
      else
        # `name` sometimes have (non-standard) extension (e.g. `.bat`).
        name
      end
    end

    def objfile(name)
      if name.is_a?(Array)
        name.flatten.map { |n| objfile(n) }
      else
        "#{name}#{exts.object}"
      end
    end

    def libfile(name)
      if name.is_a?(Array)
        name.flatten.map { |n| libfile(n) }
      else
        "#{name}#{exts.library}"
      end
    end

    def build_mrbtest_lib_only
      @build_mrbtest_lib_only = true
    end

    def build_mrbtest_lib_only?
      @build_mrbtest_lib_only
    end

    def verbose_flag
      Rake.verbose ? ' -v' : ''
    end

    def run_test
      puts ">>> Test #{name} <<<"
      mrbtest = exefile("#{build_dir}/bin/mrbtest")
      sh "#{filename mrbtest.relative_path}#{verbose_flag}"
      puts
    end

    def run_bintest
      puts ">>> Bintest #{name} <<<"
      targets = @gems.select { |v| File.directory? "#{v.dir}/bintest" }.map { |v| filename v.dir }
      mrbc = @gems["mruby-bin-mrbc"] ? exefile("#{@build_dir}/bin/mrbc") : mrbcfile
      env = {"BUILD_DIR" => @build_dir, "MRBCFILE" => mrbc,
             "EXECUTABLE_EXT" => @exts.executable}
      bintest = File.join(MRUBY_ROOT, "test/bintest.rb")
      sh env, "ruby #{bintest}#{verbose_flag} #{targets.join ' '}"
    end

    # Report that this build directory holds output produced by another
    # configuration, once for the whole directory: the command line is
    # recorded per output, so every output that follows carries the same
    # change and would report it again.
    #
    # `recorded` is nil when there is no record at all, which is what a
    # directory built before this check looks like.
    def report_flags_change(recorded, current)
      FLAGS_CHANGE_LOCK.synchronize do
        return if @flags_change_reported
        @flags_change_reported = true

        unless recorded
          warn "#{build_dir}: output here has no record of what built it, rebuilding it"
          return
        end

        warn "#{build_dir}: output here was built by another configuration, rebuilding it"
        recorded.lines.zip(current.lines).each do |before, after|
          next if before == after
          field = (after || before)[/\A[^:]+/]
          before, after = [before, after].map {|line| line.to_s.split(": ", 2)[1].to_s.split}
          warn "  #{field} added: #{(after - before).join(" ")}" unless (after - before).empty?
          warn "  #{field} removed: #{(before - after).join(" ")}" unless (before - after).empty?
        end
      end
    end

    def print_build_summary
      puts "================================================"
      puts "      Config Name: #{@name}"
      puts " Output Directory: #{self.build_dir.relative_path}"
      puts "         Binaries: #{@bins.join(', ')}" unless @bins.empty?
      unless @gems.empty?
        puts "    Included Gems:"
        gems = @gems.sort_by { |gem| gem.name }
        gems.each do |gem|
          gem_version = " - #{gem.version}" if gem.version != '0.0.0'
          gem_summary = " - #{gem.summary}" if gem.summary
          puts "             #{gem.name}#{gem_version}#{gem_summary}"
          puts "               - Binaries: #{gem.bins.join(', ')}" unless gem.bins.empty?
        end
      end
      puts "================================================"
      puts
    end

    def libmruby_static
      libfile("#{build_dir}/#{libdir_name}/libmruby")
    end

    def libraries
      [libmruby_static]
    end

    def host?
      @name == "host"
    end

    def internal?
      @internal
    end

    def each_header_files(&block)
      return to_enum(__method__) unless block

      basedir = File.join(MRUBY_ROOT, "include")
      Find.find(basedir) do |d|
        next unless File.file? d
        yield d
      end

      @gems.each { |g| g.each_header_files(&block) }

      self
    end

    def install_prefix
      @install_prefix || (self.name == "host" ? MRuby::INSTALL_PREFIX :
                                                File.join(MRuby::INSTALL_PREFIX, "mruby/#{self.name}"))
    end

    def install_prefix=(dir)
      @install_prefix = dir&.to_s
    end

    protected

    attr_writer :presym

    # Map the directories of this build unless the config has spoken for
    # itself, either way.
    def apply_default_file_prefix_map
      enable_file_prefix_map if @file_prefix_map.nil?
    end

    def create_mrbc_build
      exclusions = %i[@name @build_dir @gems @enable_test @enable_bintest @internal @install_excludes]
      name = "#{@name}/mrbc"
      MRuby.targets.delete(name)
      build = self.class.new(name, internal: true){}
      build.build_dir = "#{@build_dir}/mrbc"
      instance_variables.each do |n|
        next if exclusions.include?(n)
        v = instance_variable_get(n)
        v = case v
            when nil, true, false, Numeric; v
            when String; v.clone
            when Command; v.clone.tap { |u| u.build = build }
            else Marshal.load(Marshal.dump(v))  # deep clone
            end
        build.instance_variable_set(n, v)
      end
      # Bootstrap mrbc with the Prism compiler so mrblib is compiled with
      # matching presyms. This runs before dependency resolution.
      build.build_mrbc_exec
      build.disable_libmruby
      build.presym = Presym.new(build)
      @mrbc_build = build
      self.mrbcfile = build.mrbcfile
      build
    end
  end # Build

  class CrossBuild < Build
    attr_block %w(test_runner)
    # cross compiling targets for building native extensions.
    # host  - arch of where the built binary will run
    # build - arch of the machine building the binary
    attr_accessor :host_target, :build_target

    def initialize(name, build_dir=nil, &block)
      @test_runner = Command::CrossTestRunner.new(self)
      super
    end

    def mrbcfile
      return super if mrbcfile_external?
      unless @mrbc_host
        fail "the `mrbc' for '#{@name}' is not bound yet; `MRuby.resolve_mrbc_hosts' " \
             "binds it once the whole build config has been read"
      end
      MRuby::targets[@mrbc_host].mrbcfile
    end

    # The defines a target and the `mrbc` it borrows have to agree on.
    #
    # The bytecode `mrbc` emits has to be loadable on the target, and
    # `src/load.c` refuses a whole irep over a single pool entry the target
    # cannot represent: under `MRB_NO_FLOAT` a float literal is one. A define
    # that decides what a pool entry may hold belongs in this list, which is
    # where the comparison below, the name of a generated build and the
    # defines it carries all read the question from.
    MRBC_DEFINES = %w[MRB_NO_FLOAT].freeze

    # Bind this target to the build it borrows `mrbc` from, generating one
    # where none will do.
    #
    # A `host` the build config declares is borrowed as it is written. Where
    # there is none, where it has no `mrbc` to lend, or where it answers
    # otherwise, the target borrows a build generated here, named for the
    # answer it carries rather than for the target that asked for it: targets
    # that agree share one, and it belongs to none of them. `mrbc_builds`
    # carries the ones this pass has generated, so a config with several
    # cross targets builds `mrbc` once per answer.
    #
    # The name is one the build config does not write, so a build generated
    # here cannot take a name the config wants, and `build/mrbc` is where
    # `mrbc` built for its own sake goes, which is where `build_config/mrbc.rb`
    # already puts it. `build/host` is left to a `host` the config declares.
    def bind_mrbc_host(mrbc_builds)
      return if mrbcfile_external?
      needed = mrbc_defines(self)
      host = MRuby.targets['host']
      if host && host.supplies_mrbc? && mrbc_defines(host) == needed
        @mrbc_host = 'host'
      else
        @mrbc_host = (mrbc_builds[needed] ||= generate_mrbc_build(needed))
        # `tasks/presym.rake` reads this to leave the generated build's
        # objects to it, which a target named `mrbc` would otherwise scan as
        # its own.
        @mrbc_build = MRuby.targets[@mrbc_host]
      end
    end

    def run_test
      @test_runner.runner_options << verbose_flag
      mrbtest = exefile("#{build_dir}/bin/mrbtest")
      if (@test_runner.command == nil)
        puts "You should run #{mrbtest} on target device."
        puts
      else
        @test_runner.run(mrbtest)
      end
    end

    def run_bintest
      puts ">>> Bintest #{name} <<<"
      targets = @gems.select { |v| File.directory? "#{v.dir}/bintest" }.map { |v| filename v.dir }
      mrbc = @gems["mruby-bin-mrbc"] ? exefile("#{@build_dir}/bin/mrbc") : mrbcfile

      env = {
        "BUILD_DIR" => @build_dir,
        "MRBCFILE" => mrbc,
        "EMULATOR" => @test_runner.emulator,
        "EXECUTABLE_EXT" => @exts.executable,
      }
      bintest = File.join(MRUBY_ROOT, "test/bintest.rb")
      sh env, "ruby #{bintest}#{verbose_flag} #{targets.join ' '}"
    end

    protected

    def create_mrbc_build; end

    private

    def generate_mrbc_build(needed)
      name = mrbc_build_name(needed)
      if MRuby.targets[name]
        fail "cannot generate the `mrbc' build for '#{@name}': " \
             "the build config already declares a build named '#{name}'"
      end
      MRuby::Build.new(name, internal: true) do |conf|
        conf.toolchain
        conf.build_mrbc_exec
        conf.disable_libmruby
        conf.compilers.each {|c| c.defines.concat(needed)}
      end
      name
    end

    # The answer `build` gives to every question in `MRBC_DEFINES`, as the
    # defines it says yes to.
    #
    # Both lists a build config writes answer, the way `Build#has_define?`
    # reads them, because `Command::Compiler#all_flags` puts `build.defines`
    # on the same command line as a compiler's own. `Build#has_define?` itself
    # cannot be asked here: it refuses until the gems are set up, and every
    # `mrbc` is bound before that.
    def mrbc_defines(build)
      own = build.defines.flatten.map {|d| d.to_s.split('=', 2).first}
      MRBC_DEFINES.select do |d|
        own.include?(d) || build.compilers.any? {|c| c.has_define?(d)}
      end
    end

    # Name a generated build after the defines it carries, so that the name
    # says which targets can borrow it. A build that carries none is the one a
    # plain `host` would have been.
    def mrbc_build_name(defines)
      answer = defines.empty? ? 'default' :
               defines.map {|d| d.delete_prefix('MRB_').downcase.tr('_', '-')}.join('+')
      "mrbc/#{answer}"
    end
  end # CrossBuild
end # MRuby
