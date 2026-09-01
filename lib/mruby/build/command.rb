require 'forwardable'
require 'tmpdir'

module MRuby
  class Command
    include Rake::DSL
    extend Forwardable
    def_delegators :@build, :filename, :objfile, :libfile, :exefile
    attr_accessor :build, :command

    def initialize(build)
      @build = build
    end

    # clone is deep clone without @build
    def clone
      target = super
      excepts = %w(@build)
      instance_variables.each do |attr|
        unless excepts.include?(attr.to_s)
          val = Marshal::load(Marshal.dump(instance_variable_get(attr))) # deep clone
          target.instance_variable_set(attr, val)
        end
      end
      target
    end

    def shellquote(s)
      "\"#{s}\""
    end

    private
    # Run the command, from +chdir+ where one is given.
    #
    # A command handed the names its files have from the tree is run from the
    # tree, and is given the directory rather than the build being left in it:
    # `rake -m` runs the tasks of a build in threads of one process, which
    # have one working directory between them, while a directory given here
    # belongs to the one child it is given to.
    def _run(options, params={}, chdir: nil)
      cmd = "#{build.filename(command)} #{options % params}"
      chdir ? sh(cmd, chdir: chdir) : sh(cmd)
    end
  end

  class Command::Compiler < Command
    # The answers `option_support?` has had from the commands it asked, which
    # are the same from one compiler to the next.
    OPTION_SUPPORT = {}

    # The answers `try_compile` has had, keyed by the command line and the
    # source it asked with.
    COMPILE_PROBES = {}

    attr_accessor :label, :flags, :include_paths, :defines, :source_exts
    # Defines are held in two lists, split by who asked for them. `defines` is
    # what the build config and the gems write. `internal_defines` is what the
    # build adds on its own behalf, from its own switches (`enable_debug`,
    # `enable_cxx_abi`) or from a toolchain. Both reach the compiler as `-D`;
    # keeping them apart lets a toolchain set its own without overwriting what
    # the config already wrote, and lets a caller ask for one list alone.
    attr_accessor :internal_defines
    attr_accessor :compile_options, :option_define, :option_include_path, :out_ext
    # The directories the compiler writes another name for, as `from => to`,
    # and the option that spells that to it. They are kept out of `flags`
    # because they name directories of the machine the build runs on, which
    # the flags a package exports are asked not to carry.
    attr_accessor :file_prefix_maps, :option_file_prefix_map
    # The option that tells the compiler which directory to record as the one
    # it compiled in, for a compiler that takes such a thing.
    attr_accessor :option_compilation_dir
    attr_accessor :cxx_compile_flag, :cxx_exception_flag, :cxx_invalid_flags
    attr_writer :preprocess_options

    def initialize(build, source_exts=[], label: "CC")
      super(build)
      @command = ENV['CC'] || 'cc'
      @label = label
      @flags = [ENV['CFLAGS'] || []]
      @source_exts = source_exts
      @include_paths = ["#{MRUBY_ROOT}/include"]
      @defines = []
      @internal_defines = []
      @option_include_path = %q[-I"%s"]
      @option_define = %q[-D"%s"]
      @file_prefix_maps = {}
      @option_file_prefix_map = %q[-ffile-prefix-map="%s=%s"]
      @option_compilation_dir = %q[-ffile-compilation-dir="%s"]
      @compile_options = %q[%{flags} -o "%{outfile}" -c "%{infile}"]
      @cxx_invalid_flags = []
      @out_ext = build.exts.object
    end

    alias header_search_paths include_paths

    def preprocess_options
      @preprocess_options ||= @compile_options.sub(/(?:\A|\s)\K-c(?=\s)/, "-E -P")
    end

    # True when this compiler compiles with -D<name>, whichever of the two
    # lists it sits in. A gem asks this while its own mrbgem.rake body runs,
    # where `Build#has_define?` refuses to answer because the gems that come
    # after it have not contributed their defines yet.
    def has_define?(name)
      name = name.to_s
      # A define may carry a value, as `FOO=1` does, so compare the name and
      # not the value.
      [defines, internal_defines].flatten
        .any? {|d| d.to_s.split('=', 2).first == name}
    end

    # The flags that keep the directories of this machine out of what the
    # compiler writes: the names in `file_prefix_maps` in place of the
    # directories they stand for, wherever the compiler writes a path of its
    # own, in `__FILE__` and in the debug information. A compiler that can
    # neither map a path nor be told which directory it compiled in is left
    # alone, and the paths it writes stand as they are.
    #
    # A build that compiles by the names its sources have from the tree hands
    # those names to the map too, so a directory of the machine is named here
    # only where the build could not name it any other way. Two of them then
    # answer for themselves: the tree, which every name is already written
    # against, and a build directory that is already spelled the way the map
    # would write it.
    #
    # What no name of a source can carry is the directory the compiler records
    # as the one it compiled in, which it takes from the machine whatever it
    # is handed. A compiler that spells that directory on its own is asked for
    # it, since that spelling names no path of this machine; the map is what
    # answers for a compiler that does not.
    def file_prefix_map_flags
      relative = build.compile_relative?
      flags = []
      file_prefix_maps.each do |from, to|
        if relative && from == MRUBY_ROOT
          if compilation_dir?
            flags << option_compilation_dir % to
            next
          end
        elsif relative
          from = build.compile_path(from)
          next if from == to
        end
        flags << option_file_prefix_map % [filename(from), to] if file_prefix_map?
      end
      flags
    end

    # Whether this compiler can map a path, which is two questions: whether
    # the toolchain spells such an option at all, and whether the command it
    # names knows the one it spells. The second is asked of the command,
    # since a toolchain stands for a family of compilers and the flag is not
    # in the older ones: `-ffile-prefix-map` arrived in GCC 8 and in clang
    # 10. A command that answers no is compiled with as it always was, so a
    # build that used to work is not broken by a map it never asked for.
    def file_prefix_map?
      return false unless option_file_prefix_map
      option_support?(option_file_prefix_map % ['/mruby', '.'])
    end

    # Whether this compiler is told the directory it compiled in, rather than
    # left to record the one it is run from. The option names no path of this
    # machine, so a build that compiles by the names of the tree carries the
    # same directory wherever the tree sits, and a cache keyed on the command
    # line answers for one tree what it learned from another.
    #
    # Only `clang` spells it, and only from the version the spelling arrived
    # in; `gcc` has nothing of the kind, and a build compiled by it says which
    # directory it compiled in through the map instead.
    def compilation_dir?
      return false unless option_compilation_dir
      option_support?(option_compilation_dir % '.')
    end

    # Whether this compiler compiles +source+, asked with the flags it will
    # compile the build with.  This is how a build settles a question about
    # the target that the preprocessor cannot answer on its own: whether a
    # header is there to be included is one such question, since finding out
    # means reading the header, and a `#if` that guards the `#include` runs
    # too late to help.
    #
    # The compiler is asked rather than guessed at from a table of platforms.
    # It is the one that knows what its target has, and it is asked with the
    # flags it will compile with, so a cross build answers about the target's
    # headers and not the host's: the `--sysroot` that says which is which is
    # among those flags.
    #
    # The source is compiled and never linked, so a target with no library to
    # link against still answers.  Both it and the object go in a directory of
    # their own, which is removed afterwards; the compiler still runs where
    # the build runs, so that a relative path among the flags, an `-I` a
    # configuration wrote without spelling out a root, names what it names
    # during a compile.
    #
    # The answer is kept for the life of the rake process, since a build has
    # four compilers and a config has several builds, and they name few
    # commands between them.  It is keyed by everything that goes into the
    # compile, the extension included: that is what tells a compiler whether
    # it is reading C or C++, and it can be all that separates two compilers,
    # a toolchain being free to give them one command and one set of flags.
    # `visualcpp` does exactly that, naming `cl` for both and handing the C++
    # one the C flags when `CFLAGS` is set and `CXXFLAGS` is not.
    def try_compile(source)
      key = [build.filename(command), compile_options, source_exts.first,
             all_flags, source]
      COMPILE_PROBES.fetch(key) { COMPILE_PROBES[key] = run_compile_probe(source) }
    end

    # Whether the header +name+ is there, as `#include <name>` asks for it.
    #
    # Unlike `search_header_path`, which looks through the paths this build
    # adds with `-I` and answers about those alone, this asks the compiler,
    # so it answers about the headers the compiler brings itself: those of
    # the target's C library, where a header base POSIX does not guarantee
    # (`<sys/resource.h>`, an XSI extension) either is or is not.
    def check_header(name)
      # A translation unit needs a declaration in it, and a header alone may
      # leave one empty, so one is written that costs nothing.
      try_compile("#include <#{name}>\nextern int mrb_probe;\n")
    end

    # Whether +name+ is declared once +header+ is included, or is a macro
    # spelled that way.  A macro answers yes because a call written as
    # `name(...)` compiles either way, which is what a caller is asking.
    #
    # The answer is about the declaration a compile can see, not about a
    # symbol a link would resolve: this never links.  A host whose headers
    # declare what its library does not define is not told apart here.
    def check_func(name, header: nil)
      source = header ? "#include <#{header}>\n" : ""
      # The name is mentioned and never called, so an undeclared one is the
      # error that answers no while a declared one asks nothing of a library.
      # A macro is caught before that, since a macro is not an identifier the
      # compiler would have anything to say about.
      #
      # A C++ compile is handed a different mention.  A C header read as C++
      # may declare a name more than once, `pow` and `strchr` among them, and
      # a cast to `void` cannot name a set: picking from one takes a target
      # type, which that cast does not give, so the mention that answers for
      # every other name would answer no for those.  A using-declaration asks
      # for no target and so names them all.
      source += <<~SOURCE
        int mrb_probe(void);
        int mrb_probe(void) {
        #ifdef #{name}
          return 0;
        #elif defined(__cplusplus)
          using ::#{name};
          return 0;
        #else
          (void)#{name};
          return 0;
        #endif
        }
      SOURCE
      try_compile(source)
    end

    def search_header_path(name)
      header_search_paths.find do |v|
        File.exist? build.filename("#{v}/#{name}").sub(/^"(.*)"$/, '\1')
      end
    end

    def search_header(name)
      path = search_header_path name
      path && build.filename("#{path}/#{name}").sub(/^"(.*)"$/, '\1')
    end

    # The flags a caller compiles with, or that a package carries away.
    #
    # +compiled+ says which of the two is asking. A compile is run from the
    # tree and names the directories it reads by the names they have from it,
    # where they have one; a package is compiled from somewhere else entirely,
    # and its flags name every directory in full, for whoever rewrites them
    # into the directories the package was installed in.
    def all_flags(_defines=[], _include_paths=[], _flags=[], compiled: false)
      define_flags = [defines, internal_defines, _defines, build.defines].flatten
                       .map{ |d| option_define % d }
      include_path_flags = [include_paths, _include_paths].flatten.map do |f|
        f = build.compile_path(f) if compiled
        option_include_path % filename(f)
      end
      [flags, file_prefix_map_flags, define_flags, include_path_flags, _flags].flatten.join(' ')
    end

    def run(outfile, infile, _defines=[], _include_paths=[], _flags=[])
      mkdir_p File.dirname(outfile)
      flags = compile_flags(outfile, _defines, _include_paths, _flags)
      if object_ext?(outfile)
        label = @label
        opts = compile_options
      else
        label = "CPP"
        opts = preprocess_options
      end
      _pp label, infile.relative_path, outfile.relative_path
      _run opts, {
        flags: flags,
        infile: filename(build.compile_path(infile)),
        outfile: filename(build.compile_path(outfile)),
      }, chdir: (MRUBY_ROOT if build.compile_relative?)
      # Recorded after the compile, so that a compile that failed leaves
      # nothing claiming a configuration its output was not built with.
      File.write(flags_file(outfile), flags_record(opts, flags))
    end

    # Define the rules that build the outputs under +build_dir+ from the
    # sources under +source_dir+, and from the generated sources that sit
    # beside the outputs.
    #
    # The rules compile with this compiler, or with the one the block returns
    # when a block is given. The block is called when a rule is resolved, not
    # here, so a compiler it derives from this one sees everything the build
    # adds after the rules are defined: a gem's mrbgem.rake body runs before
    # the gem's version define, the include paths of the gems it depends on
    # and the presym include path reach the gem's compilers.
    def define_rules(build_dir, source_dir='', out_ext=build.exts.object, &compiler_of)
      compiler_of ||= proc { self }
      gemrake = File.join(source_dir, "mrbgem.rake")
      rakedep = File.exist?(gemrake) ? [ gemrake ] : []

      bd = build_dir
      if bd.start_with?(MRUBY_ROOT)
        bd = bd.sub(MRUBY_ROOT, '')
      end
      if bd.include? "mrbgems/"
        generated_file_matcher = Regexp.new("^#{Regexp.escape build_dir}/(?!mrbc/)(.*)#{Regexp.escape out_ext}$")
      else
        generated_file_matcher = Regexp.new("^#{Regexp.escape build_dir}/(?!mrbc/|mrbgems/.+/)(.*)#{Regexp.escape out_ext}$")
      end
      source_exts.each do |ext|
        # The source is looked for beside the sources first and among the
        # generated files second.
        [source_dir, build_dir].each do |dir|
          source_of = proc { |file| file.sub(generated_file_matcher, "#{dir}/\\1#{ext}") }
          rule generated_file_matcher => [
            source_of,
            proc { |file| compiler_of.call.get_dependencies(file, source_of.call(file)) + rakedep }
          ] do |t|
            compiler_of.call.run t.name, t.prerequisites.first
          end
        end
      end
    end

    # This method can be redefined as a singleton method where appropriate.
    # Manipulate `flags`, `include_paths` and/or more if necessary.
    def setup_debug(conf)
      nil
    end

    protected

    # The prerequisites of an output besides its source: the config file and
    # the headers the last compile of it read. Protected, not private, so the
    # rules one compiler defines can ask the compiler they run.
    #
    # === Example of +.d+ file
    #
    # ==== Without `-MP` compiler flag
    #
    #   /build/host/src/array.o: /src/array.c \
    #     /include/mruby/common.h /include/mruby/value.h \
    #     /src/value_array.h
    #
    # ==== With `-MP` compiler flag
    #
    #   /build/host/src/array.o: /src/array.c \
    #     /include/mruby/common.h /include/mruby/value.h \
    #     /src/value_array.h
    #
    #   /include/mruby/common.h:
    #
    #   /include/mruby/value.h:
    #
    #   /src/value_array.h:
    #
    # The compile of the object writes the +.d+ file, and the presym
    # preprocess of the same source reads it too: both run the preprocessor
    # over the same source with the same include paths, so a header that
    # changes what one sees changes what the other sees. Only the presym
    # headers are left out of the preprocess: they are made from the
    # preprocessed files, so depending on them would preprocess every source
    # again on the run after the symbol table changed. The scan does not
    # include them anyway (see +MRB_PRESYM_SCANNING+ in +mruby/presym.h+).
    #
    def get_dependencies(file, source)
      discard_foreign_output(file) if rule_applies?(source)
      deps = [MRUBY_CONFIG]
      dep_file = file.ext(".d")
      return deps unless File.exist?(dep_file)

      header_deps = File.read(dep_file).gsub("\\\n ", "").split("\n").map do |dep_line|
        # dep_line:
        # - "/build/host/src/array.o:   /src/array.c   /include/mruby/common.h ..."
        # - ""
        # - "/include/mruby/common.h:"
        dep_line.scan(/^\S+:\s+(.+)$/).flatten.map { |s| s.split(' ') }.flatten
        # => ["/src/array.c", "/include/mruby/common.h" , ...]
        #    []
        #    []
      end.flatten.uniq
      # The names a +.d+ file carries are the ones the compile was given, so a
      # compile that names its sources against the tree leaves relative names
      # here. They are read against the directory the compile ran in, since
      # everything below compares them with the names of the tasks, and those
      # are absolute.
      header_deps.map! do |dep|
        Pathname.new(dep).absolute? ? dep : File.join(MRUBY_ROOT, dep)
      end
      unless object_ext?(file)
        presym_dir = "#{build.presym.header_dir}/"
        header_deps.reject! {|dep| dep.start_with?(presym_dir) }
      end
      # A header the +.d+ file names but that no longer exists is not a
      # dependency Rake can resolve: `Rake::TaskManager#attempt_rule` gives up
      # on the rule when a source neither exists nor has a task, and the
      # output is left as it is, with nothing to rebuild it (the object keeps
      # only the presym proxy from `tasks/presym.rake`, the preprocess drops
      # out of the scan). The +.d+ describes a compile that read that header,
      # so the output is stale by its own record: it is removed here, so that
      # the rule, with the header left out, builds it again and writes a
      # +.d+ that matches the sources of now.
      missing = header_deps.reject {|dep| File.exist?(dep) || Rake::Task.task_defined?(dep) }
      unless missing.empty?
        header_deps -= missing
        rm_f file if rule_applies?(source)
      end
      deps.concat(header_deps)
    end

    private

    # Whether the command this compiler names knows +option+, which is asked
    # of the command itself: a toolchain stands for a family of compilers, and
    # an option one of them takes is not in all of them.
    #
    # The answer is kept per command line, since a build has four compilers
    # and a config has several builds, and they name few commands between
    # them.
    def option_support?(option)
      probe = "#{build.filename(command)} #{option}"
      OPTION_SUPPORT.fetch(probe) do
        `echo | #{probe} -E - 2>&1`
        OPTION_SUPPORT[probe] = $?.exitstatus == 0
      end
    end

    # Compile +source+ and answer whether it compiled.  The source and the
    # object are named inside a directory that is removed on the way out; the
    # compiler is run from the directory a compile runs from, which is what a
    # relative path in the flags is written against.
    def run_compile_probe(source)
      Dir.mktmpdir("mruby-probe") do |dir|
        infile = "#{dir}/probe#{source_exts.first || '.c'}"
        outfile = "#{dir}/probe#{out_ext}"
        File.write(infile, source)
        options = compile_options % {
          flags: all_flags, infile: filename(infile), outfile: filename(outfile)
        }
        opts = {out: File::NULL, err: File::NULL}
        opts[:chdir] = MRUBY_ROOT if build.compile_relative?
        # `system` answers nil where the command is not there to run at all,
        # which is an answer of no like any other failure to compile.
        !!system("#{build.filename(command)} #{options}", **opts)
      end
    end

    #
    # === Example of +.flags+ file
    #
    #   command: gcc
    #   options: -MMD -c %{flags} -o "%{outfile}" "%{infile}"
    #   flags: -std=gnu99 -g -O3 -Wall -DMRB_NO_FLOAT -I"/mruby/include"
    #
    def flags_record(options, flags)
      "command: #{build.filename(command)}\noptions: #{options}\nflags: #{flags}\n"
    end

    # The record sits beside the output under the output's own name, so that
    # the object and the presym preprocess of one source keep a record each.
    def flags_file(outfile)
      "#{outfile}.flags"
    end

    # Whether the rule this source belongs to is the one that builds the
    # output. Rake asks the same of a rule before it applies it, and it asks
    # every rule that matches the name: the compilers of a build define a rule
    # each for the same output, differing in the extension of the source they
    # look for. Only the one that finds its source speaks for the output; the
    # others would compare it against flags no compile of it ever used.
    #
    # Rake accepts one source more than this, one another rule can produce:
    # `Rake::TaskManager#attempt_rule` falls back to
    # `enhance_with_matching_rule` for a source that is neither a file nor a
    # task. The rules here are the only ones in the tree and they match object
    # names, so nothing answers that fallback; a generated source is a `file`
    # task, which `Rake::Task.task_defined?` already finds.
    def rule_applies?(source)
      File.exist?(source) || Rake::Task.task_defined?(source)
    end

    # Remove an output that the command line beside it does not answer for,
    # so that the rule that would have found it up to date builds it again.
    #
    # Nothing else in the build tells the two apart. A +.d+ file lists header
    # dependencies only, and the config file is a dependency by path, so its
    # mtime does not move when another config takes over the same build
    # directory. Left uncompared, the output stays up to date against every
    # dependency it has, and the build silently keeps the flags of whichever
    # config wrote it first, its defines above all.
    #
    # An output with no record at all counts as foreign too, since what
    # produced it is unknown.
    def discard_foreign_output(file)
      return unless File.exist?(file)
      path = flags_file(file)
      options = object_ext?(file) ? compile_options : preprocess_options
      record = flags_record(options, compile_flags(file))
      recorded = File.read(path) if File.exist?(path)
      return if recorded == record
      build.report_flags_change(recorded, record)
      rm_f file
    end

    # The flags a compile of `outfile` runs with. The preprocess that feeds
    # the presym scan is this compiler with one define more, so the define
    # belongs to the flags and to what is recorded of them.
    def compile_flags(outfile, _defines=[], _include_paths=[], _flags=[])
      flags = all_flags(_defines, _include_paths, _flags, compiled: true)
      flags += " -DMRB_PRESYM_SCANNING" unless object_ext?(outfile)
      flags
    end

    def object_ext?(path)
      File.extname(path) == build.exts.object
    end
  end

  class Command::Linker < Command
    attr_accessor :flags, :library_paths, :flags_before_libraries, :libraries, :flags_after_libraries
    attr_accessor :link_options, :option_library, :option_library_path

    def initialize(build)
      super
      @command = ENV['LD'] || 'ld'
      @flags = (ENV['LDFLAGS'] || [])
      @flags_before_libraries, @flags_after_libraries = [], []
      @libraries = []
      @library_paths = []
      @option_library = %q[-l"%s"]
      @option_library_path = %q[-L"%s"]
      @link_options = %Q[%{flags} -o "%{outfile}" %{objs} %{flags_before_libraries} %{libs} %{flags_after_libraries}]
    end

    def all_flags(_library_paths=[], _flags=[])
      library_path_flags = [library_paths, _library_paths].flatten.map do |f|
        option_library_path % filename(f)
      end
      [flags, library_path_flags, _flags].flatten.join(' ')
    end

    def library_flags(_libraries)
      [libraries, _libraries].flatten.map{ |d| option_library % d }.join(' ')
    end

    def run_attrs
      [@libraries, @library_paths, @flags, @flags_before_libraries, @flags_after_libraries]
    end

    def run(outfile, objfiles, _libraries=[], _library_paths=[], _flags=[], _flags_before_libraries=[], _flags_after_libraries=[])
      mkdir_p File.dirname(outfile)
      library_flags = [libraries, _libraries].flatten.map { |d| option_library % d }

      _pp "LD", outfile.relative_path
      _run link_options, { :flags => all_flags(_library_paths, _flags),
                            :outfile => filename(outfile) , :objs => filename(objfiles).map{|f| %Q["#{f}"]}.join(' '),
                            :flags_before_libraries => [flags_before_libraries, _flags_before_libraries].flatten.join(' '),
                            :flags_after_libraries => [flags_after_libraries, _flags_after_libraries].flatten.join(' '),
                            :libs => library_flags.join(' ') }
    end
  end

  class Command::Archiver < Command
    attr_accessor :archive_options

    def initialize(build)
      super
      @command = ENV['AR'] || 'ar'
      @archive_options = 'rcs%{deterministic} "%{outfile}" %{objs}'
    end

    # The archive is written from the objects of now. `ar r` adds to an
    # archive that exists and never takes a member out, so an object whose
    # source was renamed or removed stays in it, ahead of the one that
    # replaced it, and the linker resolves a symbol both define through the
    # member it meets first: the old one. Only a build directory that starts
    # empty avoids that, so the archive that exists is removed here first.
    def run(outfile, objfiles)
      mkdir_p File.dirname(outfile)
      rm_f outfile
      _pp "AR", outfile.relative_path
      params = { :outfile => filename(outfile), :objs => filename(objfiles).map{|f| %Q["#{f}"]}.join(' ') }
      params[:deterministic] = deterministic_modifier(File.dirname(outfile)) if
        archive_options.include?("%{deterministic}")
      _run archive_options, params
    end

    private

    # `ar` copies the mtime, uid, gid and mode of every object into the member
    # header it writes, so the same objects archived by another user, or a
    # second later, make a different archive. The `D` modifier of GNU ar and
    # llvm-ar writes zeros in those fields instead. Most binutils are built to
    # do that unasked, but that is the choice of whoever built binutils and not
    # one to rest on, and the `ar` of cctools takes no `D` at all. So ask this
    # `ar` whether it takes one, once per build, with its complaint about an
    # unknown modifier kept off the screen.
    def deterministic_modifier(dir)
      return @deterministic_modifier if defined?(@deterministic_modifier)
      probe = "#{dir}/.ar-deterministic-probe"
      rm_f probe
      accepted = system(%Q[#{build.filename(command)} rcsD "#{filename(probe)}" > #{File::NULL} 2>&1])
      rm_f probe
      @deterministic_modifier = accepted ? "D" : ""
    end

    # The `ar` that takes no `D`, cctools', zeroes the timestamps when this is
    # set instead. GNU ar and llvm-ar pay it no attention.
    def _run(options, params={})
      sh({"ZERO_AR_DATE" => "1"}, "#{build.filename(command)} #{options % params}")
    end
  end

  class Command::Yacc < Command
    attr_accessor :compile_options

    def initialize(build)
      super
      @command = ENV['YACC'] || 'lrama'
      @compile_options = %q[-o "%{outfile}" "%{infile}"]
    end

    def run(outfile, infile)
      mkdir_p File.dirname(outfile)
      _pp "YACC", infile.relative_path, outfile.relative_path
      _run compile_options, { :outfile => filename(outfile) , :infile => filename(infile) }
    end
  end

  class Command::Gperf < Command
    attr_accessor :compile_options

    def initialize(build)
      super
      @command = 'gperf'
      @compile_options = %q[-L ANSI-C -C -j1 -i 1 -o -t -N mrb_reserved_word -k"1,3,$" "%{infile}" > "%{outfile}"]
    end

    def run(outfile, infile)
      mkdir_p File.dirname(outfile)
      _pp "GPERF", infile.relative_path, outfile.relative_path
      _run compile_options, { :outfile => filename(outfile) , :infile => filename(infile) }
    end
  end

  class Command::Git < Command
    attr_accessor :flags
    attr_accessor :clone_options, :pull_options, :checkout_options, :checkout_detach_options, :reset_options

    def initialize(build)
      super
      @command = 'git'
      @flags = []
      @clone_options = "clone %{flags} %{url} %{dir}"
      @pull_options = "--git-dir %{repo_dir}/.git --work-tree %{repo_dir} pull"
      @checkout_options = "--git-dir %{repo_dir}/.git --work-tree %{repo_dir} checkout %{checksum_hash}"
      @checkout_detach_options = "--git-dir %{repo_dir}/.git --work-tree %{repo_dir} checkout --detach %{checksum_hash}"
      @reset_options = "--git-dir %{repo_dir}/.git --work-tree %{repo_dir} reset %{checksum_hash}"
    end

    def run_clone(dir, url, _flags = [])
      _pp "GIT", url, dir.relative_path
      _run clone_options, { :flags => [flags, _flags].flatten.join(' '), :url => shellquote(url), :dir => shellquote(filename(dir)) }
    end

    def run_pull(dir, url)
      _pp "GIT PULL", url, dir.relative_path
      _run pull_options, { :repo_dir => shellquote(dir) }
    end

    def run_checkout(dir, checksum_hash)
      _pp "GIT CHECKOUT", dir, checksum_hash
      _run checkout_options, { :checksum_hash => checksum_hash, :repo_dir => shellquote(dir) }
    end

    def run_checkout_detach(dir, checksum_hash)
      _pp "GIT CHECKOUT DETACH", dir, checksum_hash
      _run checkout_detach_options, { :checksum_hash => checksum_hash, :repo_dir => shellquote(dir) }
    end

    def run_reset_hard(dir, checksum_hash)
      _pp "GIT RESET", dir, checksum_hash
      _run reset_options, { :checksum_hash => checksum_hash, :repo_dir => shellquote(dir) }
    end

    def commit_hash(dir)
      `#{@command} --git-dir #{shellquote(dir + '/.git')} --work-tree #{shellquote(dir)} rev-parse --verify HEAD`.strip
    end

    def current_branch(dir)
      `#{@command} --git-dir #{shellquote(dir + '/.git')} --work-tree #{shellquote(dir)} rev-parse --abbrev-ref HEAD`.strip
    end
  end

  class Command::Mrbc < Command
    attr_accessor :compile_options

    def initialize(build)
      super
      @command = nil
      @compile_options = "-B%{funcname} -o-"
    end

    def run(out, infiles, funcname, cdump: true, static: false)
      @command ||= @build.mrbcfile
      infiles = [infiles].flatten
      infiles.each_with_index do |f, i|
        _pp i == 0 ? "MRBC" : "", f.relative_path, indent: 2
      end
      opt = @compile_options % {funcname: funcname}
      opt << " -S" if cdump
      opt << " -s" if static
      # Have mrbc write to a private tempfile (-o) instead of stdout (-o-)
      # to avoid pipe-inheritance races with parallel rake on Windows MinGW,
      # where unrelated _pp build-progress lines from sibling workers can
      # leak into the captured stdout and corrupt the generated C file.
      tmpout = "#{out.path}.#{funcname}.mrbcout"
      opt = opt.sub(/\s-o-(?=\s|\z)/, %Q[ -o "#{filename tmpout}"])
      cmd = %["#{filename @command}" #{opt} #{filename(infiles).map{|f| %["#{f}"]}.join(' ')}]
      puts cmd if Rake.verbose
      unless system(cmd)
        rm_f tmpout
        rm_f out.path
        fail "Command failed with status (#{$?.exitstatus}): [#{cmd[0,42]}...]"
      end
      out.write File.binread(tmpout)
      rm_f tmpout
    end
  end

  class Command::CrossTestRunner < Command
    attr_accessor :runner_options
    attr_accessor :verbose_flag
    attr_accessor :flags

    def initialize(build)
      super
      @command = nil
      @runner_options = '%{flags} %{infile}'
      @verbose_flag = ''
      @flags = []
    end

    def emulator
      return "" unless @command
      return [@command, *@flags].map{|c| shellquote(c)}.join(' ')
    end

    def run(testbinfile)
      puts "TEST for " + @build.name
      _run runner_options, { :flags => [flags, verbose_flag].flatten.join(' '), :infile => testbinfile }
    end
  end

end
