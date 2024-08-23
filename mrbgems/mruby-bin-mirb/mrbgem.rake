MRuby::Gem::Specification.new('mruby-bin-mirb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command'

  if spec.build.cc.search_header_path 'readline/readline.h'
    spec.cc.defines << "MRB_USE_READLINE"
    spec.cc.defines << "MRB_READLINE_HEADER='<readline/readline.h>'"
    spec.cc.defines << "MRB_READLINE_HISTORY='<readline/history.h>'"
    if spec.build.cc.search_header_path 'termcap.h'
      if MRUBY_BUILD_HOST_IS_CYGWIN || MRUBY_BUILD_HOST_IS_OPENBSD
        if spec.build.cc.search_header_path 'termcap.h'
          if MRUBY_BUILD_HOST_IS_CYGWIN then
            spec.linker.libraries << 'ncurses'
          else
            spec.linker.libraries << 'termcap'
          end
        end
      end
    end
    if RUBY_PLATFORM.include?('netbsd')
      spec.linker.libraries << 'edit'
    else
      spec.linker.libraries << 'readline'
      if RUBY_PLATFORM.include?('darwin')
        # Workaround to build with Homebrew's readline on Mac (#4537)
        lib_path = spec.build.cc.header_search_paths.find do |include_path|
          lib_path = File.expand_path("#{include_path}/../lib")
          break lib_path if File.exist?("#{lib_path}/libreadline.dylib") ||
                            File.exist?("#{lib_path}/libreadline.a")
        end
        spec.linker.library_paths << lib_path if lib_path
      elsif spec.build.cc.search_header_path 'curses.h'
        spec.linker.libraries << 'ncurses'
        if spec.build.cc.search_header_path 'term.h'
          spec.linker.libraries << 'tinfo'
        end
      elsif spec.build.cc.search_header_path 'ncursesw/curses.h'
        spec.linker.libraries << 'ncursesw'
        if spec.build.cc.search_header_path 'ncursesw/term.h'
          spec.linker.libraries << 'tinfow'
        end
      end
    end
  elsif spec.build.cc.search_header_path 'edit/readline/readline.h'
    spec.cc.defines << "MRB_USE_READLINE"
    spec.cc.defines << "MRB_READLINE_HEADER='<edit/readline/readline.h>'"
    spec.cc.defines << "MRB_READLINE_HISTORY='<edit/readline/history.h>'"
    spec.linker.libraries << "edit"
  elsif spec.build.cc.search_header_path 'linenoise.h'
    spec.cc.defines << "MRB_USE_LINENOISE"
  end

  spec.bins = %w(mirb)
  spec.add_dependency('mruby-compiler', :core => 'mruby-compiler')
end
