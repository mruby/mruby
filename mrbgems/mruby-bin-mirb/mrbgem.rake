# Environment Variable Configuration:
#
# MRUBY_MIRB_READLINE - Control which readline library mirb uses:
#   auto (default) - Auto-detect: try readline, then edit, then linenoise
#   readline, gnu  - Force GNU readline only
#   edit, libedit  - Force libedit only
#   linenoise      - Force linenoise only
#   none, off, false, disabled - Use plain input mode (no readline)
#
# Example:
#   MRUBY_MIRB_READLINE=none rake
#   MRUBY_MIRB_READLINE=linenoise rake

MRuby::Gem::Specification.new('mruby-bin-mirb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command'

  # Allow user to override readline detection via environment variable
  readline_mode = (ENV['MRUBY_MIRB_READLINE'] || 'auto').downcase

  case readline_mode
  when 'auto'
    # Auto-detect: try readline, then edit, then linenoise (default behavior)
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

  when 'readline', 'gnu'
    # Force GNU readline only
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
    end

  when 'edit', 'libedit'
    # Force libedit only
    if spec.build.cc.search_header_path 'edit/readline/readline.h'
      spec.cc.defines << "MRB_USE_READLINE"
      spec.cc.defines << "MRB_READLINE_HEADER='<edit/readline/readline.h>'"
      spec.cc.defines << "MRB_READLINE_HISTORY='<edit/readline/history.h>'"
      spec.linker.libraries << "edit"
    end

  when 'linenoise'
    # Force linenoise only
    if spec.build.cc.search_header_path 'linenoise.h'
      spec.cc.defines << "MRB_USE_LINENOISE"
    end

  when 'none', 'off', 'false', 'disabled'
    # Disable readline - use plain input mode

  else
    fail "Invalid MRUBY_MIRB_READLINE='#{readline_mode}'. " \
         "Valid values: auto, readline, gnu, edit, libedit, linenoise, none, off, false, disabled"
  end

  spec.bins = %w(mirb)
  spec.add_dependency('mruby-compiler', :core => 'mruby-compiler')
end
