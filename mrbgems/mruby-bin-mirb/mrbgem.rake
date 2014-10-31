MRuby::Gem::Specification.new('mruby-bin-mirb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command'

  if spec.build.cc.search_header_path 'readline/readline.h'
    spec.cc.defines << "ENABLE_READLINE"
    if spec.build.cc.search_header_path 'termcap.h'
      if MRUBY_BUILD_HOST_IS_CYGWIN then
        spec.linker.libraries << 'ncurses'
      else
        spec.linker.libraries << 'termcap'
      end
    end
    spec.linker.libraries << 'readline'
  elsif spec.build.cc.search_header_path 'linenoise.h'
    spec.cc.defines << "ENABLE_LINENOISE"
  end

  spec.bins = %w(mirb)
end
