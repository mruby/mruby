MRuby::Gem::Specification.new('mruby-dir') do |spec|
  spec.license = 'MIT'
  spec.authors = [ 'Internet Initiative Japan Inc.' ]

  spec.cc.include_paths << "#{build.root}/src"
  
  case RUBY_PLATFORM
  when /mingw|mswin/
    spec.license = 'MIT and MIT-like license'
    spec.authors += [ 'Kevlin Henney' ]
    spec.objs += Dir.glob("#{dir}/src/Win/*.{c,cpp,m,asm,S}").map { |f| 
      objfile(f.relative_path_from(dir).pathmap("#{build_dir}/%X")) 
    }
  end
end
