MRuby::Gem::Specification.new('mruby-pack') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
 
  # spec.cflags = ''
 
  # spec.mruby_cflags = ''
  # spec.mruby_ldflags = ''
  # spec.mruby_libs = ''
  spec.mruby_includes = ["#{build.root}/src"]
 
  # spec.rbfiles = Dir.glob("#{dir}/mrblib/*.rb")
  # spec.objs = Dir.glob("#{dir}/src/*.{c,cpp,m,asm,S}").map { |f| f.relative_path_from(dir).pathmap("#{build_dir}/%X.o") }
  # spec.test_rbfiles = Dir.glob("#{dir}/test/*.rb")
  # spec.test_objs = Dir.glob("#{dir}/test/*.{c,cpp,m,asm,S}").map { |f| f.relative_path_from(dir).pathmap("#{build_dir}/%X.o") }
  # spec.test_preload = 'test/assert.rb'
end
