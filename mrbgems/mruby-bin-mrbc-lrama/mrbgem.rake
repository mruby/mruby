MRuby::Gem::Specification.new 'mruby-bin-mrbc-lrama' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby compiler executable (legacy lrama-based)'
  spec.add_dependency 'mruby-compiler-lrama', :core => 'mruby-compiler-lrama'
  spec.add_conflict 'mruby-bin-mrbc'

  exec = exefile("#{build.build_dir}/bin/mrbc")
  mrbc_objs = Dir.glob("#{spec.dir}/tools/mrbc/*.c").map { |f| objfile(f.pathmap("#{spec.build_dir}/tools/mrbc/%n")) }

  file exec => mrbc_objs << build.libmruby_core_static do |t|
    build.linker.run t.name, t.prerequisites
  end

  build.bins << 'mrbc'
end
