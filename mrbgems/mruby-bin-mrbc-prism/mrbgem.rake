MRuby::Gem::Specification.new 'mruby-bin-mrbc-prism' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby & PicoRuby developers'
  spec.summary = 'mruby compiler executable using Prism'

  spec.add_dependency 'mruby-compiler-prism', core: 'mruby-compiler-prism'
  spec.add_conflict 'mruby-compiler'
  spec.add_conflict 'mruby-bin-mrbc'

  spec.cc.include_paths << "#{MRUBY_ROOT}/mrbgems/mruby-compiler-prism/lib/prism/include"

  mrbc_prism_objs = Dir.glob("#{dir}/tools/mrbc/*.c").map { |f| objfile(f.pathmap("#{build_dir}/tools/mrbc/%n")) }
  mrbc_prism_objs += build.gems['mruby-compiler-prism'].objs
  mrbc_prism_objs.delete_if { |o| o.include?('gem_init') || o.include?('mruby_compat') }

  exe_name = 'mrbc-prism'

  file exefile("#{build.build_dir}/bin/#{exe_name}") => mrbc_prism_objs do |t|
    build.linker.run t.name, t.prerequisites
  end

  build.bins << exe_name

end
