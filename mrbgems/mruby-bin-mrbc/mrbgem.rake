MRuby::Gem::Specification.new 'mruby-bin-mrbc' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby & PicoRuby developers'
  spec.summary = 'mruby compiler executable using Prism'

  spec.add_dependency 'mruby-compiler', core: 'mruby-compiler'
  spec.add_conflict 'mruby-compiler-lrama'
  spec.add_conflict 'mruby-bin-mrbc-lrama'

  spec.cc.include_paths << "#{MRUBY_ROOT}/mrbgems/mruby-compiler/lib/prism/include"

  # The layout of struct mrc_ccontext depends on MRC_TARGET_*; the tool's own
  # translation units must use the same target define as mruby-compiler,
  # otherwise field offsets (e.g. diagnostic_list) disagree across the link.
  spec.cc.defines.flatten!
  if spec.cc.defines.include?('PICORB_VM_MRUBY')
    spec.cc.defines << 'MRC_TARGET_MRUBY'
  elsif spec.cc.defines.include?('PICORB_VM_MRUBYC')
    spec.cc.defines << 'MRC_TARGET_MRUBYC'
  elsif !spec.cc.defines.include?('MRB_NO_GEMS')
    spec.cc.defines << 'MRC_TARGET_MRUBY'
  end

  mrbc_prism_objs = Dir.glob("#{dir}/tools/mrbc/*.c").map { |f| objfile(f.pathmap("#{build_dir}/tools/mrbc/%n")) }
  # Ensure the compiler gem is set up so its objs are available regardless of
  # the order in which gems are processed (setup is idempotent).
  compiler = build.gems['mruby-compiler']
  if compiler.objs.nil?
    saved = MRuby::Gem.current
    compiler.setup
    MRuby::Gem.current = saved
  end
  mrbc_prism_objs += compiler.objs
  mrbc_prism_objs.delete_if { |o| o.include?('gem_init') || o.include?('mruby_compat') }

  exe_name = 'mrbc'

  file exefile("#{build.build_dir}/bin/#{exe_name}") => mrbc_prism_objs do |t|
    build.linker.run t.name, t.prerequisites
  end

  build.bins << exe_name

end
