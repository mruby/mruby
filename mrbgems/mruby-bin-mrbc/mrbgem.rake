MRuby::Gem::Specification.new 'mruby-bin-mrbc' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby & PicoRuby developers'
  spec.summary = 'mruby compiler executable using Prism'

  spec.add_dependency 'mruby-compiler', core: 'mruby-compiler'

  exe_name = 'mrbc'

  build.bins << exe_name

  build_settings do
    # The compiler gem is set up by the time this block runs, whatever order
    # the gems were processed in.
    compiler = build.gems['mruby-compiler']

    # Prism's generated headers are written under the build directory rather
    # than into the submodule, so both directories are named here.
    spec.cc.include_paths << "#{build.build_root}/prism/include"
    spec.cc.include_paths << "#{MRUBY_ROOT}/mrbgems/mruby-compiler/lib/prism/include"

    # The layout of struct mrc_ccontext depends on MRC_TARGET_*; the tool's own
    # translation units must use the same target define as mruby-compiler,
    # otherwise field offsets (e.g. diagnostic_list) disagree across the link.
    # Copy the one that gem settled on rather than deriving it a second time
    # from the same build: two derivations can disagree, a copy cannot.
    spec.cc.defines += compiler.cc.defines.flatten.grep(/\AMRC_TARGET_/)

    mrbc_prism_objs = Dir.glob("#{dir}/tools/mrbc/*.c").map { |f| objfile(f.pathmap("#{build_dir}/tools/mrbc/%n")) }
    mrbc_prism_objs += compiler.objs
    mrbc_prism_objs.delete_if { |o| o.include?('gem_init') || o.include?('mruby_compat') }

    file exefile("#{build.build_dir}/bin/#{exe_name}") => mrbc_prism_objs do |t|
      build.linker.run t.name, t.prerequisites
    end
  end
end
