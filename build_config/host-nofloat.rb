# MRB_NO_FLOAT build in a dedicated build directory (build/host-nofloat) so it
# does not clobber a normal host build.
MRuby::Build.new('host-nofloat') do |conf|
  # load specific toolchain settings
  toolchain :gcc

  # include the GEM box
  conf.gembox "stdlib"
  conf.gembox "stdlib-ext"
  conf.gembox "stdlib-io"
  conf.gembox "metaprog"

  # none of the gemboxes above supplies mrbc, and a build that is not named
  # 'host' cannot borrow one from a 'host' target that is not being built
  conf.gem :core => 'mruby-bin-mrbc'
  conf.gem :core => 'mruby-bin-mruby'
  conf.gem :core => 'mruby-bin-mirb'

  # Add configuration
  conf.compilers.each do |c|
    c.defines << "MRB_NO_FLOAT"
  end

  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end
