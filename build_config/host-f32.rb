# MRB_USE_FLOAT32 build in a dedicated build directory (build/host-f32) so it
# does not clobber a normal host build.
MRuby::Build.new('host-f32') do |conf|
  # load specific toolchain settings
  toolchain :gcc

  # include the GEM box
  conf.gembox 'full-core'

  conf.cc.defines << 'MRB_USE_FLOAT32'


  # Turn on `enable_debug` for better debugging
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end
