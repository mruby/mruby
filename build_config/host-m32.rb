# 32-bit build in a dedicated build directory (build/host-m32) so it does not
# clobber a normal host build.
MRuby::Build.new('host-m32') do |conf|
  # load specific toolchain settings
  toolchain :gcc

  # include the GEM box
  conf.gembox 'full-core'

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'


  # Turn on `enable_debug` for better debugging
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end
