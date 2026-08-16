# gprof build in a dedicated build directory (build/host-gprof) so it does not
# clobber a normal host build.
MRuby::Build.new('host-gprof') do |conf|
  # load specific toolchain settings
  toolchain :gcc

  # include the GEM box
  conf.gembox 'full-core'

  conf.cc.flags << '-pg'
  conf.linker.flags << '-pg'

  # Turn on `enable_debug` for better debugging
  conf.enable_debug
  conf.enable_test
end
