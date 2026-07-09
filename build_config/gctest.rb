# Task-scheduled GC test build (PicoRuby-independent).
#
# Runs the mruby core test suite plus the mruby-task gem tests under mrbtest,
# with the scheduler and GC primitives compiled in. Keep this build focused:
# unrelated default-gembox tests and bintests make this target noisy.
#
#   MRUBY_CONFIG=gctest rake test
#
MRuby::Build.new('gctest') do |conf|
  conf.toolchain :gcc
  conf.enable_debug

  conf.cc.defines << 'MRB_INT64'
  conf.cc.defines << 'MRB_NO_BOXING'
  conf.cc.defines << 'MRB_GC_PROFILE'

  conf.gem core: 'mruby-compiler'
  conf.gem core: 'mruby-bin-mrbc'
  conf.gem core: 'mruby-task'

  conf.enable_test
end
