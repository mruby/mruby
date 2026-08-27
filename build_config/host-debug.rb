MRuby::Build.new('host') do |conf|
  # load specific toolchain settings
  conf.toolchain

  conf.enable_debug

  # include the default GEMs
  conf.gembox 'full-core'

  # C compiler settings
  # `+=` keeps MRB_DEBUG, which conf.enable_debug added above
  conf.cc.defines += %w(MRB_USE_DEBUG_HOOK MRB_NO_BOXING)

  # Generate mruby debugger command (require mruby-eval)
  conf.gem :core => "mruby-bin-debugger"

  # Regexp is included via stdlib.gembox

  # The compile_commands.json at the source root speaks for this build
  conf.enable_compile_commands default: true

  # test
  conf.enable_test
  # bintest
  conf.enable_bintest
end
