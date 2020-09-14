MRuby::Build.new('host') do |conf|
  # load specific toolchain settings

  # Gets set by the VS command prompts.
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    toolchain :visualcpp
  else
    toolchain :gcc
  end

  enable_debug

  # include the default GEMs
  conf.gembox 'full-core'

  # C compiler settings
  conf.cc.defines = %w(MRB_ENABLE_DEBUG_HOOK MRB_NO_BOXING)

  # Generate mruby debugger command (require mruby-eval)
  conf.gem :core => "mruby-bin-debugger"

  # test
  enable_test
  # bintest
  enable_bintest
end
