# Define cross build settings
MRuby::CrossBuild.new('no-float') do |conf|
  conf.toolchain

  # Add configuration
  conf.compilers.each do |c|
    c.defines << "MRB_NO_FLOAT"
  end

  conf.gem :core => "mruby-bin-mruby"

  conf.test_runner.command = 'env'

  # The compile_commands.json at the source root speaks for this build
  conf.enable_compile_commands default: true

  conf.enable_debug
#  conf.enable_bintest
  conf.enable_test
end
