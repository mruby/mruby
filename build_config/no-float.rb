MRuby::Build.new("host") do |conf|
  # load specific toolchain settings
  toolchain :gcc
  conf.gem :core => "mruby-bin-mrbc"
end

# Define cross build settings
MRuby::CrossBuild.new('no-float') do |conf|
  toolchain :gcc

  # include the GEM box
  conf.compilers.each do |c|
    c.defines << "MRB_NO_FLOAT"
  end

  conf.gem :core => "mruby-bin-mruby"
  conf.gem :core => "mruby-test"

  conf.test_runner.command = 'env'

  conf.enable_debug
#  conf.enable_bintest
  conf.enable_test
end
