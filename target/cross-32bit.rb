# Define cross build settings
MRuby::CrossBuild.new('32bit') do |conf|
  toolchain :gcc

  conf.cc.flags << "-m32"
  conf.linker.flags << "-m32"

  conf.build_mrbtest_lib_only

  conf.gem 'examples/mrbgems/c_and_ruby_extension_example'

  conf.test_runner.command = 'env'
end
