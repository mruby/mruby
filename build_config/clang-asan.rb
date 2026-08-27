# Address/Undefined sanitizer build in a dedicated build directory
# (build/clang-asan) so it does not clobber a normal host build.
MRuby::Build.new('clang-asan') do |conf|
  conf.toolchain :clang
  # include the GEM box
  conf.gembox 'full-core'


  # Turn on `enable_debug` for better debugging
  conf.enable_sanitizer "address,undefined"
  conf.enable_debug
  conf.enable_bintest
  conf.enable_test
end
