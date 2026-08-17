# Address/Undefined sanitizer build with gcc, in a dedicated build directory
# (build/gcc-asan) so it does not clobber a normal host build. The counterpart
# of build_config/clang-asan.rb.
MRuby::Build.new('gcc-asan') do |conf|
  conf.toolchain :gcc

  # include the GEM box
  conf.gembox 'full-core'

  # Turn on `enable_debug` for better debugging
  conf.enable_sanitizer "address,undefined"
  conf.enable_debug
  conf.enable_bintest
  conf.enable_test
end
