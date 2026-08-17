# Address/Undefined sanitizer build in a dedicated build directory (build/asan)
# so it does not clobber a normal host build. Used by the pre-push hook to catch
# memory-safety regressions (see issues #6905, #6906) before pushing.
#
# The toolchain is the guessed one, so this config runs wherever a sanitizer
# runtime is installed: gcc with libasan, or clang with compiler-rt. Pin
# build_config/clang-asan.rb instead to ask for clang specifically.
MRuby::Build.new('asan') do |conf|
  conf.toolchain

  conf.gembox 'full-core'

  conf.enable_sanitizer "address,undefined"
  conf.enable_debug
  conf.enable_bintest
  conf.enable_test
end
