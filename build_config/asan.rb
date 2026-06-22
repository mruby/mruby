# Address/Undefined sanitizer build in a dedicated build directory (build/asan)
# so it does not clobber a normal host build. Used by the pre-push hook to catch
# memory-safety regressions (see issues #6905, #6906) before pushing.
MRuby::Build.new('asan') do |conf|
  conf.toolchain :clang

  conf.gembox 'full-core'

  conf.enable_sanitizer "address,undefined"
  conf.enable_debug
  conf.enable_bintest
  conf.enable_test
end
