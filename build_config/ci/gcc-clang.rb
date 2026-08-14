STDOUT.sync = STDERR.sync = true unless Rake.application.options.always_multitask

MRuby::Build.new('full-debug') do |conf|
  conf.toolchain
  conf.enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines += %w(MRB_GC_STRESS MRB_USE_DEBUG_HOOK)

  # Widen the regexp /i flag from ASCII letters to the 1:1 Unicode case
  # foldings. The option is off by default because of the table it carries, so
  # mruby-regexp/test/unicode_case.rb is only compiled into a build that turns
  # it on, and without one here the generated table ships untested. It goes on
  # this build rather than a job of its own so it costs no runner; the other
  # two builds in this file keep the default, which is what
  # mruby-regexp/test/ascii_case.rb needs, so both sides stay covered.
  conf.cc.defines << 'MRB_REGEXP_UNICODE_CASE'

  conf.enable_test
end

MRuby::Build.new('bintest') do |conf|
  conf.toolchain

  # include all core GEMs
  conf.gembox 'full-core'
  conf.gem :core => 'mruby-bin-debugger'
  conf.compilers.each do |c|
    c.defines += %w(MRB_GC_FIXED_ARENA)
  end
  conf.enable_bintest
  conf.enable_test
end

MRuby::Build.new('cxx_abi') do |conf|
  conf.toolchain

  # Full C++ ABI build with the Prism compiler. Prism is a vendored C library
  # and cannot be compiled as C++ (neither g++ nor clang++ accepts its
  # generated code), so mruby-compiler's mrbgem.rake keeps the Prism sources on
  # the C compiler; the rest of mruby, including the compiler glue, is built as
  # C++.
  conf.gembox 'full-core'
  conf.compilers.each do |c|
    c.defines += %w(MRB_GC_FIXED_ARENA)
  end
  conf.enable_test

  conf.enable_cxx_abi
end

MRuby::Build.new('default') do |conf|
  conf.toolchain

  # The one build here on the default gembox. It leaves out mruby-encoding,
  # which is what defines MRB_UTF8_STRING, so its strings index by byte. The
  # tests written as the byte-indexed mirror of the UTF-8 ones (String#scrub
  # degrading to a no-op, the byte-counting halves of mruby-regexp and
  # mruby-string-ext) run nowhere else: every other build in CI, here and in
  # ci/msvc, takes full-core. Tests only, since the binaries this gembox adds
  # are the same ones the bintest above already covers.
  conf.gembox 'default'

  conf.enable_test
end
