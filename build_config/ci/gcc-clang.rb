STDOUT.sync = STDERR.sync = true unless Rake.application.options.always_multitask

MRuby::Build.new('full-debug') do |conf|
  conf.toolchain
  conf.enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines += %w(MRB_GC_STRESS MRB_USE_DEBUG_HOOK)

  # Widen the regexp /i flag from ASCII letters to the 1:1 Unicode case
  # foldings, which it reads off the case table core already carries. The
  # option is off by default because of the walks it adds over that table, so
  # mruby-regexp/test/unicode_case.rb is only compiled into a build that turns
  # it on, and without one here those walks ship untested. It goes on this
  # build rather than a job of its own so it costs no runner; the other two
  # builds in this file keep the default, which is what
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

MRuby::Build.new('byte-string') do |conf|
  conf.toolchain

  # The one build here whose strings index by byte. mruby-encoding is what
  # defines MRB_UTF8_STRING, so dropping it is what makes "あ".length 3, and
  # the tests written as the byte-indexed mirror of the UTF-8 ones (String#scrub
  # degrading to a no-op, the byte-counting halves of mruby-regexp and
  # mruby-string-ext) run nowhere else: every other build in CI, here and in
  # ci/msvc, keeps the gem. Taking it out of full-core rather than reaching for
  # a smaller gembox keeps the rest of the box on the byte-indexed side too.
  # Tests only, since the binaries full-core adds are the same ones the bintest
  # above already covers.
  conf.gembox 'full-core'
  conf.gems.delete 'mruby-encoding'

  conf.enable_test
end
