STDOUT.sync = STDERR.sync = true unless Rake.application.options.always_multitask

# MRB_GC_STRESS collects on every allocation, so this build's suite is the one
# that takes real time: 33 to 47 seconds against one second for each of the
# others, on every runner that executes its own instructions.  Under emulation
# that ratio is what decides a job's length -- 319 seconds of an armhf job's
# 597 -- and what it buys there is a GC the other runners already stress.  A
# runner that says it is emulated is left the four builds whose value is the
# ABI they compile for.
if ENV['MRUBY_CI_EMULATED'].to_s.empty?
MRuby::Build.new('full-debug') do |conf|
  conf.toolchain
  conf.enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines += %w(MRB_GC_STRESS MRB_USE_DEBUG_HOOK)


  conf.enable_test
end
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

MRuby::Build.new('ascii-ctype') do |conf|
  conf.toolchain

  # The one build here that indexes by character and classifies it by ASCII.
  # Both halves of that pair are what it covers: core's ASCII case conversion,
  # and what mruby-regexp answers without the tables, the refusal a pattern
  # gets when /i is asked for a folding the build has no table for and a POSIX
  # bracket holding its ASCII and no character above it. The refusal has no
  # other home, since a build reading its strings as bytes has no character to
  # refuse, so mruby-regexp/test/ascii_case.rb skips its assertions there.
  # Tests only, for the reason byte-string gives above.
  conf.gembox 'full-core'
  conf.cc.defines << 'MRB_USE_ASCII_CTYPE'

  # A second mruby-regexp refusal that needs a build of its own: the parse
  # depth limit. Reaching a limit costs the C stack that limit stands for,
  # about 600 bytes a level, so no build can be asked to reach the default
  # 4096, whose 2.4 MiB is more than a Windows thread's whole stack, and the
  # test skips above 512 for that reason. This build sets a limit a test may
  # reach anywhere in the matrix, some 300 KiB, which is what gives
  # `parse depth limit over` a home; the arithmetic that refuses is the same
  # at any limit. It rides along here rather than in a build of its own so
  # that the matrix runs no extra pass, and it costs this build nothing: the
  # depth a pattern may nest is not what ASCII classification is about.
  conf.cc.defines << 'MRB_REGEXP_PARSE_DEPTH_LIMIT=512'

  conf.enable_test
end
