STDOUT.sync = STDERR.sync = true unless Rake.application.options.always_multitask

# Two switches take builds off a runner, and each says what it is about rather
# than which runner it was written for.
#
# MRUBY_CI_ABI_ONLY: the runner is in the matrix for the ABI it compiles, and
# what a build covers that reads the same everywhere is covered elsewhere. It
# drops full-debug and no-bigint. full-debug is the GC under MRB_GC_STRESS, a
# collector every other runner stresses too; no-bigint is an Integer that ends
# at mrb_int, which is a property of the define rather than of the machine.
# What is left is the four builds whose value is the ABI they compile for.
#
# MRUBY_CI_EMULATED: the runner does not execute its own instructions, so a
# suite's running time is what decides the job's length. It drops no-float on
# top of the above. no-float is not in the ABI_ONLY group because it reaches
# the integer arms of Time and sleep, and a platform with a clock of its own
# can reach code no other runner compiles; an emulated Linux shares its clock
# with the native Linux runners and does not.
#
# The two costs are different, which is why the two switches are. Under
# emulation what costs is running the suites: MRB_GC_STRESS collects on every
# allocation, and full-debug's suite takes 33 to 47 seconds against one second
# for each of the others, 319 seconds of an armhf job's 597. (Those figures
# include the stress tests, which since then run only where MRBTEST_STRESS is
# set, the two native x86-64 rows; without them full-debug's suite is a
# quarter of that.) On the Windows
# runners what costs is compiling: the suites there finish in under a minute
# together while the seven builds take ten, so dropping two of them is 628
# object files of 2204.
if ENV['MRUBY_CI_ABI_ONLY'].to_s.empty?
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

  # mruby-process reads CPU time through getrusage(2) wherever the header
  # probe finds <sys/resource.h>, which is every host this matrix runs on, so
  # its times(2) fallback would otherwise compile nowhere in CI. Answering the
  # probe no by hand is the override the port documents as standing on its
  # own. It rides along here for the reason the depth limit above does: no
  # extra pass, and CPU time accounting is not what ASCII classification is
  # about.
  conf.cc.defines << 'MRB_PROCESS_HAVE_GETRUSAGE=0'

  conf.enable_test
end

# Neither of the two builds below is about an ABI, and each is left off a
# runner by the switch that names its own cost, for the accounting at the top
# of this file. This one is off an emulated runner only. Taking Float out of
# the numeric tower reaches the integer arms of Time and sleep, so a platform
# holding a clock of its own reaches code compiled nowhere else here, which is
# why a native runner keeps it however slowly it builds; an emulated Linux
# holds the same clock as the native Linux runners and keeps nothing they do
# not already compile.
if ENV['MRUBY_CI_EMULATED'].to_s.empty?
MRuby::Build.new('no-float') do |conf|
  conf.toolchain

  # The one build here without Float. MRB_NO_FLOAT takes the type out of the
  # numeric tower, and what is written for its absence compiles nowhere else
  # in CI: the integer arms of Time, Rational and sleep, the %f that String#%
  # refuses, and the codegen that reads a float literal as Integer 0 rather
  # than a pool entry. The gems that refuse the define say so with #error and
  # leave full-core here, Math, Complex and CMath; mruby-benchmark leaves it
  # too, its Tms being fractional seconds its README documents as Float and
  # its to_s formatting them with %f.
  # Tests only, for the reason byte-string gives above.
  conf.gembox 'full-core'
  %w(mruby-math mruby-complex mruby-cmath mruby-benchmark).each do |g|
    conf.gems.delete g
  end
  conf.compilers.each do |c|
    c.defines << 'MRB_NO_FLOAT'
  end

  conf.enable_test
end

end

# Where Integer ends is what the define says, not what the machine does, so
# this is one of the two the ABI_ONLY switch takes off a runner asked only for
# the ABI it compiles.
if ENV['MRUBY_CI_ABI_ONLY'].to_s.empty?
MRuby::Build.new('no-bigint') do |conf|
  conf.toolchain

  # The one build here whose Integer ends at mrb_int. Every other build in
  # CI carries mruby-bigint, so the RangeError that arithmetic raises where
  # mrb_int overflows, the literal the compiler refuses for the same reason,
  # and the half of each width-dependent test written for the gem's absence
  # run nowhere else; on the i686 runner that edge is 32 bits wide.
  # Tests only, for the reason byte-string gives above.
  conf.gembox 'full-core'
  conf.gems.delete 'mruby-bigint'

  conf.enable_test
end
end
