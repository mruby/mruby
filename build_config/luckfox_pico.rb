# Cross Compiling configuration for Luckfox Pico embedded SBC.
#
# To build on Ubuntu x86_64: rake MRUBY_CONFIG=build_config/luckfox_pico.rb
# Uses Buildroot SDK for this board. Binaries run on the corresponding Linux image.
# Requires: https://github.com/LuckfoxTECH/luckfox-pico
#
# NOTE: default config includes all standard mrbgems, EXCEPT: mruby-cmath
#
MRuby::CrossBuild.new("luckfox_pico") do |conf|
  # Clone the luckfox-pico repo above next to (same directory level) as mruby.
  SDK_BASE = File.realpath(File.expand_path("../../../", File.expand_path(__FILE__)) + "/luckfox-pico")
  TOOLCHAIN_BASE = "#{SDK_BASE}/tools/linux/toolchain/arm-rockchip830-linux-uclibcgnueabihf"
  SYSROOT = "#{TOOLCHAIN_BASE}/arm-rockchip830-linux-uclibcgnueabihf/sysroot"

  toolchain :gcc

  # C compiler settings
  conf.cc do |cc|
    cc.command = "#{TOOLCHAIN_BASE}/bin/arm-rockchip830-linux-uclibcgnueabihf-gcc"

    cc.include_paths << "#{TOOLCHAIN_BASE}/lib/gcc/arm-rockchip830-linux-uclibcgnueabihf/8.3.0/include"
    cc.include_paths << "#{TOOLCHAIN_BASE}/lib/gcc/arm-rockchip830-linux-uclibcgnueabihf/8.3.0/include-fixed"
    cc.include_paths << "#{TOOLCHAIN_BASE}/arm-rockchip830-linux-uclibcgnueabihf/include/c++/8.3.0/arm-rockchip830-linux-uclibcgnueabihf"
    cc.include_paths << "#{SYSROOT}/usr/include"

    # Flags taken from the SDK's Makefile
    cc.flags << ["-march=armv7-a", "-mfpu=neon", "-mfloat-abi=hard"]
    cc.flags << ["-D_LARGEFILE_SOURCE", "-D_LARGEFILE64_SOURCE", "-D_FILE_OFFSET_BITS=64", "-ffunction-sections", "-fdata-sections"]
    cc.flags << ["-O2", "-fPIC"]
    cc.flags << ["-Wl,--copy-dt-needed-entries", "-Wl,-lc,-lgcc_s"]
  end

  # Linker settings
  conf.linker do |linker|
    linker.command = cc.command
    linker.library_paths << "#{TOOLCHAIN_BASE}/arm-rockchip830-linux-uclibcgnueabihf/lib"
    linker.flags = cc.flags
  end

  # Archiver settings
  conf.archiver do |archiver|
    archiver.command = "#{TOOLCHAIN_BASE}/bin/arm-rockchip830-linux-uclibcgnueabihf-ar"
  end

  # Do not build executable test
  conf.build_mrbtest_lib_only

  # Disable C++ exception
  conf.disable_cxx_exception

  # All standard gems.
  conf.gem 'mrbgems/mruby-array-ext/'
  conf.gem 'mrbgems/mruby-bigint/'
  conf.gem 'mrbgems/mruby-bin-config/'
  conf.gem 'mrbgems/mruby-bin-debugger/'
  conf.gem 'mrbgems/mruby-bin-mirb/'
  conf.gem 'mrbgems/mruby-bin-mrbc/'
  conf.gem 'mrbgems/mruby-bin-mruby/'
  conf.gem 'mrbgems/mruby-bin-strip/'
  conf.gem 'mrbgems/mruby-binding/'
  conf.gem 'mrbgems/mruby-catch/'
  conf.gem 'mrbgems/mruby-class-ext/'
  # SDK doesn't include complex math for uClibc
  # conf.gem 'mrbgems/mruby-cmath/'
  conf.gem 'mrbgems/mruby-compar-ext/'
  conf.gem 'mrbgems/mruby-compiler/'
  conf.gem 'mrbgems/mruby-complex/'
  conf.gem 'mrbgems/mruby-data/'
  conf.gem 'mrbgems/mruby-dir/'
  conf.gem 'mrbgems/mruby-enum-chain/'
  conf.gem 'mrbgems/mruby-enum-ext/'
  conf.gem 'mrbgems/mruby-enum-lazy/'
  conf.gem 'mrbgems/mruby-enumerator/'
  conf.gem 'mrbgems/mruby-errno/'
  conf.gem 'mrbgems/mruby-error/'
  conf.gem 'mrbgems/mruby-eval/'
  conf.gem 'mrbgems/mruby-exit/'
  conf.gem 'mrbgems/mruby-fiber/'
  conf.gem 'mrbgems/mruby-hash-ext/'
  conf.gem 'mrbgems/mruby-io/'
  conf.gem 'mrbgems/mruby-kernel-ext/'
  conf.gem 'mrbgems/mruby-math/'
  conf.gem 'mrbgems/mruby-metaprog/'
  conf.gem 'mrbgems/mruby-method/'
  conf.gem 'mrbgems/mruby-numeric-ext/'
  conf.gem 'mrbgems/mruby-object-ext/'
  conf.gem 'mrbgems/mruby-objectspace/'
  conf.gem 'mrbgems/mruby-os-memsize/'
  conf.gem 'mrbgems/mruby-pack/'
  conf.gem 'mrbgems/mruby-proc-binding/'
  conf.gem 'mrbgems/mruby-proc-ext/'
  conf.gem 'mrbgems/mruby-random/'
  conf.gem 'mrbgems/mruby-range-ext/'
  conf.gem 'mrbgems/mruby-rational/'
  conf.gem 'mrbgems/mruby-set/'
  conf.gem 'mrbgems/mruby-sleep/'
  conf.gem 'mrbgems/mruby-socket/'
  conf.gem 'mrbgems/mruby-sprintf/'
  conf.gem 'mrbgems/mruby-string-ext/'
  conf.gem 'mrbgems/mruby-struct/'
  conf.gem 'mrbgems/mruby-symbol-ext/'
  # conf.gem 'mrbgems/mruby-test-inline-struct/'
  # conf.gem 'mrbgems/mruby-test/'
  conf.gem 'mrbgems/mruby-time/'
  conf.gem 'mrbgems/mruby-toplevel-ext/'
end
