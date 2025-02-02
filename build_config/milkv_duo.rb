# Cross Compiling configuration for Milk-V Duo.
# To build (on Ubuntu 24.04): rake MRUBY_CONFIG=build_config/milkv_duo.rb
#
# Requires: https://github.com/milkv-duo/duo-sdk
#
MRuby::CrossBuild.new("milkv_duo") do |conf|
  # Set this string to match your board: milkv_duo, milkv_duo256m, milkv_duos
  MILKV_DUO_VARIANT = "milkv_duo256m"

  # Expect duo-sdk directory is same level as (next to) mruby top-level directory.
  SDK_BASE = File.expand_path("../../../", File.expand_path(__FILE__)) + "/duo-sdk"
  TOOLCHAIN_BASE = "#{SDK_BASE}/riscv64-linux-musl-x86_64"
  SYSROOT = "#{SDK_BASE}/rootfs"

  toolchain :gcc

  # C compiler settings
  conf.cc do |cc|
    cc.command = "#{TOOLCHAIN_BASE}/bin/riscv64-unknown-linux-musl-gcc"
    cc.include_paths << "#{TOOLCHAIN_BASE}/lib/gcc/riscv64-unknown-linux/musl/10.2.0/include-fixed"
    cc.include_paths << "#{TOOLCHAIN_BASE}/lib/gcc/riscv64-unknown-linux/musl/10.2.0/include"
    cc.include_paths << "#{TOOLCHAIN_BASE}/riscv64-unknown-linux/include"
    cc.include_paths << "#{TOOLCHAIN_BASE}/include"
    cc.include_paths << "#{SYSROOT}/usr/include"
    cc.flags << ["-mcpu=c906fdv", "-march=rv64imafdcv0p7xthead", "-mcmodel=medany", "-mabi=lp64d"]
    cc.flags << ["-D_LARGEFILE_SOURCE", "-D_LARGEFILE64_SOURCE", "-D_FILE_OFFSET_BITS=64"]
    cc.flags << ["-Wl,--copy-dt-needed-entries", "-Wl,-lc,-lgcc_s,-lwiringx"]
    cc.defines << "MILKV_DUO_VARIANT=_#{MILKV_DUO_VARIANT}"
  end

  # Linker settings
  conf.linker do |linker|
    linker.command = cc.command
    linker.library_paths << ["#{SYSROOT}/lib", "#{SYSROOT}/usr/lib"]
    linker.flags = cc.flags
  end

  # Archiver settings
  conf.archiver do |archiver|
    archiver.command = "#{TOOLCHAIN_BASE}/bin/riscv64-unknown-linux-musl-gcc-ar"
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
  conf.gem 'mrbgems/mruby-cmath/'
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

  # GPIO, ADC, PWM, I2C and SPI support for Milk-V Duo
  # conf.gem :github => 'denko-rb/mruby-milkv-duo'
end
