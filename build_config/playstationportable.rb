# Cross Compiling configuration for the Sony PlayStation Portable.
# This configuration requires toolchain from https://github.com/pspdev

MRuby::CrossBuild.new("playstationportable") do |conf|
  toolchain :gcc

  PSPDEV_PATH = "#{ENV['PSPDEV']}"
  BIN_PATH = "#{PSPDEV_PATH}/bin"

  # C compiler
  conf.cc do |cc|
    cc.command = "#{BIN_PATH}/psp-gcc"
    cc.flags << ["-O2", "-D_PSP_FW_VERSION=600"]
    cc.include_paths << ["#{PSPDEV_PATH}/psp/include", "#{PSPDEV_PATH}/psp/sdk/include"]
    cc.compile_options = %(%{flags} -o "%{outfile}" -c "%{infile}")
  end

  # C++ compiler
  conf.cxx do |cxx|
    cxx.command = "#{BIN_PATH}/psp-g++"
    cxx.include_paths = conf.cc.include_paths.dup
    cxx.flags = conf.cc.flags.dup
    cxx.flags << %w[-fno-rtti -fno-exceptions]
    cxx.defines = conf.cc.defines.dup
    cxx.compile_options = conf.cc.compile_options.dup
  end

  # Linker
  conf.linker do |linker|
    linker.command = "#{BIN_PATH}/psp-gcc"
    linker.flags << ["-Wl,-zmax-page-size=128"]
  end

  # No executables
  conf.bins = []

  # Do not build executable test
  conf.build_mrbtest_lib_only

  # Gems from core
  conf.gem :core => "mruby-metaprog"
  conf.gem :core => "mruby-pack"
  conf.gem :core => "mruby-sprintf"
  conf.gem :core => "mruby-math"
  conf.gem :core => "mruby-time"
  conf.gem :core => "mruby-struct"
  conf.gem :core => "mruby-compar-ext"
  conf.gem :core => "mruby-enum-ext"
  conf.gem :core => "mruby-string-ext"
  conf.gem :core => "mruby-numeric-ext"
  conf.gem :core => "mruby-array-ext"
  conf.gem :core => "mruby-hash-ext"
  conf.gem :core => "mruby-range-ext"
  conf.gem :core => "mruby-proc-ext"
  conf.gem :core => "mruby-symbol-ext"
  conf.gem :core => "mruby-random"
  conf.gem :core => "mruby-object-ext"
  conf.gem :core => "mruby-objectspace"
  conf.gem :core => "mruby-fiber"
  conf.gem :core => "mruby-enumerator"
  conf.gem :core => "mruby-enum-lazy"
  conf.gem :core => "mruby-toplevel-ext"
  conf.gem :core => "mruby-kernel-ext"
  conf.gem :core => "mruby-class-ext"
  conf.gem :core => "mruby-compiler"
  conf.gem :core => "mruby-binding"
  conf.gem :core => "mruby-catch"
  conf.gem :core => "mruby-enum-chain"
  conf.gem :core => "mruby-errno"
  conf.gem :core => "mruby-error"
  conf.gem :core => "mruby-exit"
  conf.gem :core => "mruby-os-memsize"
  conf.gem :core => "mruby-proc-binding"
  conf.gem :core => "mruby-sleep"
  conf.gem :core => "mruby-io"
  conf.gem :core => "mruby-dir"
  #conf.gem :core => "mruby-socket" unsupported
end
