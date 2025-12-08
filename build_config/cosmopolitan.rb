# Cosmopolitan Libc build configuration for mruby
# https://github.com/jart/cosmopolitan
#
# Produces Actually Portable Executables (APE) - single binaries that run on:
#   - Linux (x86_64, ARM64)
#   - macOS (x86_64, ARM64)
#   - Windows (x86_64)
#   - FreeBSD (x86_64)
#   - OpenBSD (x86_64)
#   - NetBSD (x86_64)
#
# Requirements:
#   Download cosmocc toolchain from https://cosmo.zip/pub/cosmocc/
#
# Usage:
#   COSMO_ROOT=/path/to/cosmocc rake MRUBY_CONFIG=cosmopolitan
#
# The resulting binaries in bin/ will have .com extension and run on all
# supported platforms without recompilation.

COSMO_ROOT = ENV['COSMO_ROOT']

unless COSMO_ROOT && File.directory?(COSMO_ROOT)
  msg = <<~MSG
    Cosmopolitan toolchain not found.

    Please set COSMO_ROOT environment variable to the cosmocc directory:

      mkdir -p ~/cosmo && cd ~/cosmo
      wget https://cosmo.zip/pub/cosmocc/cosmocc.zip
      unzip cosmocc.zip
      export COSMO_ROOT=~/cosmo

    Then run:
      COSMO_ROOT=~/cosmo rake MRUBY_CONFIG=cosmopolitan
  MSG
  raise msg
end

MRuby::Build.new do |conf|
  # C compiler
  conf.cc do |cc|
    cc.command = "#{COSMO_ROOT}/bin/cosmocc"
    cc.flags = %w[-Os -fno-omit-frame-pointer]
  end

  # C++ compiler
  conf.cxx do |cxx|
    cxx.command = "#{COSMO_ROOT}/bin/cosmoc++"
    cxx.flags = conf.cc.flags.dup
  end

  # Linker
  conf.linker do |linker|
    linker.command = "#{COSMO_ROOT}/bin/cosmocc"
    linker.flags = %w[-static]
  end

  # Archiver
  conf.archiver do |archiver|
    archiver.command = "#{COSMO_ROOT}/bin/cosmoar"
  end

  # APE binaries use .com extension
  conf.exts.executable = '.com'

  # Cosmopolitan provides POSIX compatibility, explicitly select POSIX HALs
  conf.gem core: 'hal-posix-io'
  conf.gem core: 'hal-posix-socket'
  conf.gem core: 'hal-posix-dir'

  # Standard library
  conf.gembox 'stdlib'
  conf.gembox 'stdlib-ext'
  conf.gembox 'stdlib-io'  # Includes mruby-io, mruby-socket, mruby-dir
  conf.gembox 'math'
  conf.gembox 'metaprog'

  # Binary tools
  # Note: mruby-bin-config is a shell script and incompatible with .com extension
  conf.gem core: 'mruby-bin-mrbc'
  conf.gem core: 'mruby-bin-mruby'
  conf.gem core: 'mruby-bin-strip'
  conf.gem core: 'mruby-bin-mirb'
  conf.gem core: 'mruby-bin-debugger'
end
