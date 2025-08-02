# Cross Compiling configuration for the Sega Dreamcast
# https://dreamcast.wiki/Using_Ruby_for_Sega_Dreamcast_development
#
# Requires KallistiOS (KOS)
# http://gamedev.allusion.net/softprj/kos/
#
# This configuration has been improved to be used as KallistiOS Port (kos-ports)
# Updated: 2025-07-31
#
# Tested on GNU/Linux, macOS and Windows (MinGW-w64/MSYS2, Cygwin, DreamSDK)
# DreamSDK is based on both MinGW/MSYS and MinGW-w64/MSYS2: https://dreamsdk.org/
#
# Install mruby for Sega Dreamcast using the "mruby" kos-port.
# See: https://github.com/kallistios/kallistios
#
# If you want to see examples, check the /examples/dreamcast/mruby directory
# in the KallistiOS repository.
#
MRuby::CrossBuild.new("dreamcast") do |conf|
  toolchain :gcc

  # Getting critical environment variables
  KOS_BASE = ENV["KOS_BASE"]
  KOS_CC_BASE = ENV["KOS_CC_BASE"]

  # Check environment variables
  if KOS_BASE.to_s.empty?
    raise "Error: KallistiOS is required; KOS_BASE need to be declared; Stop."
  end

  # Root directory for KallistiOS wrappers
  # This will handle specific DreamSDK wrappers if needed
  KOS_WRAPPERS_BASE = if ENV["ENVIRONMENT_NAME"] == "DreamSDK" && ENV["RAKE_AVAILABLE"] == "0"
                      "#{KOS_CC_BASE}/bin"
                    else
                      "#{KOS_BASE}/utils/build_wrappers"
                    end

  # C compiler
  conf.cc do |cc|
    cc.command = "#{KOS_WRAPPERS_BASE}/kos-cc"
  end

  # C++ compiler
  conf.cxx do |cxx|
    cxx.command = "#{KOS_WRAPPERS_BASE}/kos-c++"
  end

  # Linker
  conf.linker do |linker|
    linker.command = "#{KOS_WRAPPERS_BASE}/kos-ld"
  end

  # Archiver
  conf.archiver do |archiver|
    archiver.command = "#{KOS_WRAPPERS_BASE}/kos-ar"
  end

  # No executables needed for KallistiOS
  conf.bins = []

  # Do not build test binaries
  conf.build_mrbtest_lib_only

  # Gemboxes
  conf.gembox "default-no-stdio"
  conf.gembox "stdlib-ext"
  conf.gembox "metaprog"

  # Additional Gems
  # Currently unsupported on KallistiOS: "mruby-io", "mruby-dir", "mruby-socket"
  conf.gem :core => "mruby-binding"
  conf.gem :core => "mruby-catch"
  conf.gem :core => "mruby-enum-chain"
  conf.gem :core => "mruby-errno"
  conf.gem :core => "mruby-error"
  conf.gem :core => "mruby-exit"
  conf.gem :core => "mruby-os-memsize"
  conf.gem :core => "mruby-proc-binding"
  conf.gem :core => "mruby-sleep"
end
