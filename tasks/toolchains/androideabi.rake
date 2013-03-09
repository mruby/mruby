# Download and unarchive latest Android NDK from https://developer.android.com/tools/sdk/ndk/index.html
# Make custom standalone toolchain as described here (android_ndk/docs/STANDALONE-TOOLCHAIN.html)
# Please export custom standalone toolchain path
#   export ANDROID_STANDALONE_TOOLCHAIN=/tmp/android-14-toolchain

# Add to your build_config.rb
# MRuby::CrossBuild.new('androideabi') do |conf|
#   toolchain :androideabi
# end

MRuby::Toolchain.new(:androideabi) do |conf|
  toolchain :gcc

  DEFAULT_ANDROID_TOOLCHAIN   = 'gcc'
  DEFAULT_ANDROID_TARGET_ARCH = 'arm'
  DEFAULT_ANDROID_TARGET_ARCH_ABI = 'armeabi'
  GCC_COMMON_CFLAGS  = %W(-ffunction-sections -funwind-tables -fstack-protector)
  GCC_COMMON_LDFLAGS = %W()

  # An environment variable 'ANDROID_STANDALONE_TOOLCHAIN' must be set a path to toolchains.
  ANDROID_STANDALONE_TOOLCHAIN = ENV['ANDROID_STANDALONE_TOOLCHAIN']
  SYSROOT = ENV['ANDROID_STANDALONE_TOOLCHAIN'] + '/sysroot'

  ANDROID_TARGET_ARCH = ENV['ANDROID_TARGET_ARCH'] || DEFAULT_ANDROID_TARGET_ARCH
  ANDROID_TOOLCHAIN = ENV['ANDROID_TOOLCHAIN'] || DEFAULT_ANDROID_TOOLCHAIN

  case ANDROID_TARGET_ARCH.downcase
  when 'arch-arm',  'arm'  then
    toolchain_prefix = 'arm-linux-androideabi-'
  when 'arch-x86',  'x86'  then
    toolchain_prefix = 'i686-linux-android-'
  when 'arch-mips', 'mips' then
    toolchain_prefix = 'mipsel-linux-android-'
  else
    # Any other architectures are not supported by Android NDK.
    # Notify error.
  end

  case ANDROID_TOOLCHAIN.downcase
  when 'gcc' then
    ANDROID_CC = ANDROID_STANDALONE_TOOLCHAIN + '/bin/' + toolchain_prefix + 'gcc'
    ANDROID_LD = ANDROID_STANDALONE_TOOLCHAIN + '/bin/' + toolchain_prefix + 'gcc'
    ANDROID_AR = ANDROID_STANDALONE_TOOLCHAIN + '/bin/' + toolchain_prefix + 'ar'
    ANDROID_CFLAGS  = GCC_COMMON_CFLAGS + %W(-mandroid --sysroot=#{SYSROOT})
    ANDROID_LDFLAGS = GCC_COMMON_LDFLAGS + %W(-mandroid --sysroot=#{SYSROOT})
  when 'clang' then
    # clang is not supported yet.
  when 'clang31', 'clang3.1' then
    # clang is not supported yet.
  else
    # Any other toolchains are not supported by Android NDK.
	# Notify error.
  end

  [conf.cc, conf.cxx, conf.objc, conf.asm].each do |cc|
    cc.command = ENV['CC'] || ANDROID_CC
    cc.flags = [ENV['CFLAGS'] || ANDROID_CFLAGS]
  end
  conf.linker.command = ENV['LD'] || ANDROID_LD
  conf.linker.flags = [ENV['LDFLAGS'] || ANDROID_LDFLAGS]
  conf.archiver.command = ENV['AR'] || ANDROID_AR
end
