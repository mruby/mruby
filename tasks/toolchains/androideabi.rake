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

  ANDROID_STANDALONE_TOOLCHAIN = ENV['ANDROID_STANDALONE_TOOLCHAIN'] + '/bin/arm-linux-androideabi-'
  SYSROOT = ENV['ANDROID_STANDALONE_TOOLCHAIN'] + '/sysroot'

  [conf.cc, conf.cxx, conf.objc, conf.asm].each do |cc|
    cc.command = ENV['CC'] || ANDROID_STANDALONE_TOOLCHAIN + 'gcc'
    cc.flags = [ENV['CFLAGS'] || ['--sysroot ' + SYSROOT]]
  end
  conf.linker.command = ENV['LD'] || ANDROID_STANDALONE_TOOLCHAIN + 'gcc'
  conf.archiver.command = ENV['AR'] || ANDROID_STANDALONE_TOOLCHAIN + 'ar'
end
