# Cross compiling configuration for SerenityOS
# Graphical Unix-like operating system for x86 computers.
# https://github.com/SerenityOS/serenity
#
# Should be built using the SerenityOS Ports system
# https://github.com/SerenityOS/serenity/tree/master/Ports

MRuby::CrossBuild.new('serenity') do |conf|
  conf.toolchain :gcc

  conf.archiver.command = "#{ENV['SERENITY_ARCH']}-pc-serenity-ar"
  conf.linker.command = "#{ENV['SERENITY_ARCH']}-pc-serenity-g++"

  conf.cxx.command = "#{ENV['SERENITY_ARCH']}-pc-serenity-g++"
  conf.cxx.defines << (ENV['SERENITY_ARCH'].include?('64') ? 'MRB_64BIT' : 'MRB_32BIT')

  conf.cc.command = "#{ENV['SERENITY_ARCH']}-pc-serenity-gcc"
  conf.cc.defines << (ENV['SERENITY_ARCH'].include?('64') ? 'MRB_64BIT' : 'MRB_32BIT')

  conf.gembox 'full-core'

  conf.test_runner.command = 'env'
end
