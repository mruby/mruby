
MRuby::Build.new do |conf|
  # load specific toolchain settings

  # Gets set by the VS command prompts.
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    toolchain :visualcpp
  elsif RUBY_PLATFORM =~ /darwin/
    toolchain :clang
  else
    toolchain :gcc
  end

  conf.gembox 'default'
  conf.gembox 'extras'
  conf.gembox 'carbuncle' if RUBY_PLATFORM =~ /darwin/
  enable_debug if ENV.fetch('ENABLE_DEBUG', false)
end

if ENV.fetch('BUILD_MINGW', false)
  MRuby::CrossBuild.new('win32') do |conf|
    toolchain :gcc

    conf.cc.command = 'x86_64-w64-mingw32-gcc'
    conf.cxx.command = 'x86_64-w64-mingw32-g++'
    conf.linker.command = 'x86_64-w64-mingw32-gcc'
    conf.archiver.command = 'x86_64-w64-mingw32-ar'

    conf.linker.libraries += %w[wsock32 ws2_32]

    conf.exts.executable = '.exe'

    conf.cc.flags << '-DPCRE_STATIC'

    conf.gembox 'default'
    conf.gembox 'extras'
    conf.gembox 'carbuncle'

    enable_debug if ENV.fetch('ENABLE_DEBUG', false)
  end
end

if ENV.fetch('BUILD_WEB', false)
  MRuby::CrossBuild.new('emscripten') do |conf|
    toolchain :clang
    conf.cc.command = 'emcc'
    conf.cc.flags = %w[-Os]
    conf.linker.command = 'emcc'
    conf.archiver.command = 'emar'

    conf.gembox 'wasm'
    conf.gembox 'extras'
    conf.gembox 'carbuncle'
  end
end

if ENV.fetch('BUILD_IOS', false) && RUBY_PLATFORM ~= /darwin/
  MRuby::CrossBuild.new('ios') do |conf|
    toolchain :clang

    sdk_path = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS9.2.sdk'
    arch = '-arch arm64 -arch armv7 -arch armv7s'

    conf.cc.flags << arch
    conf.cc.include_paths << "#{sdk_path}/usr/include"
    conf.linker.flags << arch
    conf.linker.library_paths << "#{sdk_path}/usr/lib"

    conf.bins = []

    conf.gembox 'default'
    conf.gembox 'extras'
    conf.gembox 'carbuncle'
  end
end
