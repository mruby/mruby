MRuby::Toolchain.new(:emscripten) do |conf|
  toolchain :clang

  # See:
  # - https://emscripten.org/docs/tools_reference/emcc.html
  # - https://emscripten.org/docs/tools_reference/settings_reference.html
  # - https://github.com/emscripten-core/emscripten/blob/main/src/settings.js
  compile_and_link_flags = [
  ]
  compile_flags = [
    *compile_and_link_flags,
    '-Wno-unused-but-set-variable',
  ]
  link_flags = [
    *compile_and_link_flags,
  ]

  conf.cc do |cc|
    cc.command = 'emcc'
    cc.flags.concat(compile_flags) unless ENV['CFLAGS']
  end

  conf.cxx do |cxx|
    cxx.command = 'em++'
    cxx.flags.concat(compile_flags) unless ENV['CXXFLAGS'] || ENV['CFLAGS']
  end

  conf.linker do |linker|
    linker.command = 'emcc'
    linker.flags.concat(link_flags) unless ENV['LDFLAGS']
  end

  conf.archiver do |archiver|
    archiver.command = 'emar'
  end
end
