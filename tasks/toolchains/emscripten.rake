MRuby::Toolchain.new(:emscripten) do |conf|
  toolchain :clang

  # See:
  # - https://emscripten.org/docs/tools_reference/emcc.html
  # - https://emscripten.org/docs/tools_reference/settings_reference.html
  # - https://github.com/emscripten-core/emscripten/blob/main/src/settings.js
  #
  # == WASM Exception Handling ==
  #
  # mruby uses setjmp/longjmp for non-local exits (exceptions, break/return
  # from blocks). This toolchain uses native WASM exception handling which
  # is more efficient than Asyncify-based emulation:
  #
  #   - Minimal memory overhead (no shadow stack)
  #   - No code size penalty
  #   - Works with both C and C++ code
  #
  # Supported runtimes (as of 2024):
  #   - Chrome 95+, Firefox 100+, Safari 15.2+
  #   - Node.js 17+, Wasmtime, Wasmer
  #
  # For older runtimes, override with CFLAGS/LDFLAGS environment variables.
  #
  # See: https://emscripten.org/docs/porting/exceptions.html
  #      https://emscripten.org/docs/porting/setjmp-longjmp.html
  compile_and_link_flags = [
    '-fwasm-exceptions',
  ]
  compile_flags = [
    *compile_and_link_flags,
    '-Wno-unused-but-set-variable',
  ]
  link_flags = [
    *compile_and_link_flags,
    '-sSUPPORT_LONGJMP=wasm',
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
