
MRuby::Build.new do |conf|
  # load specific toolchain settings

  # Gets set by the VS command prompts.
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    toolchain :visualcpp
  else
    toolchain :gcc
  end

  conf.gembox 'default'
  conf.gembox 'extras'
  conf.gembox 'carbuncle' if RUBY_PLATFORM =~ /darwin/
end

MRuby::CrossBuild.new('mingw') do |conf|
  toolchain :gcc

  conf.cc.command = 'x86_64-w64-mingw32-gcc'
  conf.linker.command = 'x86_64-w64-mingw32-gcc'
  conf.archiver.command = 'x86_64-w64-mingw32-ar'

  conf.linker.libraries += %w[wsock32 ws2_32]

  conf.exts.executable = '.exe'

  conf.gembox 'default'
  conf.gembox 'extras'
  conf.gembox 'carbuncle'
end

MRuby::CrossBuild.new('wasm') do |conf|
  toolchain :clang

  conf.cc do |cc|
    cc.command = 'emcc'
    cc.flags = [ENV['CFLAGS'] || %w()]
    cc.include_paths = ["#{root}/include"]
    cc.option_include_path = '-I%s'
    cc.option_define = '-D%s'
    cc.compile_options = "%{flags} -c %{infile} -s WASM=1 -o %{outfile}"
  end

  conf.archiver do |archiver| 
    archiver.command = 'emcc'
    archiver.archive_options = '%{objs} -s WASM=1 -o %{outfile}'
  end

  conf.exts do |exts|
    exts.object = '.bc'
    exts.executable = '' # '.exe' if Windows
    exts.library = '.bc'
  end

  conf.gembox 'wasm'
  conf.gembox 'extras'
  conf.gembox 'carbuncle'
end
