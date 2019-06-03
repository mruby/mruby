MRuby::Build.new do |conf|
  # load specific toolchain settings

  # Gets set by the VS command prompts.
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    toolchain :visualcpp
  else
    toolchain :gcc
  end

  # include the default GEMs
  conf.gembox 'default'
end


# Define cross build settings
MRuby::CrossBuild.new('emcc-wasm') do |conf|
  toolchain :gcc

  conf.cc.command = 'emcc'
  #conf.cc.flags << "-m32"
  conf.linker.command = 'emcc'
  #conf.linker.flags << "-m32"
  conf.archiver.command = 'emcc'
  conf.archiver.archive_options = '-o %{outfile} %{objs}'

  conf.exts do |exts|
    exts.object = '.o'
    exts.executable = ''
    exts.library = '.bc' # It's LLVM bit code
  end

  conf.build_mrbtest_lib_only

  conf.gembox 'default'
end

