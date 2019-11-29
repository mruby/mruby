
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
