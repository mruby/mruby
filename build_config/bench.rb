MRuby::Build.new('bench') do |conf|
  # Gets set by the VS command prompts.
  if ENV['VisualStudioVersion'] || ENV['VSINSTALLDIR']
    toolchain :visualcpp
  else
    toolchain :gcc
    conf.cc.flags << '-O3'
  end

  conf.gembox 'default'

  # The compile_commands.json at the source root speaks for this build
  conf.enable_compile_commands default: true
end
