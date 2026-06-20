MRuby::Build.new do |conf|
  conf.toolchain

  conf.gembox 'prism'

  conf.enable_bintest
  conf.enable_test
end
