MRuby::Build.new do |conf|
  conf.toolchain

  conf.gembox 'lrama'

  conf.enable_bintest
  conf.enable_test
end
