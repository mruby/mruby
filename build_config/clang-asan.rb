MRuby::Build.new do |conf|
  toolchain :clang
  # include the GEM box
  conf.gembox 'default'

  # C compiler settings
  conf.cc do |cc|
    cc.flags << "-fsanitize=address,undefined"
    cc.include_paths = ["#{root}/include"]
  end

  # Linker settings
  conf.linker do |linker|
    linker.flags << "-fsanitize=address,undefined"
  end

  # Turn on `enable_debug` for better debugging
  enable_debug
  conf.enable_bintest
  conf.enable_test
end
