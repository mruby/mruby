MRuby::Build.new do |conf|

  # load specific toolchain settings
  toolchain :gcc
    conf.cc do |cc|
    cc.flags = '-g -O2 -fstack-protector --param=ssp-buffer-size=4 -Wformat -Werror=format-security'
  end
  # Linker settings
  conf.linker do |linker|
     linker.flags = '-Wl,-z,relro'
  end

end
