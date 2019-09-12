MRuby::Toolchain.new(:clang) do |conf, _params|
  toolchain :gcc, default_command: 'clang'

  [conf.cc, conf.objc, conf.asm, conf.cxx].each do |compiler|
    compiler.flags.unshift('-Wzero-length-array')
  end
end
