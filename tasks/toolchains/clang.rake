MRuby::Toolchain.new(:clang) do |conf, _params|
  toolchain :gcc

  [conf.cc, conf.objc, conf.asm].each do |cc|
    cc.command = ENV['CC'] || 'clang'
    cc.flags << '-Wzero-length-array' unless ENV['CFLAGS']
  end
  conf.cxx.command = ENV['CXX'] || 'clang++'
  conf.cxx.flags << '-Wzero-length-array' unless ENV['CXXFLAGS'] || ENV['CFLAGS']
  conf.linker.command = ENV['LD'] || ENV['CXX'] || ENV['CC'] || 'clang'
end
