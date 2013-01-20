MRuby::Toolchain.new(:clang) do |conf|
  toolchain :gcc

  conf.cc.command = ENV['CC'] || 'clang'
  conf.linker.command = ENV['LD'] || 'clang'
end
