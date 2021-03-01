MRuby::CrossBuild.new('Minimal') do |conf|
  toolchain :gcc
  conf.cc.defines = %w(DISABLE_STDIO)
  conf.bins = []  
end
