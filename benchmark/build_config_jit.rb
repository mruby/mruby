MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('interp') do |conf|
  toolchain :clang

  conf.gembox 'default'
end

MRuby::Build.new('jit') do |conf|
  toolchain :clang

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_JIT)
  end
end


