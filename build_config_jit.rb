MRuby::Build.new do |conf|
  toolchain :clang
  conf.gembox 'default'

  compilers.each do |c|
    #c.defines += %w(MRB_NAN_BOXING)
  end
end

MRuby::Build.new('jit') do |conf|
  toolchain :clang
  conf.gembox 'default'

  compilers.each do |c|
    c.defines += %w(MRB_ENABLE_JIT)
    #c.defines += %w(MRB_NAN_BOXING)
  end
end

