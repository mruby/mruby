MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('no_mcache') do |conf|
  toolchain :clang

  conf.gembox 'default'
end

MRuby::Build.new('mcache_monomorphic') do |conf|
  toolchain :clang

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_MORPHICITY=1)
  end
end

MRuby::Build.new('mcache_dimorphic') do |conf|
  toolchain :clang

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_MORPHICITY=2)
  end
end

MRuby::Build.new('mcache_trimorphic') do |conf|
  toolchain :clang

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_MORPHICITY=3)
  end
end

MRuby::Build.new('mcache_tetramorphic') do |conf|
  toolchain :clang

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_MORPHICITY=4)
  end
end
