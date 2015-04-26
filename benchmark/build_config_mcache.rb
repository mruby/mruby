MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('no_mcache') do |conf|
  toolchain :gcc

  conf.gembox 'default'
end

MRuby::Build.new('mcache1') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=1)
  end
end

MRuby::Build.new('mcache2') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=2)
  end
end

MRuby::Build.new('mcache3') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=3)
  end
end

MRuby::Build.new('mcache4') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=4)
  end
end

MRuby::Build.new('mcache5') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=5)
  end
end

MRuby::Build.new('mcache6') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
    c.defines += %w(MRB_METHOD_CACHE_SIZE=6)
  end
end
