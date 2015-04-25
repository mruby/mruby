MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('no_mcache') do |conf|
  toolchain :gcc

  conf.gembox 'default'
end

MRuby::Build.new('mcache') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_ENABLE_METHOD_CACHE)
  end
end
