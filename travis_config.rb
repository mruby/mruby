MRuby::Build.new('debug') do |conf|
  toolchain :gcc
  enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines += %w(MRB_GC_FIXED_ARENA)
end

MRuby::Build.new do |conf|
  toolchain :gcc

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines += %w(MRB_GC_FIXED_ARENA)
end
