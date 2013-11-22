MRuby::Build.new do |conf|
  toolchain :gcc

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.defines = %w(MRB_DEBUG MRB_GC_FIXED_ARENA)
end
