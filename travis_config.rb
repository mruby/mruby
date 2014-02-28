MRuby::Build.new('debug') do |conf|
  toolchain :gcc
  enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.flags += %w(-Werror=declaration-after-statement)
  conf.enable_mrbconf :gc_fixed_arena
end

MRuby::Build.new do |conf|
  toolchain :gcc

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.flags += %w(-Werror=declaration-after-statement)
  conf.enable_mrbconf :debug, :gc_fixed_arena
  conf.enable_bintest = true
end
