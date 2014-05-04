class MRuby::Build
  def travis_toolchain_selection
    if ENV['CC'] == 'clang'
      toolchain :clang
      sanitizers = %w(-fsanitize=address,undefined -fno-sanitize=alignment,float-divide-by-zero)
      cc.flags += sanitizers
      linker.flags += sanitizers
    else
      toolchain :gcc
    end
  end
end

MRuby::Build.new('debug') do |conf|
  travis_toolchain_selection

  enable_debug

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.flags += %w(-Werror=declaration-after-statement)
  conf.compilers.each do |c|
    c.defines += %w(MRB_GC_STRESS MRB_GC_FIXED_ARENA)
  end
end

MRuby::Build.new do |conf|
  travis_toolchain_selection

  # include all core GEMs
  conf.gembox 'full-core'
  conf.cc.flags += %w(-Werror=declaration-after-statement)
  conf.compilers.each do |c|
    c.defines += %w(MRB_GC_FIXED_ARENA)
  end
  conf.enable_bintest
end

MRuby::Build.new('cxx_abi') do |conf|
  travis_toolchain_selection

  conf.gembox 'full-core'
  conf.cc.flags += %w(-Werror=declaration-after-statement)
  conf.compilers.each do |c|
    c.defines += %w(MRB_GC_FIXED_ARENA)
  end
  conf.enable_bintest

  enable_cxx_abi
end
