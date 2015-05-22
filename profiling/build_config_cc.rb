MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('prof_gcc') do |conf|
  toolchain :gcc

  conf.compilers.each do |c|
    c.defines += %w(MRB_VM_NO_INLINE)
  end

  conf.gembox 'default'
end

MRuby::Build.new('prof_clang') do |conf|
  toolchain :clang

  conf.compilers.each do |c|
    c.defines += %w(MRB_VM_NO_INLINE)
  end

  conf.gembox 'default'
end


