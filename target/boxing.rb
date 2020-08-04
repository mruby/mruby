MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('no_boxing') do |conf|
  toolchain :gcc

  conf.compilers.each do |c|
    c.defines += %w(MRB_NO_BOXING)
  end
  conf.gembox 'default'
  conf.enable_test
end

MRuby::Build.new('word_boxing') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_WORD_BOXING)
  end
  conf.enable_test
end

MRuby::Build.new('nan_boxing') do |conf|
  toolchain :gcc

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NAN_BOXING)
  end
  conf.enable_test
end

