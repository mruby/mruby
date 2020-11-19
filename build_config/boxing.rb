MRuby::Build.new('boxing-no-m64-i64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m64'
  conf.linker.flags << '-m64'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NO_BOXING MRB_INT64)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-no-m64-i32') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m64'
  conf.linker.flags << '-m64'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NO_BOXING MRB_INT32)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-no-m32-i64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'

  # Turn on `enable_debug` for better debugging
  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NO_BOXING MRB_INT64)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-no-m32-i32') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'

  # Turn on `enable_debug` for better debugging
  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NO_BOXING MRB_INT32)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-word-m64-i64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m64'
  conf.linker.flags << '-m64'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_WORD_BOXING MRB_INT64)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-word-m64-i32') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m64'
  conf.linker.flags << '-m64'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_WORD_BOXING MRB_INT32)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-word-m32-i64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_WORD_BOXING MRB_INT64)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-word-m32-i32') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_WORD_BOXING MRB_INT32)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-nan-m64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m64'
  conf.linker.flags << '-m64'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NAN_BOXING)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end

MRuby::Build.new('boxing-nan-m64') do |conf|
  toolchain :gcc

  conf.cc.flags << '-m32'
  conf.linker.flags << '-m32'

  conf.gembox 'default'
  conf.compilers.each do |c|
    c.defines += %w(MRB_NAN_BOXING MRB_INT32)
  end
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end
