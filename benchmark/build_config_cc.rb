MRuby::Build.new do |conf|
  toolchain :gcc
end

MRuby::Build.new('gcc-O3-flto') do |conf|
  toolchain :gcc

  def self.gem(*args)
    super(*args) do |g|
      g.cc.flags << '-flto'
      g.linker.flags << '-flto -fuse-ld=gold'
    end
  end

  conf.gembox 'default'

  conf.archiver do |archiver|
    archiver.command = 'gcc-ar'
  end

  conf.cc.flags << '-flto'
  conf.linker.flags << '-flto -fuse-ld=gold'
end

MRuby::Build.new('clang-O3-flto') do |conf|
  toolchain :clang

  def self.gem(*args)
    super(*args) do |g|
      g.cc.flags << '-flto'
      g.linker.flags << '-flto -fuse-ld=gold'
    end
  end

  conf.gembox 'default'

  conf.archiver do |archiver|
    archiver.command = 'llvm-ar'
  end

  conf.cc.flags << '-flto'
  conf.linker.flags << '-v -flto -fuse-ld=gold'
end

MRuby::Build.new('gcc-O2') do |conf|
  toolchain :gcc
  conf.gembox 'default'

  conf.cc do |cc|
    cc.flags.each do |f|
      f.delete '-O3'
    end
    cc.flags << '-O2'
    p cc.flags
  end
end

MRuby::Build.new('clang-O2') do |conf|
  toolchain :clang
  conf.gembox 'default'

  conf.cc do |cc|
    cc.flags.each do |f|
      f.delete '-O3'
    end
    cc.flags << '-O2'
  end
end

MRuby::Build.new('gcc-O3') do |conf|
  toolchain :gcc
  conf.gembox 'default'
end

MRuby::Build.new('clang-O3') do |conf|
  toolchain :clang
  conf.gembox 'default'
end

MRuby::Build.new('gcc-O3-march-native') do |conf|
  toolchain :gcc
  conf.gembox 'default'

  conf.cc.flags << '-march=native'
end

MRuby::Build.new('clang-O3-march-native') do |conf|
  toolchain :clang
  conf.gembox 'default'

  conf.cc.flags << '-march=native'
end


  # conf.cc do |cc|
  #   cc.command = ENV['CC'] || 'gcc'
  #   cc.flags = [ENV['CFLAGS'] || %w()]
  #   cc.include_paths = ["#{root}/include"]
  #   cc.defines = %w(DISABLE_GEMS)
  #   cc.option_include_path = '-I%s'
  #   cc.option_define = '-D%s'
  #   cc.compile_options = "%{flags} -MMD -o %{outfile} -c %{infile}"
  # end

  # mrbc settings
  # conf.mrbc do |mrbc|
  #   mrbc.compile_options = "-g -B%{funcname} -o-" # The -g option is required for line numbers
  # end

  # Linker settings
  # conf.linker do |linker|
  #   linker.command = ENV['LD'] || 'gcc'
