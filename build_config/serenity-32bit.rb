MRuby::CrossBuild.new('serenity-32bit') do |conf|
  conf.toolchain :gcc

  conf.archiver.command = 'i686-pc-serenity-ar'
  conf.linker.command = 'i686-pc-serenity-g++'
  conf.cc.command = 'i686-pc-serenity-gcc'
  conf.cc.defines << 'MRB_32BIT'

  conf.gembox 'full-core'

  conf.test_runner.command = 'env'
end
