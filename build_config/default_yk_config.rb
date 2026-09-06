MRuby::Build.new('default_yk_config') do |conf|
  conf.toolchain

  plain_mrbc = "#{MRUBY_ROOT}/build/host/mrbc/bin/mrbc"
  fail "#{plain_mrbc} not found - run a plain `rake` first" unless File.exist?(plain_mrbc)
  conf.mrbcfile = plain_mrbc

  conf.cc.defines << 'MRB_NO_DIRECT_THREADING'
  conf.cc.defines << 'MRB_NO_BOXING'
  conf.cc.defines << 'MRB_INT64'

  conf.gembox 'stdlib'
  conf.gembox 'stdlib-ext'
  conf.gembox 'stdlib-io'
  conf.gembox 'math'
  conf.gembox 'metaprog'
  conf.gem core: 'mruby-bin-mruby'

  conf.enable_bintest
  conf.enable_test
end
