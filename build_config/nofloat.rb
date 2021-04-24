MRuby::Build.new do |conf|
  # load specific toolchain settings
  toolchain :gcc

  # include the GEM box
  conf.gembox "stdlib"
  conf.gembox "stdlib-ext"
  conf.gembox "stdlib-io"
  conf.gembox "metaprog"

  conf.gem :core => 'mruby-bin-mruby'
  conf.gem :core => 'mruby-bin-mirb'

  conf.cc.defines << 'MRB_NO_FLOAT'

  # Turn on `enable_debug` for better debugging
  conf.enable_debug
  conf.enable_test
  conf.enable_bintest
end
