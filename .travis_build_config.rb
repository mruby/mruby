MRuby::Build.new do |conf|
  toolchain :gcc
  conf.gembox 'default'
  conf.gem '../mruby-set'
end
