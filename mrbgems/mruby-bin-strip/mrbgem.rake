MRuby::Gem::Specification.new('mruby-bin-strip') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'irep dump debug section remover command'
  spec.bins = %w(mruby-strip)

  add_dependency 'mruby-mrubyvm'
  add_dependency 'mruby-exit'
  add_dependency 'mruby-io'
  add_dependency 'mruby-array-ext'
end
