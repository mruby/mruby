MRuby::Gem::Specification.new('mruby-rational') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Rational class'

  spec.add_dependency 'mruby-metaprog', core: 'mruby-metaprog'
  spec.add_dependency 'mruby-object-ext', core: 'mruby-object-ext'
  spec.add_dependency 'mruby-numeric-ext', core: 'mruby-numeric-ext'
end
