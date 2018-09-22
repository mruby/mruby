MRuby::Gem::Specification.new('mruby-complex') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Complex class'

  spec.add_dependency 'mruby-metaprog', core: 'mruby-metaprog'
  spec.add_dependency 'mruby-object-ext', core: 'mruby-object-ext'
  spec.add_dependency 'mruby-numeric-ext', core: 'mruby-numeric-ext'
  spec.add_dependency 'mruby-math', core: 'mruby-math'
end
