MRuby::Gem::Specification.new('mruby-objectspace') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'ObjectSpace class'

  spec.add_test_dependency('mruby-metaprog')
  spec.add_test_dependency('mruby-method')
  spec.add_test_dependency('mruby-fiber')
end
