MRuby::Gem::Specification.new('mruby-enum-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Enumerable module extension'
  spec.add_test_dependency 'mruby-fiber', core: 'mruby-fiber'
  spec.add_test_dependency 'mruby-enumerator', core: 'mruby-enumerator'
end
