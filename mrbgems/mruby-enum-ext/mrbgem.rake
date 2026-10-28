MRuby::Gem::Specification.new('mruby-enum-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Enumerable module extension'
  spec.add_test_dependency 'mruby-fiber', core: 'mruby-fiber'
  spec.add_test_dependency 'mruby-enumerator', core: 'mruby-enumerator'

  # A test reaches what Array#minmax builds through ObjectSpace, the way a
  # script can; it skips where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency 'mruby-objectspace', core: 'mruby-objectspace'
  end
end
