MRuby::Gem::Specification.new('mruby-hash-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Hash class extension'
  spec.add_dependency 'mruby-array-ext', core: 'mruby-array-ext'
  spec.add_test_dependency 'mruby-fiber', core: 'mruby-fiber'

  # A test reaches what the methods build through ObjectSpace, the way a
  # script can; it skips where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency 'mruby-objectspace', core: 'mruby-objectspace'
  end
end
