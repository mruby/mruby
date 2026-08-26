MRuby::Gem::Specification.new('mruby-complex') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Complex class'
  spec.build.defines << "MRB_USE_COMPLEX"
  spec.add_dependency 'mruby-math', core: 'mruby-math'

  # The exact-division path needs mruby-rational, but a test dependency on
  # it here would close a cycle: mruby-rational already test-depends on this
  # gem, and test dependencies sort with the regular ones. The tests for
  # that path live in mruby-rational's test directory instead, where both
  # gems are in the state.
end
