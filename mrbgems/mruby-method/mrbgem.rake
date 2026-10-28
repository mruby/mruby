MRuby::Gem::Specification.new('mruby-method') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Method and UnboundMethod class'

  spec.add_dependency('mruby-proc-ext', :core => 'mruby-proc-ext')

  # A test reaches the string Method#inspect builds through ObjectSpace, the
  # way a script can; it skips where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency('mruby-objectspace', :core => 'mruby-objectspace')
  end
end
