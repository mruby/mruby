MRuby::Gem::Specification.new('mruby-struct') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Struct class'

  # The tests reach the temporaries through ObjectSpace, and a Rational key's
  # #eql? into Struct#deconstruct_keys, the way a script can; they skip where
  # the build has neither.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency 'mruby-objectspace', :core => 'mruby-objectspace'
  end
  if build.gems.any? {|g| g.name == 'mruby-rational'}
    spec.add_test_dependency 'mruby-rational', :core => 'mruby-rational'
  end
end
