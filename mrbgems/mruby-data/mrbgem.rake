MRuby::Gem::Specification.new('mruby-data') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Data class'

  # The tests reach the members and the string inspect builds through
  # ObjectSpace, the way a script can; they skip where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency 'mruby-objectspace', :core => 'mruby-objectspace'
  end
end
