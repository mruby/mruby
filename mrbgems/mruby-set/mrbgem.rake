MRuby::Gem::Specification.new('mruby-set') do |spec|
  spec.license = 'MIT'
  spec.authors = 'yui-knk'
  spec.summary = 'Set class'
  spec.build.defines << "MRB_USE_SET"

  spec.add_dependency "mruby-hash-ext", :core => "mruby-hash-ext"
  spec.add_dependency "mruby-enumerator", :core => "mruby-enumerator"

  # A test reaches the string inspect and join build through ObjectSpace, the
  # way a script can; it skips where the build has none.
  if build.gems.any? {|g| g.name == 'mruby-objectspace'}
    spec.add_test_dependency "mruby-objectspace", :core => "mruby-objectspace"
  end
end
