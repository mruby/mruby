MRuby::Gem::Specification.new('mruby-rational') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Rational class'
  spec.build.defines << "MRB_USE_RATIONAL"
  # Only where the build has a Float: mruby-complex refuses to build without
  # one, and asking for it unconditionally is what kept this gem's tests from
  # being built at all in an MRB_NO_FLOAT build.
  unless cc.defines.include?('MRB_NO_FLOAT')
    spec.add_test_dependency('mruby-complex', :core => 'mruby-complex')
  end
end
