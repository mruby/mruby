MRuby::Gem::Specification.new('mruby-encoding') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = "Poorman's Encoding for mruby"
  spec.build.defines << "HAVE_MRUBY_ENCODING_GEM"
  spec.build.defines << "MRB_UTF8_STRING"
  spec.add_test_dependency 'mruby-string-ext'
  # The tests ask String for the methods that change a receiver, so that a
  # method the build carries and the coderange checklist does not name fails
  # the suite rather than going unasked.
  spec.add_test_dependency 'mruby-metaprog'
  # String#bitwise_*! write arbitrary bytes over a receiver, so they belong in
  # that checklist; without the gem the tests would ask a method that is not
  # there and pass on the refusal.
  spec.add_test_dependency 'mruby-string-bitops'
end
