MRuby::Gem::Specification.new('mruby-regexp') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Regexp class (built-in NFA engine)'

  spec.add_dependency 'mruby-string-ext', :core => 'mruby-string-ext'
  # String#gsub without a block returns an enumerator; the test for that
  # needs Enumerator (and thus Fiber).
  spec.add_test_dependency 'mruby-enumerator', :core => 'mruby-enumerator'
end
