MRuby::Gem::Specification.new('mruby-encoding-transcode') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Akira Matsuda', 'mruby developers']
  spec.summary = 'String#encode, the encoding errors and Encoding.default_external'

  spec.add_dependency 'mruby-encoding', core: 'mruby-encoding'
  # the tests pass a Method as fallback:
  spec.add_test_dependency 'mruby-method', core: 'mruby-method'
end
