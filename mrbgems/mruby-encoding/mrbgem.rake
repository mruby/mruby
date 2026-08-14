MRuby::Gem::Specification.new('mruby-encoding') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = "Poorman's Encoding for mruby"
  spec.build.defines << "HAVE_MRUBY_ENCODING_GEM"
  spec.add_test_dependency 'mruby-string-ext'
end
