MRuby::Gem::Specification.new('mruby-warning') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Akira Matsuda', 'mruby developers']
  spec.summary = 'Warning module: warning categories and Warning.warn'

  # Warning.warn writes to $stderr, and Kernel#warn goes through Warning
  spec.add_dependency 'mruby-io', core: 'mruby-io'
  spec.add_test_dependency 'mruby-metaprog', core: 'mruby-metaprog'
end
