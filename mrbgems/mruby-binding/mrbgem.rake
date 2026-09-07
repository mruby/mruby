MRuby::Gem::Specification.new('mruby-binding') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Binding class (core features only)'

  spec.add_test_dependency('mruby-proc-ext', :core => 'mruby-proc-ext')
  spec.add_test_dependency('mruby-metaprog', :core => 'mruby-metaprog')
end
