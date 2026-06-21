MRuby::Gem::Specification.new('mruby-eval-lrama') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Kernel#eval method (legacy lrama-based)'

  spec.add_conflict 'mruby-eval'
  add_dependency 'mruby-compiler-lrama', :core => 'mruby-compiler-lrama'
  add_dependency 'mruby-binding', :core => 'mruby-binding'
  spec.add_test_dependency('mruby-metaprog', :core => 'mruby-metaprog')
  spec.add_test_dependency('mruby-method', :core => 'mruby-method')
end
