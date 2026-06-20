MRuby::Gem::Specification.new('mruby-eval-prism') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Kernel#eval method using Prism compiler'

  spec.add_conflict 'mruby-eval'
  add_dependency 'mruby-compiler-prism', core: 'mruby-compiler-prism'
  add_dependency 'mruby-binding', :core => 'mruby-binding'
  spec.add_test_dependency('mruby-metaprog', :core => 'mruby-metaprog')
  spec.add_test_dependency('mruby-method', :core => 'mruby-method')
end
