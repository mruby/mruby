MRuby::Gem::Specification.new('mruby-eval') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Kernel#eval method'

  add_dependency 'mruby-compiler', :core => 'mruby-compiler'
  add_dependency 'mruby-binding', :core => 'mruby-binding'
end
