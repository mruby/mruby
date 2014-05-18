MRuby::Gem::Specification.new('mruby-eval') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Kernel#eval method'
  spec.add_dependency 'mruby-proc-ext', :core => 'mruby-proc-ext'
end
