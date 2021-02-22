MRuby::Gem::Specification.new('mruby-binding') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Binding class'

  spec.add_dependency('mruby-eval', :core => 'mruby-eval')
end
