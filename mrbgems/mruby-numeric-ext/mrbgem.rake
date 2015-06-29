MRuby::Gem::Specification.new('mruby-numeric-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'extensional Numeric class'
  spec.add_dependency 'mruby-math', :core => 'mruby-math'
end
