MRuby::Gem::Specification.new('mruby-math') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard Math module'
  spec.add_test_dependency('mruby-fiber', :core => 'mruby-fiber')
end
