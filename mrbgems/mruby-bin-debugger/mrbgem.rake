MRuby::Gem::Specification.new('mruby-bin-debugger') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby debugger command'
  spec.build.defines << "MRB_USE_DEBUG_HOOK"
  spec.add_dependency('mruby-eval', :core => 'mruby-eval')
  spec.add_test_dependency('mruby-bin-mrbc', :core => 'mruby-bin-mrbc')

  spec.bins = %w(mrdb)
end
