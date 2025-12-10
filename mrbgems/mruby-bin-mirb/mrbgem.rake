MRuby::Gem::Specification.new('mruby-bin-mirb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command'
  spec.bins = %w(mirb)
  spec.add_dependency('mruby-compiler', :core => 'mruby-compiler')
end
