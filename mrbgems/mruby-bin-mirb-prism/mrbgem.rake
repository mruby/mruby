MRuby::Gem::Specification.new('mruby-bin-mirb-prism') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command using Prism compiler'
  spec.bins = %w(mirb-prism)
  spec.add_dependency('mruby-compiler-prism', core: 'mruby-compiler-prism')
end
