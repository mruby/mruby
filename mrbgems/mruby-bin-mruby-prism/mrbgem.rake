MRuby::Gem::Specification.new('mruby-bin-mruby-prism') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby command using Prism compiler'
  spec.bins = %w(mruby)
  spec.add_dependency('mruby-compiler-prism', core: 'mruby-compiler-prism')
end
