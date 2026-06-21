MRuby::Gem::Specification.new('mruby-bin-mirb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command using Prism compiler'
  spec.bins = %w(mirb)
  spec.add_dependency('mruby-compiler', core: 'mruby-compiler')
  spec.add_conflict 'mruby-bin-mirb-lrama'
end
