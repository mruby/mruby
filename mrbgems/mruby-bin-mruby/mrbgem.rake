MRuby::Gem::Specification.new('mruby-bin-mruby') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby command using Prism compiler'
  spec.bins = %w(mruby)
  spec.add_dependency('mruby-compiler', core: 'mruby-compiler')
  spec.add_conflict 'mruby-bin-mruby-lrama'
end
