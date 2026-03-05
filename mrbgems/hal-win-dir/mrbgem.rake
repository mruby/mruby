MRuby::Gem::Specification.new('hal-win-dir') do |spec|
  spec.license = 'MIT and MIT-like license'
  spec.authors = ['mruby developers', 'Kevlin Henney']
  spec.summary = 'Windows HAL for mruby-dir'

  spec.add_dependency 'mruby-dir', core: 'mruby-dir'
end
