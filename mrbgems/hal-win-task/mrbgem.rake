MRuby::Gem::Specification.new('hal-win-task') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'Windows HAL for mruby-task'

  # HAL gem depends on feature gem - brings in mruby-task automatically
  spec.add_dependency 'mruby-task', core: 'mruby-task'

  # Windows multimedia timer library
  spec.linker.libraries << 'winmm'
end
