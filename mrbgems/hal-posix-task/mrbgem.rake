MRuby::Gem::Specification.new('hal-posix-task') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'POSIX HAL for mruby-task (Linux, macOS, BSD, Unix)'

  # HAL gem depends on feature gem - brings in mruby-task automatically
  spec.add_dependency 'mruby-task', core: 'mruby-task'
end
