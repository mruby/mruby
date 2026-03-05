MRuby::Gem::Specification.new('hal-posix-io') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'POSIX HAL for mruby-io (Linux, macOS, BSD, Unix)'

  # HAL gem depends on feature gem - brings in mruby-io automatically
  spec.add_dependency 'mruby-io', core: 'mruby-io'
end
