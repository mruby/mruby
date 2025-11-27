MRuby::Gem::Specification.new('hal-posix-socket') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'POSIX HAL for mruby-socket (Linux, macOS, BSD, Unix)'

  # HAL gem depends on feature gem - brings in mruby-socket automatically
  spec.add_dependency 'mruby-socket', core: 'mruby-socket'
end
