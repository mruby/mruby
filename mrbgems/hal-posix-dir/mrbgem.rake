MRuby::Gem::Specification.new('hal-posix-dir') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'
  spec.summary = 'POSIX HAL for mruby-dir (Linux, macOS, BSD, Unix)'

  spec.add_dependency 'mruby-dir', core: 'mruby-dir'
end
