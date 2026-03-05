MRuby::Gem::Specification.new('hal-win-io') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Windows HAL for mruby-io (Windows, MinGW)'

  # HAL gem depends on feature gem - brings in mruby-io automatically
  spec.add_dependency 'mruby-io', core: 'mruby-io'

  # Link Windows socket library
  spec.linker.libraries << "ws2_32"
end
