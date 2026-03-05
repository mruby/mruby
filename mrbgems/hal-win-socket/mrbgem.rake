MRuby::Gem::Specification.new('hal-win-socket') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Windows HAL for mruby-socket (Windows, MinGW)'

  # HAL gem depends on feature gem - brings in mruby-socket automatically
  spec.add_dependency 'mruby-socket', core: 'mruby-socket'

  # Link Windows socket libraries
  spec.linker.libraries << "wsock32"
  spec.linker.libraries << "ws2_32"
end
