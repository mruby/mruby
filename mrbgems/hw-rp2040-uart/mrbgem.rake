MRuby::Gem::Specification.new('hw-rp2040-uart') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'UART HAL for RP2040'

  spec.add_dependency 'hw-uart'
end
