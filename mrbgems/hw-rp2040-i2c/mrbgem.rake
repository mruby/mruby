MRuby::Gem::Specification.new('hw-rp2040-i2c') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'I2C HAL for RP2040'

  spec.add_dependency 'hw-i2c'
end
