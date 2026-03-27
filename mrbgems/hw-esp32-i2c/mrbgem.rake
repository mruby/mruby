MRuby::Gem::Specification.new('hw-esp32-i2c') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'I2C HAL for ESP32'

  spec.add_dependency 'hw-i2c'
end
