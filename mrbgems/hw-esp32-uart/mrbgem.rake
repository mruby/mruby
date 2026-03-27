MRuby::Gem::Specification.new('hw-esp32-uart') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'UART HAL for ESP32'

  spec.add_dependency 'hw-uart'
end
