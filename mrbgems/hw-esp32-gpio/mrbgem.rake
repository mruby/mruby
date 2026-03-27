MRuby::Gem::Specification.new('hw-esp32-gpio') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'GPIO HAL for ESP32'

  spec.add_dependency 'hw-gpio'
end
