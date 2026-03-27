MRuby::Gem::Specification.new('hw-rp2040-gpio') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'GPIO HAL for RP2040'

  spec.add_dependency 'hw-gpio'
end
