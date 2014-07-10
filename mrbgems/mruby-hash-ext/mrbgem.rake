MRuby::Gem::Specification.new('mruby-hash-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'extensional Hash class'
  spec.add_dependency 'mruby-enum-ext', :core => 'mruby-enum-ext'
  spec.add_dependency 'mruby-array-ext', :core => 'mruby-array-ext'
end
