MRuby::Gem::Specification.new('mruby-objectspace-weakmap') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'ObjectSpace::WeakMap and ObjectSpace::WeakKeyMap'
  spec.build.defines << 'MRB_USE_GC_WEAK'
  spec.add_test_dependency 'mruby-objectspace', core: 'mruby-objectspace'
end
