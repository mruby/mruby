MRuby::Gem::Specification.new('mruby-benchmark') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'benchmarking and profiling tools'

  spec.add_dependency('mruby-time', :core => 'mruby-time')
  spec.add_dependency('mruby-objectspace', :core => 'mruby-objectspace')
end
