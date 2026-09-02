MRuby::Gem::Specification.new('mruby-benchmark') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'benchmarking and profiling tools'

  # `Benchmark.measure` is built the way CRuby's is: CPU time from
  # `Process.times`, real time from `Process.clock_gettime` on the
  # monotonic clock.
  spec.add_dependency('mruby-process', :core => 'mruby-process')
  spec.add_dependency('mruby-objectspace', :core => 'mruby-objectspace')
  spec.add_dependency('mruby-sprintf', :core => 'mruby-sprintf')
  spec.add_dependency('mruby-io', :core => 'mruby-io')
end
