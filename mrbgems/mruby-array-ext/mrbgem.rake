MRuby::Gem::Specification.new('mruby-array-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Array class extension'

  # intersection.rb reaches the result array from an element's #hash through
  # ObjectSpace.each_object; the overflow it guards against is otherwise
  # unreachable from plain Ruby.
  spec.add_test_dependency 'mruby-objectspace', core: 'mruby-objectspace'
end
