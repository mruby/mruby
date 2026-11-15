MRuby::Gem::Specification.new('mruby-random-formatter') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Random::Formatter with random_number and rand'
  spec.add_dependency 'mruby-random', core: 'mruby-random'
end
