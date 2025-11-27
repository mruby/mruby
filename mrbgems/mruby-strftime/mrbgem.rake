MRuby::Gem::Specification.new('mruby-strftime') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Time#strftime implementation'

  spec.add_dependency 'mruby-time'
end
