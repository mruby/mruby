MRuby::Gem::Specification.new('mruby-print') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'standard print/puts/p'
  spec.add_dependency 'mruby-sprintf', :weak => true
end
