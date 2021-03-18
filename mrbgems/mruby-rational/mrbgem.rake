MRuby::Gem::Specification.new('mruby-rational') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Rational class'
  spec.build.cc.defines << "MRB_USE_RATIONAL"
end
