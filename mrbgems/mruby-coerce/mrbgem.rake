MRuby::Gem::Specification.new('mruby-coerce') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'coerce protocol for Integer and Float operators'
  # Turns on the calls in src/numeric.c; without this gem they are not
  # compiled at all.
  spec.build.defines << "MRB_USE_COERCE"
end
