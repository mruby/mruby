MRuby::Gem::Specification.new('mruby-bigint') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Integer class extension to multiple-precision'
  spec.build.defines << "MRB_USE_BIGINT"
end
