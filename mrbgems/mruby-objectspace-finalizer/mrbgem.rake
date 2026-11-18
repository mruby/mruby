MRuby::Gem::Specification.new('mruby-objectspace-finalizer') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'ObjectSpace.define_finalizer and undefine_finalizer'
  spec.build.defines << 'MRB_USE_GC_WEAK' unless spec.build.defines.include?('MRB_USE_GC_WEAK')
end
