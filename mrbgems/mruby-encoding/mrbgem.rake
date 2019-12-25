MRuby::Gem::Specification.new('mruby-encoding') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Encoding subset for mruby'

  spec.cc.defines << 'MRB_UTF8_STRING=1'

  build.cc.defines << 'MRB_UTF8_STRING=1'
  build.cxx.defines << 'MRB_UTF8_STRING=1'
end
