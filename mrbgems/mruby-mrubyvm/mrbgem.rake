MRuby::Gem::Specification.new 'mruby-mrubyvm' do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby implementation of `RubyVM`'

  add_dependency 'mruby-compiler'
end

