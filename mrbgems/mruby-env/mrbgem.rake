MRuby::Gem::Specification.new('mruby-env') do |spec|
  spec.license = 'MIT'
  spec.authors = ['mruby developers']
  spec.summary = 'ENV object for environment variable access'
  spec.add_dependency('mruby-enumerator', :core => 'mruby-enumerator')
end
