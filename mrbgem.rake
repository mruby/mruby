MRuby::Gem::Specification.new('mruby-dir') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'

  spec.cc.include_paths << "#{build.root}/src"
end
