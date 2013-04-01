MRuby::Gem::Specification.new('mruby-io') do |spec|
  spec.license = 'MIT'
  spec.authors = 'Internet Initiative Japan'

  spec.cc.include_paths << "#{build.root}/src"
end
