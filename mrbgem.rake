MRuby::Gem::Specification.new('mruby-socket') do |spec|
  spec.license = 'MIT'
  spec.authors = 'Internet Initiative Japan'

  spec.cc.include_paths << "#{build.root}/src"

  spec.add_dependency('mruby-io')
  spec.add_dependency('mruby-pack')
  # spec.add_dependency('mruby-mtest')
end
