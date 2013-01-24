MRuby::Gem::Specification.new('mruby-pack') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'

  spec.cc.incude_paths << "#{build.root}/src"
end
