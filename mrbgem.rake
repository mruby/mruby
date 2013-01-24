MRuby::Gem::Specification.new('mruby-pack') do |spec|
  spec.license = 'MIT'
  spec.authors = 'mruby developers'

  spec.cc.flags << "#{build.root}/src"
end
