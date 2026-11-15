MRuby::Gem::Specification.new('mruby-random-urandom') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Akira Matsuda', 'mruby developers']
  spec.summary = 'Random.urandom: random bytes from the platform'

  spec.add_dependency 'mruby-random', core: 'mruby-random'

  if spec.for_windows?
    spec.linker.libraries << "bcrypt"
  end
end
