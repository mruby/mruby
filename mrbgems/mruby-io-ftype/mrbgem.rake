MRuby::Gem::Specification.new('mruby-io-ftype') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Akira Matsuda', 'mruby developers']
  spec.summary = 'File.ftype and the rest of the FileTest predicates'

  spec.add_dependency 'mruby-io', core: 'mruby-io'
end
