MRuby::Gem::Specification.new('mruby-dir') do |spec|
  spec.license = 'MIT and MIT-like license'
  spec.authors = ['Internet Initiative Japan Inc.', 'Kevlin Henney']

  # Dir.glob matches with File.fnmatch and asks File what a name is
  spec.add_dependency 'mruby-io', core: 'mruby-io'
end
