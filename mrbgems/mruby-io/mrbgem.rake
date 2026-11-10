MRuby::Gem::Specification.new('mruby-io') do |spec|
  spec.license = 'MIT'
  spec.authors = ['Internet Initiative Japan Inc.', 'mruby developers']
  spec.summary = 'IO and File class'

  spec.build.defines << "HAVE_MRUBY_IO_GEM"
  spec.add_test_dependency 'mruby-time', core: 'mruby-time'
  spec.add_test_dependency 'mruby-errno', core: 'mruby-errno'
  spec.add_test_dependency 'mruby-kernel-ext', core: 'mruby-kernel-ext'

  if spec.for_windows?
    spec.linker.libraries << "ws2_32"
  end
end
