MRuby::Gem::Specification.new('mruby-bin-mruby') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby command'
  spec.bins = %w(mruby)
  spec.add_dependency('mruby-compiler', :core => 'mruby-compiler')

  if build.cxx_exception_enabled?
    build.compile_as_cxx("#{spec.dir}/tools/mruby/mruby.c")
  end
end
