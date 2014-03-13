MRuby::Gem::Specification.new('mruby-debug-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'

  spec.cc.include_paths << MRUBY_ROOT

  spec.build.compilers.each do |cc|
    cc.defines << 'MRB_DEBUG_DUMP_FUNCTIONS'
  end
end
