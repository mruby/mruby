MRuby::Gem::Specification.new('mruby-bin-mruby-lrama') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby command (legacy lrama-based)'
  spec.bins = %w(mruby)
  spec.add_dependency('mruby-compiler-lrama', :core => 'mruby-compiler-lrama')
  spec.add_conflict 'mruby-bin-mruby'
end
