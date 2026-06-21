MRuby::Gem::Specification.new('mruby-bin-mirb-lrama') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mirb command (legacy lrama-based)'
  spec.bins = %w(mirb)
  spec.add_dependency('mruby-compiler-lrama', :core => 'mruby-compiler-lrama')
  spec.add_conflict 'mruby-bin-mirb'
end
