MRuby::Gem::Specification.new('mruby-set') do |spec|
  spec.license = 'MIT'
  spec.authors = 'yui-knk'
  spec.build.defines << "MRB_USE_SET"

  spec.add_dependency "mruby-hash-ext", :core => "mruby-hash-ext"
  spec.add_dependency "mruby-enumerator", :core => "mruby-enumerator"
end
