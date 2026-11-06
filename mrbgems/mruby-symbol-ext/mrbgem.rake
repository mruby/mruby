MRuby::Gem::Specification.new('mruby-symbol-ext') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Symbol class extension'
  # start_with? and end_with? answer through String's
  spec.add_dependency('mruby-string-ext', :core => 'mruby-string-ext')
end
