MRuby::Gem::Specification.new('mruby-bin-carbuncle') do |spec|
  spec.license = 'MIT'
  spec.author  = 'Ramiro Rojo'
  spec.summary = 'Carbuncle application'

  spec.bins = %w[mruby-carbuncle]

  spec.add_dependency 'carbuncle-core'
  spec.add_dependency 'carbuncle-audio'
  spec.add_dependency 'carbuncle-graphics'
  spec.add_dependency 'carbuncle-input'
  spec.add_dependency 'carbuncle-math'

  spec.add_dependency('mruby-compiler', core: 'mruby-compiler')
  spec.add_dependency('mruby-error', core: 'mruby-error')
  spec.add_test_dependency('mruby-print', core: 'mruby-print')
end
