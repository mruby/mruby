MRuby::Gem::Specification.new('mruby-bin-strip') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'irep dump debug section remover command'
  spec.bins = %w(mruby-strip)
  spec.add_dependency 'mruby-proc-ext', :core =>'mruby-proc-ext'
end
