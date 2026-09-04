MRuby::Gem::Specification.new('mruby-bin-mrb') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'mruby runtime command (compiler-free)'
  spec.bins = %w(mrb)

  # NOTE: Unlike mruby-bin-mruby, this gem does NOT depend on
  # mruby-compiler. This makes it suitable for builds where the
  # compiler is excluded to reduce binary size.
  #
  # To use this gem in your build_config.rb:
  #
  #   MRuby::Build.new do |conf|
  #     conf.toolchain
  #     conf.gem :core => 'mruby-bin-mrb'
  #     # Do NOT include mruby-bin-mruby or mruby-compiler
  #     # unless other gems require them.
  #   end
end
