MRuby::Gem::Specification.new('mruby-bigint') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby developers'
  spec.summary = 'Integer class extension to multiple-precision'
  spec.build.defines << "MRB_USE_BIGINT"

  spec.add_test_dependency('mruby-numeric-ext', :core => 'mruby-numeric-ext')

  spec.build.libmruby_core_objs << Dir.glob(File.join(__dir__, "core/**/*.c")).map { |fn|
    objfile(fn.relative_path_from(__dir__).pathmap("#{spec.build_dir}/%X"))
  }
end
