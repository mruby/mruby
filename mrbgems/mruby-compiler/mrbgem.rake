MRuby::Gem::Specification.new('mruby-compiler') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby and PicoRuby developers'
  spec.summary = 'mruby compiler using Prism'

  spec.add_conflict 'mruby-compiler-lrama'

  lib_dir = "#{dir}/lib"
  prism_dir = "#{lib_dir}/prism"
  prism_templates_dir = "#{prism_dir}/templates"

  cc.include_paths << "#{dir}/include"
  cc.include_paths << "#{prism_dir}/include"

  cc.defines.flatten!
  cc.defines << 'PRISM_XALLOCATOR'
  # Prism is a recursive-descent parser and mruby has no machine-stack-overflow
  # backstop, so its default nesting cap of 10000 overflows the C stack (a
  # crash) before the limit trips -- e.g. an ASan build dies around 400-500
  # deep. Cap nesting to match the codegen's own MRC_CODEGEN_LEVEL_MAX (256):
  # the codegen cannot compile an expression nested deeper than that anyway, so
  # this rejects nothing compilable while staying well below the stack limit.
  # Targets on a tiny stack can lower it; those that raise it must also raise
  # MRC_CODEGEN_LEVEL_MAX and have the C stack to match.
  unless cc.defines.any? { _1.match?(/\APRISM_DEPTH_MAXIMUM(=|\z)/) }
    cc.defines << 'PRISM_DEPTH_MAXIMUM=256'
  end
  if cc.defines.include?('PICORB_VM_MRUBY')
    cc.defines << 'MRC_TARGET_MRUBY'
  elsif cc.defines.include?('PICORB_VM_MRUBYC')
    cc.defines << 'MRC_TARGET_MRUBYC'
  elsif !cc.defines.include?('MRB_NO_GEMS')
    cc.defines << 'MRC_TARGET_MRUBY'
  end
  cc.defines << 'MRC_DEBUG' if cc.defines.any? { _1.match?(/\AMRB_DEBUG(=|\z)/) }
  cc.defines << 'PRISM_BUILD_MINIMAL' unless cc.defines.include?('MRC_DEBUG')

  next if %w(clean deep_clean).include?(Rake.application.top_level_tasks.first)

  prism_generated_files = %w[
    ext/prism/api_node.c
    include/prism/ast.h
    include/prism/diagnostic.h
    src/diagnostic.c
    src/node.c
    src/prettyprint.c
    src/serialize.c
    src/token_type.c
  ].map { |path| "#{prism_dir}/#{path}" }

  task :prism_submodule do
    next if File.exist?("#{prism_dir}/templates/template.rb")

    FileUtils.cd dir do
      sh 'git submodule update --init lib/prism'
    end
  end

  task prism_templates: :prism_submodule do
    missing = prism_generated_files.reject { |path| File.exist?(path) }
    next if missing.empty?

    FileUtils.cd prism_dir do
      sh "#{RbConfig.ruby} templates/template.rb"
    end
  end

  Rake::Task[:prism_templates].invoke

  %w(node prettyprint serialize token_type).each do |name|
    dst = "#{prism_dir}/src/#{name}.c"
    file dst => ["#{prism_templates_dir}/src/#{name}.c.erb", "#{prism_templates_dir}/template.rb"] do
      Rake::Task[:prism_templates].invoke
    end
  end

  Dir.glob("#{prism_dir}/src/**/*.c").map do |src|
    obj = objfile(src.pathmap("#{build_dir}/lib/%n"))
    objs << obj
    file obj => [src] do |f|
      cc.run f.name, f.prerequisites.first
    end
  end
end
