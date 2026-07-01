MRuby::Gem::Specification.new('mruby-compiler') do |spec|
  spec.license = 'MIT'
  spec.author  = 'mruby and PicoRuby developers'
  spec.summary = 'mruby compiler using Prism'

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
  unless cc.defines.any? { |d| d.match?(/\APRISM_DEPTH_MAXIMUM(=|\z)/) }
    cc.defines << 'PRISM_DEPTH_MAXIMUM=256'
  end
  if cc.defines.include?('PICORB_VM_MRUBY')
    cc.defines << 'MRC_TARGET_MRUBY'
  elsif cc.defines.include?('PICORB_VM_MRUBYC')
    cc.defines << 'MRC_TARGET_MRUBYC'
  elsif !cc.defines.include?('MRB_NO_GEMS')
    cc.defines << 'MRC_TARGET_MRUBY'
  end
  cc.defines << 'MRC_DEBUG' if cc.defines.any? { |d| d.match?(/\AMRB_DEBUG(=|\z)/) }
  cc.defines << 'PRISM_BUILD_MINIMAL' unless cc.defines.include?('MRC_DEBUG')

  # The compiler glue is built as C++ under MRB_USE_CXX_ABI, and mruby.h
  # requires __STDC_LIMIT_MACROS / __STDC_CONSTANT_MACROS before <stdint.h> in
  # C++ mode -- some libc stdint.h (e.g. mingw) only define UINTPTR_MAX and
  # friends when they are set. Define them on the command line so they apply no
  # matter which header pulls in <stdint.h> first.
  if build.cxx_abi_enabled?
    cc.defines += %w(__STDC_LIMIT_MACROS __STDC_CONSTANT_MACROS)
  end

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

  # Prism is a vendored C library and must be compiled as C: neither g++ (its
  # generated diagnostic table uses non-trivial designated initializers) nor
  # clang++ (its implicit void* conversions) can build it as C++. In an
  # MRB_USE_CXX_ABI build the rest of mruby compiles as C++, so strip the C++
  # compile flag here to keep these sources on the C compiler; mrc_common.h
  # wraps the Prism header in extern "C" so the C++ glue links against them.
  # Route Prism's allocator to libc there too: the C++ core exports mrb_malloc
  # with C++ linkage, which the C-compiled Prism objects could not resolve, and
  # Prism's parse memory is transient and freed through the same libc path.
  # The clone happens inside the file task (not here) so cc.flags is already
  # fully populated with the build's generated-header include flags.
  Dir.glob("#{prism_dir}/src/**/*.c").map do |src|
    obj = objfile(src.pathmap("#{build_dir}/lib/%n"))
    objs << obj
    file obj => [src] do |f|
      prism_cc = cc
      if build.cxx_abi_enabled?
        prism_cc = cc.clone
        prism_cc.flags = cc.flags.flatten - [cc.cxx_compile_flag].flatten
        prism_cc.defines = cc.defines + %w(MRC_ALLOC_LIBC)
      end
      prism_cc.run f.name, f.prerequisites.first
    end
  end
end
