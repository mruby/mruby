MRuby.each_target do
  current_dir = File.dirname(__FILE__).relative_path_from(Dir.pwd)
  relative_from_root = File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)
  current_build_dir = "#{build_dir}/#{relative_from_root}"

  lex_def = "#{current_dir}/lex.def"
  objs = Dir.glob("#{current_dir}/*.c").map { |f|
    next nil if cxx_abi_enabled? and f =~ /(codegen|error|vm).c$/
    objfile(f.pathmap("#{current_build_dir}/%n"))
  }.compact

  if cxx_abi_enabled?
    cxx_abi_dependency = %w(codegen error vm)
    cxx_abi_objs = cxx_abi_dependency.map { |v|
      src = "#{current_build_dir}/#{v}.cxx"
      file src => ["#{current_dir}/#{v}.c", __FILE__] do |t|
        File.open(t.name, 'w') do |f|
          f.write <<EOS
#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS

extern "C" {
#include "#{MRUBY_ROOT}/#{t.prerequisites.first}"
}


#{v == 'error'? 'mrb_int mrb_jmpbuf::jmpbuf_id = 0;' : ''}
EOS
        end
      end

      file objfile(src) => src do |t|
        cxx.run t.name, t.prerequisites.first, [], [current_dir]
      end

      objfile src
    }
    cxx_abi_objs << objfile("#{current_build_dir}/y.tab")

    file "#{current_build_dir}/y.tab.cxx" => ["#{current_build_dir}/y.tab.c", __FILE__] do |t|
      File.open(t.name, 'w') do |f|
        f.write <<EOS
#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS

extern "C" {
#include "#{t.prerequisites.first}"
}
EOS
      end
    end
    file objfile("#{current_build_dir}/y.tab") => ["#{current_build_dir}/y.tab.cxx", lex_def] do |t|
      cxx.run t.name, t.prerequisites.first, [], [current_dir]
    end

    objs += cxx_abi_objs
  else
    objs += [objfile("#{current_build_dir}/y.tab")]
    file objfile("#{current_build_dir}/y.tab") => ["#{current_build_dir}/y.tab.c", lex_def] do |t|
      cc.run t.name, t.prerequisites.first, [], [current_dir]
    end
  end
  self.libmruby << objs

  file libfile("#{build_dir}/lib/libmruby_core") => objs do |t|
    archiver.run t.name, t.prerequisites
  end

  # Parser
  file "#{current_build_dir}/y.tab.c" => ["#{current_dir}/parse.y"] do |t|
    yacc.run t.name, t.prerequisites.first
  end

  # Lexical analyzer
  file lex_def => "#{current_dir}/keywords" do |t|
    gperf.run t.name, t.prerequisites.first
  end
end
