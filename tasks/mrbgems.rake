MRuby.each_target do
  current_dir = File.dirname(__FILE__).relative_path_from(Dir.pwd)
  relative_from_root = File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)

  if enable_gems?
    # set up all gems
    gems.each(&:setup)
    
    # loader all gems
    self.libmruby << objfile("#{build_dir}/mrbgems/gem_init")
    file objfile("#{build_dir}/mrbgems/gem_init") => "#{build_dir}/mrbgems/gem_init.c"
    file "#{build_dir}/mrbgems/gem_init.c" => [MRUBY_CONFIG] do |t|
      FileUtils.mkdir_p "#{build_dir}/mrbgems"
      open(t.name, 'w') do |f|
        f.puts %Q[/*]
        f.puts %Q[ * This file contains a list of all]
        f.puts %Q[ * initializing methods which are]
        f.puts %Q[ * necessary to bootstrap all gems.]
        f.puts %Q[ *]
        f.puts %Q[ * IMPORTANT:]
        f.puts %Q[ *   This file was generated!]
        f.puts %Q[ *   All manual changes will get lost.]
        f.puts %Q[ */]
        f.puts %Q[]
        f.puts %Q[#include "mruby.h"]
        f.puts %Q[]
        f.puts %Q[#{gems.map{|g| "void GENERATED_TMP_mrb_%s_gem_init(mrb_state* mrb);" % g.funcname}.join("\n")}]
        f.puts %Q[]
        f.puts %Q[#{gems.map{|g| "void GENERATED_TMP_mrb_%s_gem_final(mrb_state* mrb);" % g.funcname}.join("\n")}]
        f.puts %Q[]
        f.puts %Q[void]
        f.puts %Q[mrb_init_mrbgems(mrb_state *mrb) {]
        f.puts %Q[#{gems.map{|g| "GENERATED_TMP_mrb_%s_gem_init(mrb);" % g.funcname}.join("\n")}]
        f.puts %Q[}]
        f.puts %Q[]
        f.puts %Q[void]
        f.puts %Q[mrb_final_mrbgems(mrb_state *mrb) {]
        f.puts %Q[#{gems.map{|g| "GENERATED_TMP_mrb_%s_gem_final(mrb);" % g.funcname}.join("\n")}]
        f.puts %Q[}]
      end
    end
  end
end
