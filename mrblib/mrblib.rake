MRuby.each_target do
  current_dir = File.dirname(__FILE__)
  relative_from_root = File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)
  current_build_dir = "#{build_dir}/#{relative_from_root}"

  self.libmruby_objs << objfile("#{current_build_dir}/mrblib")

  file objfile("#{current_build_dir}/mrblib") => "#{current_build_dir}/mrblib.c"
  file "#{current_build_dir}/mrblib.c" => [mrbcfile, __FILE__] + Dir.glob("#{current_dir}/*.rb").sort do |t|
    _, _, *rbfiles = t.prerequisites
    mkdir_p File.dirname(t.name)
    open(t.name, 'w') do |f|
      _pp "GEN", "*.rb", "#{t.name.relative_path}"
      f.puts %Q[/*]
      f.puts %Q[ * This file is loading the mrblib]
      f.puts %Q[ *]
      f.puts %Q[ * IMPORTANT:]
      f.puts %Q[ *   This file was generated!]
      f.puts %Q[ *   All manual changes will get lost.]
      f.puts %Q[ */]
      mrbc.run f, rbfiles, 'mrblib_proc'
      f.puts <<INIT_END
void
mrb_init_mrblib(mrb_state *mrb)
{
  mrb_load_proc(mrb, mrblib_proc);
}
INIT_END
    end
  end
end
