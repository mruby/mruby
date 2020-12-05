MRuby.each_target do
  next unless libmruby_enabled?

  src = "#{build_dir}/mrblib/mrblib.c"
  obj = objfile(src.ext)
  rbfiles = Dir["#{MRUBY_ROOT}/mrblib/*.rb"].sort!

  self.libmruby_objs << obj

  file obj => src
  file src => [mrbcfile, __FILE__, *rbfiles] do |t|
    mkdir_p File.dirname(t.name)
    File.open(t.name, 'w') do |f|
      _pp "GEN", "mrblib/*.rb", "#{t.name.relative_path}"
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
