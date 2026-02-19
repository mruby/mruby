MRuby.each_target do
  next unless libmruby_enabled?

  src = "#{build_dir}/mrblib/mrblib.c"
  rbfiles = Dir["#{MRUBY_ROOT}/mrblib/*.rb"].sort!

  self.libmruby_objs << objfile(src.ext)

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
      mrbc.run f, rbfiles, "mrblib_proc", cdump: true, static: true
      f.puts %Q[void]
      f.puts %Q[mrb_init_mrblib(mrb_state *mrb)]
      f.puts %Q[{]
      f.puts %Q[  mrblib_proc_init_syms(mrb);]
      f.puts %Q[  mrb_load_proc(mrb, mrblib_proc);]
      f.puts %Q[}]
    end
  end
end
