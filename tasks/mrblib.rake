MRuby.each_target do
  src = "#{build_dir}/mrblib/mrblib.c"
  obj = objfile(src.ext)
  rbfiles = Dir["#{MRUBY_ROOT}/mrblib/*.rb"].sort!

  self.libmruby_objs << obj

  file obj => src
  file src => [mrbcfile, __FILE__, *rbfiles] do |t|
    if presym_enabled?
      cdump = true
      suffix = "proc"
    else
      cdump = false
      suffix = "irep"
    end
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
      unless presym_enabled?
        f.puts %Q[#include <mruby.h>]
        f.puts %Q[#include <mruby/irep.h>]
      end
      mrbc.run f, rbfiles, "mrblib_#{suffix}", cdump
      f.puts <<INIT_END
void
mrb_init_mrblib(mrb_state *mrb)
{
  mrb_load_#{suffix}(mrb, mrblib_#{suffix});
}
INIT_END
    end
  end
end
