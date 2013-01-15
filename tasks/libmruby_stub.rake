MRuby.each_target do
  file "#{build_dir}/mrblib/mrblib_stub.o" => "#{build_dir}/mrblib/mrblib_stub.c"
  file "#{build_dir}/mrblib/mrblib_stub.c" do |t|
    open(t.name, 'w') do |f|
      f.puts <<__EOF__
#include "mruby.h"
void mrb_init_mrblib(mrb_state *mrb) {}
__EOF__
    end
  end
  file "#{build_dir}/lib/libmruby_mrblib_stub.a" => ["#{build_dir}/mrblib/mrblib_stub.o"] do |t|
    archive t.name, 'rs', t.prerequisites
  end

  if enable_gems?
    file "#{build_dir}/mrbgems/gem_stub.o" => "#{build_dir}/mrbgems/gem_stub.c"
    file "#{build_dir}/mrbgems/gem_stub.c" do |t|
      open(t.name, 'w') do |f|
        f.puts <<__EOF__
#include "mruby.h"
void mrb_init_mrbgems(mrb_state *mrb) {}
__EOF__
      end
    end
    file "#{build_dir}/lib/libmruby_mrbgems_stub.a" => ["#{build_dir}/mrbgems/gem_stub.o"] do |t|
      archive t.name, 'rs', t.prerequisites
    end
  end
end
