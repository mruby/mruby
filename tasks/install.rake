desc "install compiled products"
task :install => :install_bin do
  if host = MRuby.targets['host']
    install_D host.libmruby_static, File.join(MRuby::INSTALL_PREFIX, "lib", File.basename(host.libmruby_static))
    # install mruby.h and mrbconf.h
    Dir.glob(File.join(MRUBY_ROOT, "include", "*.h")) do |src|
      install_D src, File.join(MRuby::INSTALL_PREFIX, "include", File.basename(src))
    end
    Dir.glob(File.join(MRUBY_ROOT, "include", "mruby", "*.h")) do |src|
      install_D src, File.join(MRuby::INSTALL_PREFIX, "include", "mruby", File.basename(src))
    end
    Dir.glob(File.join(File.join(MRUBY_ROOT, "build", "host", "include", "mruby", "presym", "*.h"))) do |src|
      install_D src, File.join(MRuby::INSTALL_PREFIX, "include", "mruby", "presym", File.basename(src))
    end
  end
end

desc "install compiled executable (on host)"
task :install_bin => :all do
  if host = MRuby.targets['host']
    Dir.glob(File.join(MRUBY_ROOT, "bin", "*")) do |src|
      install_D src, File.join(MRuby::INSTALL_PREFIX, "bin", File.basename(src))
    end
  end
end
