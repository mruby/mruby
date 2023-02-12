desc "install compiled products"
task :install => :install_bin do
  if host = MRuby.targets['host']
    Dir.glob(File.join(host.build_dir, "{include,lib}/**/*")) do |path|
      install_D path, File.join(MRuby::INSTALL_PREFIX, path.relative_path_from(host.build_dir)) if File.file? path
    end
  end
end

desc "install compiled executable (on host)"
task :install_bin => :all do
  if host = MRuby.targets['host']
    Dir.glob(File.join(host.build_dir, "bin/**/*")) do |path|
      install_D path, File.join(MRuby::INSTALL_PREFIX, path.relative_path_from(host.build_dir)) if File.file? path
    end
  end
end
