MRuby.each_target do
  current = File.dirname(__FILE__).relative_path_from(Dir.pwd)
  current_build = "#{build_dir}/#{File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)}"
  objs = Dir.glob("#{current}/*.c").map { |f| objfile f.pathmap("#{current_build}/%n") }.flatten

  file libfile("#{build_dir}/lib/libmruby_script_main") => objs do |t|
    archiver.run t.name, t.prerequisites
  end
end
