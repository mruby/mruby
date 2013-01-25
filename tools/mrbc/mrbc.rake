MRuby.each_target do
  dir = File.dirname(__FILE__).relative_path_from(root)

  if bins.find { |s| s.to_s == 'mrbc' }
    exec = exefile("#{build_dir}/bin/mrbc")
    objs = Dir.glob("#{dir}/*.c").map { |f| objfile(f.pathmap("#{build_dir}/%X")) }.flatten

    file exec => objs + [libfile("#{build_dir}/lib/libmruby_core")] do |t|
      linker.run t.name, t.prerequisites
    end
  end
end
