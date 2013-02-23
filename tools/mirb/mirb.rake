MRuby.each_target do
  current_dir = File.dirname(__FILE__).relative_path_from(Dir.pwd)
  relative_from_root = File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)
  current_build_dir = "#{build_dir}/#{relative_from_root}"

  if bins.find { |s| s.to_s == 'mirb' }
    exec = exefile("#{build_dir}/bin/mirb")
    objs = Dir.glob("#{current_dir}/*.c").map { |f| objfile(f.pathmap("#{current_build_dir}/%n")) }

    file exec => objs + [libfile("#{build_dir}/lib/libmruby")] do |t|
      gem_flags = gems.map { |g| g.linker.flags }
      gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
      gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
      gem_libraries = gems.map { |g| g.linker.libraries }
      gem_library_paths = gems.map { |g| g.linker.library_paths }
      linker.run t.name, t.prerequisites, gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries
    end
  end
end
