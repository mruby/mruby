MRuby.each_target do
  dir = File.dirname(__FILE__).relative_path_from(root)
  self.libmruby << objfile("#{build_dir}/#{dir}/mrblib")

  file objfile("#{build_dir}/#{dir}/mrblib") => "#{build_dir}/#{dir}/mrblib.c"
  file "#{build_dir}/#{dir}/mrblib.c" => [mrbcfile] + Dir.glob("#{dir}/*.rb") do |t|
    mrbc_, *rbfiles = t.prerequisites
    FileUtils.mkdir_p File.dirname(t.name)
    open(t.name, 'w') do |f|
      _pp "GEN", "*.rb", "#{t.name}"
      f.puts File.read("#{dir}/init_mrblib.c")
      mrbc.run f, rbfiles, 'mrblib_irep'
    end
  end
end
