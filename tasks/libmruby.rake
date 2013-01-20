MRuby.each_target do
  file libfile("#{build_dir}/lib/libmruby") => libmruby.flatten do |t|
    archiver.run t.name, t.prerequisites
  end
end
