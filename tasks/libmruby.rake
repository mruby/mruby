MRuby.each_target do
  file "#{build_dir}/lib/libmruby.a" => libmruby.flatten do |t|
    archive t.name, 'r', t.prerequisites
  end
end
