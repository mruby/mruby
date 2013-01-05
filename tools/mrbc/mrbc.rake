dir = File.dirname(__FILE__).sub(%r|^\./|, '')

MRuby.each_target do
  exec = exefile("#{build_dir}/bin/mrbc")
  objs = Dir.glob("#{dir}/*.{c}").map { |f| f.pathmap("#{build_dir}/%X.o") }

  file exec => objs + ["#{build_dir}/lib/libmruby_core.a"] do |t|
    link t.name, t.prerequisites, [], gems.map { |g| g.mruby_libs }
  end
end
