as_cxx_srcs = %w[vm error gc].map{|name| "#{MRUBY_ROOT}/src/#{name}.c"}

MRuby.each_target do
  objs = Dir.glob("#{MRUBY_ROOT}/src/*.c").map do |src|
    dst = src.pathmap("#{build_dir}/src/%n")
    if cxx_exception_enabled? && as_cxx_srcs.include?(src)
      compile_as_cxx(src, "#{dst}.cxx")
    else
      objfile(dst)
    end
  end
  self.libmruby_objs << objs

  file libmruby_core_static => objs do |t|
    archiver.run t.name, t.prerequisites
  end
end
