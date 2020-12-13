install_task = ->(src) do
  dst = "#{MRuby::Build.install_dir}/#{File.basename(src)}"
  file dst => src do
    install_D src, dst
  end
  dst
end

MRuby.each_target do |build|
  if build.host? && build.mrbc_build && !build.gems["mruby-bin-mrbc"]
    exe = build.exefile("#{build.mrbc_build.build_dir}/bin/mrbc")
    build.products << install_task.(exe)
  end

  build.bins.each do |bin|
    exe = build.exefile("#{build.build_dir}/bin/#{bin}")
    build.products << (build.host? ? install_task.(exe) : exe)
  end

  linker_attrs = build.gems.map{|gem| gem.linker.run_attrs}.transpose
  build.gems.each do |gem|
    gem.bins.each do |bin|
      exe = build.exefile("#{build.build_dir}/bin/#{bin}")
      objs = Dir["#{gem.dir}/tools/#{bin}/*.{c,cpp,cxx,cc}"].map do |f|
        build.objfile(f.pathmap("#{gem.build_dir}/tools/#{bin}/%n"))
      end

      file exe => objs.concat(build.libraries) do |t|
        build.linker.run t.name, t.prerequisites, *linker_attrs
      end

      build.products << (build.host? ? install_task.(exe) : exe)
    end
  end
end
