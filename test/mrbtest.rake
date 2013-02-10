MRuby.each_target do
  dir = File.dirname(__FILE__).relative_path_from(root)

  exec = exefile("#{build_dir}/#{dir}/mrbtest")
  clib = "#{build_dir}/#{dir}/mrbtest.c"
  mlib = clib.ext(exts.object)
  mrbs = Dir.glob("#{dir}/t/*.rb")
  init = "#{dir}/init_mrbtest.c"
  asslib = "#{dir}/assert.rb"
  catrb  = "#{build_dir}/#{dir}/mrbtest.rb"
  catmrb = "#{build_dir}/#{dir}/mrbtest.mrb"

  mrbtest_lib = libfile("#{build_dir}/#{dir}/mrbtest")
  file mrbtest_lib => [mlib, gems.map(&:test_objs), gems.map { |g| g.test_rbireps.ext(exts.object) }].flatten do |t|
    archiver.run t.name, t.prerequisites
  end

  unless build_mrbtest_lib_only?
    driver_obj = objfile("#{build_dir}/#{dir}/driver")
    file exec => [driver_obj, mrbtest_lib, libfile("#{build_dir}/lib/libmruby")] do |t|
      gem_flags = gems.map { |g| g.linker.flags }
      gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
      gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
      gem_libraries = gems.map { |g| g.linker.libraries }
      gem_library_paths = gems.map { |g| g.linker.library_paths }
      linker.run t.name, t.prerequisites, gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries
    end
  end

  file mlib => [clib, catmrb]
  file clib => [mrbcfile, init, asslib] + mrbs do |t|
    _pp "GEN", "*.rb", "#{clib}"
    FileUtils.mkdir_p File.dirname(clib)
    open(clib, 'w') do |f|
      f.puts IO.read(init)
      mrbc.run f, [asslib] + mrbs, 'mrbtest_irep'
      gems.each do |g|
        f.puts %Q[void GENERATED_TMP_mrb_#{g.funcname}_gem_test(mrb_state *mrb);]
      end
      f.puts %Q[void mrbgemtest_init(mrb_state* mrb) {]
      gems.each do |g|
        f.puts %Q[    GENERATED_TMP_mrb_#{g.funcname}_gem_test(mrb);]
      end
      f.puts %Q[}]
    end
  end

  file catmrb => [catrb] do |t|
    mrbc.compile(catmrb, catrb)
  end

  file catrb do |t|
    _pp "GEM", "*.rb", "#{catrb}"
    files = ([asslib] + mrbs + gems.map{ |g| g.test_rbfiles }).flatten.compact
    open(catrb, 'w') do |f|
      files.each do |fname|
        f.puts IO.read(fname)
      end
      f.puts %Q[report]
      f.puts %Q[raise 'mrbtest error' if $ko_test != 0 || $kill_test != 0]
    end
  end
end
