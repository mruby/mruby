MRuby.each_target do
  current_dir = File.dirname(__FILE__).relative_path_from(Dir.pwd)
  relative_from_root = File.dirname(__FILE__).relative_path_from(MRUBY_ROOT)
  current_build_dir = "#{build_dir}/#{relative_from_root}"

  exec = exefile("#{current_build_dir}/mrbtest")
  clib = "#{current_build_dir}/mrbtest.c"
  mlib = clib.ext(exts.object)
  mrbs = Dir.glob("#{current_dir}/t/*.rb")
  init = "#{current_dir}/init_mrbtest.c"
  ass_c = "#{current_build_dir}/assert.c"
  ass_lib = ass_c.ext(exts.object)

  mrbtest_lib = libfile("#{current_build_dir}/mrbtest")
  gem_test_files = gems.select { |g| g.run_test_in_other_mrb_state? }.map { |g| g.test_rbireps.ext(exts.object) }
  file mrbtest_lib => [mlib, ass_lib, gems.map(&:test_objs), gem_test_files].flatten do |t|
    archiver.run t.name, t.prerequisites
  end
  file mrbtest_lib => "#{build_dir}/test/no_mrb_open_test.c".ext(exts.object)

  unless build_mrbtest_lib_only?
    driver_obj = objfile("#{current_build_dir}/driver")
    file exec => [driver_obj, mrbtest_lib, libfile("#{build_dir}/lib/libmruby")] do |t|
      gem_flags = gems.map { |g| g.linker.flags }
      gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
      gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
      gem_libraries = gems.map { |g| g.linker.libraries }
      gem_library_paths = gems.map { |g| g.linker.library_paths }
      linker.run t.name, t.prerequisites, gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries
    end
  end

  file ass_lib => ass_c
  file ass_c => "#{current_dir}/assert.rb" do |t|
    FileUtils.mkdir_p File.dirname t.name
    open(t.name, 'w') do |f|
      mrbc.run f, [t.prerequisites], 'mrbtest_assert_irep'
    end
  end

  file mlib => clib
  file clib => [mrbcfile, init] + mrbs do |t|
    _pp "GEN", "*.rb", "#{clib.relative_path}"
    FileUtils.mkdir_p File.dirname(clib)
    open(clib, 'w') do |f|
      f.puts %Q[/*]
      f.puts %Q[ * This file contains a list of all]
      f.puts %Q[ * test functions.]
      f.puts %Q[ *]
      f.puts %Q[ * IMPORTANT:]
      f.puts %Q[ *   This file was generated!]
      f.puts %Q[ *   All manual changes will get lost.]
      f.puts %Q[ */]
      f.puts %Q[]
      f.puts IO.read(init)
      mrbc.run f, mrbs, 'mrbtest_irep'
      gems.each do |g|
        next unless g.run_test_in_other_mrb_state?
        f.puts %Q[void GENERATED_TMP_mrb_#{g.funcname}_gem_test(mrb_state *mrb);]
      end
      f.puts %Q[void no_mrb_open_mrbgem_test(mrb_state *mrb);]
      f.puts %Q[void mrbgemtest_init(mrb_state* mrb) {]
      gems.each do |g|
        next unless g.run_test_in_other_mrb_state?
        f.puts %Q[    GENERATED_TMP_mrb_#{g.funcname}_gem_test(mrb);]
      end
      f.puts %Q[    no_mrb_open_mrbgem_test(mrb);]
      f.puts %Q[}]
    end
  end
end
