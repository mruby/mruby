MRuby.each_target do
  no_mrb_open_test_gem = []

  gems.each do |g|
    unless g.run_test_in_other_mrb_state?
      no_mrb_open_test_gem << g
      next
    end

    test_rbobj = g.test_rbireps.ext(exts.object)

    file test_rbobj => g.test_rbireps
    file g.test_rbireps => [g.test_rbfiles].flatten + [g.build.mrbcfile, __FILE__] do |t|
      open(t.name, 'w') do |f|
        g.print_gem_test_header(f)
        test_preload = g.test_preload and [g.dir, MRUBY_ROOT].map {|dir|
          File.expand_path(g.test_preload, dir)
        }.find {|file| File.exist?(file) }

        f.puts %Q[/*]
        f.puts %Q[ * This file contains a test code for #{g.name} gem.]
        f.puts %Q[ *]
        f.puts %Q[ * IMPORTANT:]
        f.puts %Q[ *   This file was generated!]
        f.puts %Q[ *   All manual changes will get lost.]
        f.puts %Q[ */]
        if test_preload.nil?
          f.puts %Q[extern const uint8_t mrbtest_assert_irep[];]
        else
          g.build.mrbc.run f, test_preload, "gem_test_irep_#{g.funcname}_preload"
        end
        g.test_rbfiles.flatten.each_with_index do |rbfile, i|
          g.build.mrbc.run f, rbfile, "gem_test_irep_#{g.funcname}_#{i}"
        end
        f.puts %Q[void mrb_#{g.funcname}_gem_test(mrb_state *mrb);] unless g.test_objs.empty?
        f.puts %Q[void mrb_t_pass_result(mrb_state *dst, mrb_state *src);]
        f.puts %Q[void GENERATED_TMP_mrb_#{g.funcname}_gem_test(mrb_state *mrb) {]
        unless g.test_rbfiles.empty?
          f.puts %Q[  mrb_state *mrb2;]
          if g.test_args.empty?
            f.puts %Q[  mrb_value verbose;]
          else
            f.puts %Q[  mrb_value verbose, test_args_hash;]
          end
          f.puts %Q[  int ai;]
          g.test_rbfiles.count.times do |i|
            f.puts %Q[  ai = mrb_gc_arena_save(mrb);]
            f.puts %Q[  mrb2 = mrb_open();]
            f.puts %Q[  verbose = mrb_gv_get(mrb, mrb_intern_lit(mrb, "$mrbtest_verbose"));]
            f.puts %Q[  if (mrb_test(verbose)) {]
            f.puts %Q[    mrb_gv_set(mrb2, mrb_intern_lit(mrb2, "$mrbtest_verbose"), verbose);]
            f.puts %Q[  }]
            if test_preload.nil?
              f.puts %Q[  mrb_load_irep(mrb2, mrbtest_assert_irep);]
            else
              f.puts %Q[  mrb_load_irep(mrb2, gem_test_irep_#{g.funcname}_preload);]
            end
            f.puts %Q[  if (mrb2->exc) {]
            f.puts %Q[    mrb_p(mrb2, mrb_obj_value(mrb2->exc));]
            f.puts %Q[    exit(EXIT_FAILURE);]
            f.puts %Q[  }]
            f.puts %Q[  mrb_const_set(mrb2, mrb_obj_value(mrb2->object_class), mrb_intern_lit(mrb2, "GEMNAME"), mrb_str_new(mrb2, "#{g.name}", #{g.name.length}));]

            unless g.test_args.empty?
              f.puts %Q[  test_args_hash = mrb_hash_new_capa(mrb, #{g.test_args.length}); ]
              g.test_args.each do |arg_name, arg_value|
                escaped_arg_name = arg_name.gsub('\\', '\\\\\\\\').gsub('"', '\"')
                escaped_arg_value = arg_value.gsub('\\', '\\\\\\\\').gsub('"', '\"')
                f.puts %Q[  mrb_hash_set(mrb2, test_args_hash, mrb_str_new(mrb2, "#{escaped_arg_name.to_s}", #{escaped_arg_name.to_s.length}), mrb_str_new(mrb2, "#{escaped_arg_value.to_s}", #{escaped_arg_value.to_s.length})); ]
              end
              f.puts %Q[  mrb_const_set(mrb2, mrb_obj_value(mrb2->object_class), mrb_intern_lit(mrb2, "TEST_ARGS"), test_args_hash); ]
            end

            f.puts %Q[  mrb_#{g.funcname}_gem_test(mrb2);] unless g.test_objs.empty?

            f.puts %Q[  mrb_load_irep(mrb2, gem_test_irep_#{g.funcname}_#{i});]
            f.puts %Q[  if (mrb2->exc) {]
            f.puts %Q[    mrb_p(mrb2, mrb_obj_value(mrb2->exc));]
            f.puts %Q[    exit(EXIT_FAILURE);]
            f.puts %Q[  }]
            f.puts %Q[  ]

            f.puts %Q[  mrb_t_pass_result(mrb, mrb2);]
            f.puts %Q[  mrb_gc_arena_restore(mrb, ai);]
          end
        end
        f.puts %Q[}]
      end
    end

  end

  no_mrb_open_test = "#{build_dir}/test/no_mrb_open_test"
  no_mrb_open_test_rbfiles = no_mrb_open_test_gem.reduce([]) { |res, v|
    res += v.test_rbfiles
  }
  if no_mrb_open_test_rbfiles.empty?
    no_mrb_open_test_rbfiles << "#{MRUBY_ROOT}/test/no_mrb_open_test_dummy.rb"
  end

  no_mrb_open_test_lib = no_mrb_open_test.ext(exts.object)
  file no_mrb_open_test_lib => "#{no_mrb_open_test}.c"
  file "#{no_mrb_open_test}.c" => no_mrb_open_test_rbfiles + [MRUBY_CONFIG] do |t|
    open(t.name, 'w') do |f|
      f.puts %Q[/*]
      f.puts %Q[ * This file contains a test code for following gems:]
      no_mrb_open_test_gem.each { |g| f.puts %Q[ *   #{g.name}] }
      f.puts %Q[ *]
      f.puts %Q[ * IMPORTANT:]
      f.puts %Q[ *   This file was generated!]
      f.puts %Q[ *   All manual changes will get lost.]
      f.puts %Q[ */]

      f.puts %Q[]

      f.puts %Q[\#include "mruby.h"]
      f.puts %Q[\#include "mruby/irep.h"]

      f.puts %Q[]

      mrbc.run f, no_mrb_open_test_rbfiles, "no_mrb_open_gem_test_irep"

      f.puts %Q[]

      f.puts %Q[void no_mrb_open_mrbgem_test(mrb_state *mrb) {]
      f.puts %Q[  mrb_load_irep(mrb, no_mrb_open_gem_test_irep);]
      f.puts %Q[}]
    end
  end
end
