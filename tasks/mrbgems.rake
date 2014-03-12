MRuby.each_target do
  if enable_gems?
    # set up all gems
    gems.each(&:setup)
    gems.check

    lazy_gems, boot_gems = gems.partition {|g| g.activation_policy == 'lazy' }
    uses_lazy = lazy_gems.length > 0

    # loader all gems
    self.libmruby << objfile("#{build_dir}/mrbgems/gem_init")
    file objfile("#{build_dir}/mrbgems/gem_init") => ["#{build_dir}/mrbgems/gem_init.c", "#{build_dir}/LEGAL"]
    file "#{build_dir}/mrbgems/gem_init.c" => [MRUBY_CONFIG] do |t|
      FileUtils.mkdir_p "#{build_dir}/mrbgems"
      open(t.name, 'w') do |f|
        f.puts %Q[/*]
        f.puts %Q[ * This file contains a list of all]
        f.puts %Q[ * initializing methods which are]
        f.puts %Q[ * necessary to bootstrap all gems.]
        f.puts %Q[ *]
        f.puts %Q[ * IMPORTANT:]
        f.puts %Q[ *   This file was generated!]
        f.puts %Q[ *   All manual changes will get lost.]
        f.puts %Q[ */]
        f.puts %Q[]
        f.puts %Q[#include <string.h>]
        f.puts %Q[#include "mruby.h"]
        if uses_lazy
          f.puts %Q[#include "mruby/khash.h"]
          f.puts %Q[]
          f.puts %Q[static inline khint_t]
          f.puts %Q[sym_hash_func(mrb_state *mrb, const char *s)]
          f.puts %Q[{]
          f.puts %Q[  khint_t h = 0;]
          f.puts %Q[  const char *p;]
          f.puts %Q[  for (p = s; *p != '\\0'; p++) {]
          f.puts %Q[    h = (h << 5) - h + *p;]
          f.puts %Q[  }]
          f.puts %Q[  return h;]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[static inline int]
          f.puts %Q[sym_hash_equal(mrb_state *mrb, const char *a, const char *b)]
          f.puts %Q[{]
          f.puts %Q[  return strcmp(a, b) == 0;]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[typedef void (*gem_init_t)(mrb_state *mrb);]
          f.puts %Q[KHASH_DECLARE(lazy_gems, const char *, gem_init_t, 1)]
          f.puts %Q[KHASH_DEFINE (lazy_gems, const char *, gem_init_t, 1, sym_hash_func, sym_hash_equal)]
          f.puts %Q[]
          f.puts %Q[]
          f.puts %Q[inline static int]
          f.puts %Q[mrb_do_prelinked(mrb_state *mrb, const char *name, khash_t(lazy_gems) *h)]
          f.puts %Q[{]
          f.puts %Q[  int result;]
          f.puts %Q[  khiter_t k;]
          f.puts %Q[]
          f.puts %Q[  if (mrb == NULL || name == NULL || h == NULL) return -1;]
          f.puts %Q[  k = kh_get(lazy_gems, mrb, h, name);]
          f.puts %Q[  if (kh_exist(h, k) == 0) {]
          f.puts %Q[    result = -1;]
          f.puts %Q[  }]
          f.puts %Q[  else {]
          f.puts %Q[    kh_value(h, k)(mrb);]
          f.puts %Q[    kh_del(lazy_gems, mrb, h, k);]
          f.puts %Q[    result = 0;]
          f.puts %Q[  }]
          f.puts %Q[]
          f.puts %Q[  return result;]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[int]
          f.puts %Q[mrb_gem_require_prelinked(mrb_state *mrb, const char *name)]
          f.puts %Q[{]
          f.puts %Q[  khash_t(lazy_gems) *h = mrb->uninit_gems;]
          f.puts %Q[]
          f.puts %Q[  return mrb_do_prelinked(mrb, name, h);]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[int]
          f.puts %Q[mrb_gem_finalize_prelinked(mrb_state *mrb, const char *name)]
          f.puts %Q[{]
          f.puts %Q[  khash_t(lazy_gems) *h = mrb->unfini_gems;]
          f.puts %Q[]
          f.puts %Q[  return mrb_do_prelinked(mrb, name, h);]
          f.puts %Q[}]
        end
        f.puts %Q[]
        gems.map{|g|
          f.puts %Q[void GENERATED_TMP_mrb_#{g.funcname}_gem_init(mrb_state *);]
          f.puts %Q[void GENERATED_TMP_mrb_#{g.funcname}_gem_final(mrb_state *);]
        }
        lazy_gems.each_with_index {|g,i|
          f.puts %Q[static const char GENERATED_TMP_mrb_lazy_gems_name_#{i}\[\] = "#{g.name}";]
        }
        if uses_lazy
          f.puts %Q[]
          f.puts %Q[static inline void]
          f.puts %Q[gem_setup_lazy_uninit_hash(mrb_state *mrb)]
          f.puts %Q[{]
          f.puts %Q[  khiter_t k;]
          f.puts %Q[  khash_t(lazy_gems) *h = kh_init(lazy_gems, mrb);]
          f.puts %Q[  mrb->uninit_gems = h;]
          lazy_gems.each_with_index {|g,i|
            f.puts   "  k = kh_put(lazy_gems, mrb, h, &GENERATED_TMP_mrb_lazy_gems_name_#{i}[0]);"
            f.puts %Q[  kh_value(h, k) = GENERATED_TMP_mrb_#{g.funcname}_gem_init;]
          }
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[static inline void]
          f.puts %Q[gem_setup_lazy_unfini_hash(mrb_state *mrb)]
          f.puts %Q[{]
          f.puts %Q[  khiter_t k;]
          f.puts %Q[  khiter_t k_uninit;]
          f.puts %Q[  khash_t(lazy_gems) *h_uninit = mrb->uninit_gems;]
          f.puts %Q[  khash_t(lazy_gems) *h = kh_init(lazy_gems, mrb);]
          f.puts %Q[  mrb->unfini_gems = h;]
          f.puts %Q[]
          lazy_gems.each_with_index {|g,i|
            f.puts   "  k = kh_put(lazy_gems, mrb, h, &GENERATED_TMP_mrb_lazy_gems_name_#{i}[0]);"
            f.puts %Q[  kh_value(h, k) = GENERATED_TMP_mrb_#{g.funcname}_gem_final;]
          }
          f.puts %Q[  if (h_uninit) { /* Remove uninitialized gems */]
          f.puts %Q[    for(k_uninit = kh_begin(h_uninit); k_uninit != kh_end(h_uninit); k_uninit++) {]
          f.puts %Q[      if (kh_exist(h_uninit, k_uninit)) {]
          f.puts %Q[        const char *key = kh_key(h_uninit, k_uninit);]
          f.puts %Q[        k = kh_get(lazy_gems, mrb, h, key);]
          f.puts %Q[        if (k != kh_end(h) && kh_exist(h, k)) {]
          f.puts %Q[        kh_del(lazy_gems, mrb, h, k);]
          f.puts %Q[        }]
          f.puts %Q[      }]
          f.puts %Q[    }]
          f.puts %Q[  }]
          f.puts %Q[}]
          f.puts %Q[]
          f.puts %Q[static inline void]
          f.puts %Q[gem_final_mrbgems_remain(mrb_state *mrb)]
          f.puts %Q[{]
          f.puts %Q[  khiter_t k;]
          f.puts %Q[  khash_t(lazy_gems) *h = mrb->unfini_gems;]
          f.puts %Q[  for(k = kh_begin(h); k != kh_end(h); k++) {]
          f.puts %Q[    if (kh_exist(h, k)) {];
          f.puts %Q[      gem_init_t v = kh_value(h, k);]
          f.puts %Q[      if (v) v(mrb);]
          f.puts %Q[    }]
          f.puts %Q[  }]
          f.puts %Q[}]
        end
        f.puts %Q[void]
        f.puts %Q[mrb_init_mrbgems(mrb_state *mrb)]
        f.puts %Q[{]
        f.puts %Q[  gem_setup_lazy_uninit_hash(mrb);] if uses_lazy
        boot_gems.map{|g|
          f.puts %Q[  GENERATED_TMP_mrb_#{g.funcname}_gem_init(mrb);]
        }
        f.puts %Q[}]
        f.puts %Q[]
        f.puts %Q[void]
        f.puts %Q[mrb_final_mrbgems(mrb_state *mrb)]
        f.puts %Q[{]
        if uses_lazy
          f.puts %Q[  gem_setup_lazy_unfini_hash(mrb);]
        end
        gems.map{|g|
          f.puts %Q[  GENERATED_TMP_mrb_#{g.funcname}_gem_final(mrb);]
        }
        if uses_lazy
          f.puts %Q[  gem_final_mrbgems_remain(mrb);]
          f.puts %Q[]
          f.puts %Q[  mrb_assert(mrb->uninit_gems);]
          f.puts %Q[  kh_destroy(lazy_gems, mrb, mrb->uninit_gems);]
          f.puts %Q[  mrb_assert(mrb->unfini_gems);]
          f.puts %Q[  kh_destroy(lazy_gems, mrb, mrb->unfini_gems);]
        end
        f.puts %Q[}]
      end
    end
  end

  # legal documents
  file "#{build_dir}/LEGAL" => [MRUBY_CONFIG] do |t|
    open(t.name, 'w+') do |f|
     f.puts <<LEGAL
Copyright (c) #{Time.now.year} mruby developers

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
LEGAL

      if enable_gems?
        f.puts <<GEMS_LEGAL

Additional Licenses

Due to the reason that you choosed additional mruby packages (GEMS),
please check the following additional licenses too:
GEMS_LEGAL

        gems.map do |g|
          authors = [g.authors].flatten.sort.join(", ")
          f.puts
          f.puts "GEM: #{g.name}"
          f.puts "Copyright (c) #{Time.now.year} #{authors}"
          f.puts "License: #{g.licenses}"
        end
      end
    end
  end
end
