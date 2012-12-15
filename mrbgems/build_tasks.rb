MRBGEMS_PATH = File.dirname(__FILE__)

GEM_INIT = "#{MRBGEMS_PATH}/gem_init"
GEM_MAKEFILE = "#{MRBGEMS_PATH}/g/Makefile"
GEM_MAKEFILE_LIST = "#{MRBGEMS_PATH}/g/MakefileGemList"
MAKEFILE_4_GEM = "#{MRUBY_ROOT}/mrbgems/Makefile4gem"

GEM_MAKE_FLAGS = "#{MAKE_FLAGS} MAKEFILE_4_GEM='#{MAKEFILE_4_GEM}'"

task :mrbgems_all => ["#{GEM_INIT}.a"] do
  for_each_gem do |f|
    sh "#{MAKE} -C #{f} #{GEM_MAKE_FLAGS}"
  end
end

task :mrbgems_clean do
  sh "cd #{MRUBY_ROOT}/mrbgems/g && #{RM_F} *.c *.d *.rbtmp *.ctmp *.o mrbtest"
  for_each_gem do |f|
    sh "#{MAKE} clean -C #{f} #{GEM_MAKE_FLAGS}"
  end
end

task :mrbgems_prepare_test do
  sh "#{CAT} #{for_each_gem{|f| "#{f}/test/*.rb "}} > #{MRUBY_ROOT}/mrbgems/g/mrbgemtest.rbtmp"
  sh "#{MRUBY_ROOT}/bin/mrbc -Bmrbgemtest_irep -o#{MRUBY_ROOT}/mrbgems/g/mrbgemtest.ctmp #{MRUBY_ROOT}/mrbgems/g/mrbgemtest.rbtmp"
end

file "#{GEM_INIT}.a" => ["#{GEM_INIT}.c", "#{GEM_INIT}.o"] do |t|
  sh "#{AR} rs #{t.name} #{t.prerequisites.join(' ')}"
end

rule ".o" => [".c"] do |t|
  puts "Build the driver which initializes all gems"
  sh "#{CC} #{CFLAGS.join(' ')} -I #{MRUBY_ROOT}/include -MMD -c #{t.source} -o #{t.name}"
end

file "#{GEM_INIT}.c" do |t|
  puts "Generate Gem driver: #{t.name}"
  open(t.name, 'w') do |f|
    f.puts <<__EOF__
/*
 * This file contains a list of all
 * initializing methods which are
 * necessary to bootstrap all gems.
 *
 * IMPORTANT:
 *   This file was generated!
 *   All manual changes will get lost.
 */

#include "mruby.h"

#{for_each_gem{|f| "void GENERATED_TMP_mrb_%s_gem_init(mrb_state*);\n" % [File.basename(f)]}}

void
mrb_init_mrbgems(mrb_state *mrb) {
#{for_each_gem{|f| "  GENERATED_TMP_mrb_%s_gem_init(mrb);\n" % [File.basename(f)]}}
}
__EOF__
  end
end

def for_each_gem(&block)
  IO.readlines(ACTIVE_GEMS).map { |line|
    block.call(line.chomp)
  }.join('')
end

task :mrbgems_generate_gem_makefile_list do
  open(GEM_MAKEFILE_LIST, 'w') do |f|
    f.puts <<__EOF__
GEM_LIST := #{for_each_gem{|f| "#{f}/mrb-#{File.basename(f)}-gem.a"}}

GEM_ARCHIVE_FILES := #{MRUBY_ROOT}/mrbgems/gem_init.a
GEM_ARCHIVE_FILES += $(GEM_LIST)
__EOF__
  end
end
