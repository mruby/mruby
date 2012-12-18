MRBGEMS_PATH = File.dirname(__FILE__)

GEM_INIT = "#{MRBGEMS_PATH}/gem_init"
GEM_MAKEFILE = "#{MRBGEMS_PATH}/g/Makefile"
GEM_MAKEFILE_LIST = "#{MRBGEMS_PATH}/g/MakefileGemList"
MAKEFILE_4_GEM = "#{MRUBY_ROOT}/mrbgems/Makefile4gem"

if ENV['OS'] == 'Windows_NT'
GEM_MAKE_FLAGS = "#{MAKE_FLAGS} MAKEFILE_4_GEM=\"#{MAKEFILE_4_GEM}\""
else
GEM_MAKE_FLAGS = "#{MAKE_FLAGS} MAKEFILE_4_GEM='#{MAKEFILE_4_GEM}'"
end

task :mrbgems_all => ["#{GEM_INIT}.a", :mrbgems_generate_gem_makefile_list] do
  for_each_gem do |path, gemname|
    sh "#{MAKE} -C #{path} #{GEM_MAKE_FLAGS}"
  end
end

task :mrbgems_clean do
  sh "cd #{MRUBY_ROOT}/mrbgems && #{RM_F} *.c *.d *.a *.o"
  sh "cd #{MRUBY_ROOT}/mrbgems/g && #{RM_F} *.c *.d *.rbtmp *.ctmp *.o mrbtest"
  for_each_gem do |path, gemname|
    sh "#{MAKE} clean -C #{path} #{GEM_MAKE_FLAGS}"
  end
end

task :mrbgems_prepare_test do
  sh "#{CAT} #{for_each_gem{|path, gemname| "#{path}/test/*.rb "}} > #{MRUBY_ROOT}/mrbgems/g/mrbgemtest.rbtmp"
  sh "#{MRUBY_ROOT}/bin/mrbc -Bmrbgemtest_irep -o#{MRUBY_ROOT}/mrbgems/g/mrbgemtest.ctmp #{MRUBY_ROOT}/mrbgems/g/mrbgemtest.rbtmp"
end

file "#{GEM_INIT}.a" => ["#{GEM_INIT}.c", "#{GEM_INIT}.o"] do |t|
  sh "#{AR} rs #{t.name} #{GEM_INIT}.o"
end

rule ".o" => [".c"] do |t|
  puts "Build the driver which initializes all gems"
  sh "#{CC} #{CFLAGS.join(' ')} -I#{MRUBY_ROOT}/include -MMD -c #{t.source} -o #{t.name}"
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

#{for_each_gem{|path, gemname, escaped_gemname| "void GENERATED_TMP_mrb_%s_gem_init(mrb_state*);" % [escaped_gemname]}}

void
mrb_init_mrbgems(mrb_state *mrb) {
#{for_each_gem{|path, gemname, escaped_gemname| "  GENERATED_TMP_mrb_%s_gem_init(mrb);" % [escaped_gemname]}}
}
__EOF__
  end
end

def for_each_gem(&block)
  IO.readlines(ACTIVE_GEMS).map { |line|
    path = line.chomp
    if not File.exist?(path)
      path2 = File.join MRUBY_ROOT, 'mrbgems', 'g', path
      path = path2 if File.exist? path2
    end
    gemname = File.basename(path)
    escaped_gemname = gemname.gsub(/-/, '_')
    block.call(path, gemname, escaped_gemname)
  }.join('')
end

task :mrbgems_generate_gem_makefile_list do
  open(GEM_MAKEFILE_LIST, 'w') do |f|
    f.puts <<__EOF__
GEM_LIST := #{for_each_gem{|path, gemname| "#{path}/mrb-#{gemname}-gem.a "}}

GEM_ARCHIVE_FILES := #{MRUBY_ROOT}/mrbgems/gem_init.a
GEM_ARCHIVE_FILES += $(GEM_LIST)

GEM_INCLUDE_LIST := #{for_each_gem{|path, gemname| "-I#{path}/include "}}
__EOF__
  end
end
