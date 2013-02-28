# encoding: utf-8
# Build description.
# basic build file for mruby
MRUBY_ROOT = File.dirname(File.expand_path(__FILE__))

# load build systems
load "#{MRUBY_ROOT}/tasks/ruby_ext.rake"
load "#{MRUBY_ROOT}/tasks/mruby_build.rake"
load "#{MRUBY_ROOT}/tasks/mrbgem_spec.rake"

# load configuration file
MRUBY_CONFIGS = ["#{MRUBY_ROOT}/build_config.rb", ENV['MRUBY_CONFIG']].compact
MRUBY_CONFIGS.each do |config|
  load config unless config.empty?
end

# load basic rules
MRuby.each_target do |build|
  build.define_rules
end

# load custom rules
load "#{MRUBY_ROOT}/src/mruby_core.rake"
load "#{MRUBY_ROOT}/mrblib/mrblib.rake"
load "#{MRUBY_ROOT}/tools/mrbc/mrbc.rake"

load "#{MRUBY_ROOT}/tasks/mrbgems.rake"
load "#{MRUBY_ROOT}/tasks/libmruby.rake"
load "#{MRUBY_ROOT}/tools/mruby/mruby.rake"
load "#{MRUBY_ROOT}/tools/mirb/mirb.rake"

load "#{MRUBY_ROOT}/tasks/mrbgems_test.rake"
load "#{MRUBY_ROOT}/test/mrbtest.rake"

##############################
# generic build targets, rules
task :default => :all

depfiles = MRuby.targets['host'].bins.map do |bin|
  install_path = MRuby.targets['host'].exefile("#{MRUBY_ROOT}/bin/#{bin}")
  source_path = MRuby.targets['host'].exefile("#{MRuby.targets['host'].build_dir}/bin/#{bin}")

  file install_path => source_path do |t|
    FileUtils.cp t.prerequisites.first, t.name, { :verbose => $verbose }
  end
  
  install_path
end

depfiles += MRuby.targets.reject { |n, t| n == 'host' }.map { |n, t|
  [t.libfile("#{t.build_dir}/lib/libmruby")] + t.bins.map { |bin| t.exefile("#{t.build_dir}/bin/#{bin}") }
}.flatten

desc "build all targets, install (locally) in-repo"
task :all => depfiles do
  puts
  puts "Build summary:"
  puts
  MRuby.each_target do
    print_build_summary
  end
end

desc "run all mruby tests"
task :test => MRuby.targets.values.map { |t| t.build_mrbtest_lib_only? ? t.libfile("#{t.build_dir}/test/mrbtest") : t.exefile("#{t.build_dir}/test/mrbtest") } do
  MRuby.each_target do
    run_test unless build_mrbtest_lib_only?
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.each_target do |t|
    FileUtils.rm_rf t.build_dir, { :verbose => $verbose }
  end
  FileUtils.rm_f depfiles, { :verbose => $verbose }
  puts "Cleaned up build folder"
end
