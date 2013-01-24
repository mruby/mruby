# encoding: utf-8
# Build description.
# basic build file for mruby

load 'tasks/ruby_ext.rake'
load 'tasks/mruby_build.rake'
load 'tasks/mrbgem_spec.rake'

##############################
# compile flags
MRUBY_CONFIG = File.expand_path(ENV['MRUBY_CONFIG'] || './build_config.rb')
load MRUBY_CONFIG

load 'src/mruby_core.rake'
load 'mrblib/mrblib.rake'
load 'tools/mrbc/mrbc.rake'

load 'tasks/mrbgems.rake'
load 'tasks/libmruby.rake'
load 'tools/mruby/mruby.rake'
load 'tools/mirb/mirb.rake'

load 'tasks/mrbgems_test.rake'
load 'test/mrbtest.rake'

##############################
# generic build targets, rules
task :default => :all

depfiles = MRuby.targets['host'].bins.map do |bin|
  install_path = MRuby.targets['host'].exefile("bin/#{bin}")
  
  file install_path => MRuby.targets['host'].exefile("build/host/bin/#{bin}") do |t|
    FileUtils.cp t.prerequisites.first, t.name
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
task :test => MRuby.targets.values.map { |t| t.exefile("#{t.build_dir}/test/mrbtest") } do
  MRuby.each_target do
    run_test
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.each_target do |t|
    FileUtils.rm_rf t.build_dir
  end
  FileUtils.rm_f depfiles
  puts "Cleaned up build folder"
end
