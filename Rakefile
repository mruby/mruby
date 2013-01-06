# encoding: utf-8
# Build description.
# basic build file for mruby

load 'tasks/ruby_ext.rake'
load 'tasks/mruby_build.rake'
load 'tasks/mruby_gem_spec.rake'

##############################
# compile flags
load File.expand_path(ENV['CONFIG'] || './build_config.rb')

load 'tasks/rules.rake'
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

binfiles = [exefile('bin/mruby'), exefile('bin/mirb'), exefile('bin/mrbc')]

desc "build all targets, install (locally) in-repo"
task :all => binfiles + MRuby.targets.map { |t| [exefile("#{t.build_dir}/bin/mruby"), exefile("#{t.build_dir}/bin/mirb"), exefile("#{t.build_dir}/bin/mrbc")] }.flatten

file exefile('bin/mruby') => exefile('build/host/bin/mruby') do |t|
  FileUtils.cp t.prerequisites.first, t.name
end

file exefile('bin/mirb') => exefile('build/host/bin/mirb') do |t|
  FileUtils.cp t.prerequisites.first, t.name
end

file exefile('bin/mrbc') => exefile('build/host/bin/mrbc') do |t|
  FileUtils.cp t.prerequisites.first, t.name
end

desc "run all mruby tests"
task :test => MRuby.targets.map { |t| exefile("#{t.build_dir}/test/mrbtest") } do
  sh "#{filename exefile('build/host/test/mrbtest')}"
  if MRuby.targets.count > 1
    puts "\nYou should run #{MRuby.targets.map{ |t| t.name == 'host' ? nil : "#{t.build_dir}/test/mrbtest" }.compact.join(', ')} on target device."
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.targets.each do |t|
    FileUtils.rm_rf t.build_dir
  end
  FileUtils.rm_f binfiles
end
