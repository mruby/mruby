# encoding: utf-8
# Build description.
# basic build file for mruby
MRUBY_ROOT = File.dirname(File.expand_path(__FILE__))
MRUBY_BUILD_HOST_IS_CYGWIN = RUBY_PLATFORM.include?('cygwin')
MRUBY_BUILD_HOST_IS_OPENBSD = RUBY_PLATFORM.include?('openbsd')

Rake.verbose(false) if Rake.verbose == Rake::DSL::DEFAULT

$LOAD_PATH << File.join(MRUBY_ROOT, "lib")

# load build systems
require "mruby/core_ext"
require "mruby/build"

# load configuration file
MRUBY_CONFIG = MRuby::Build.mruby_config_path
load MRUBY_CONFIG

# load basic rules
MRuby.each_target do |build|
  build.define_rules
end

# load custom rules
load "#{MRUBY_ROOT}/tasks/core.rake"
load "#{MRUBY_ROOT}/tasks/mrblib.rake"
load "#{MRUBY_ROOT}/tasks/mrbgems.rake"
load "#{MRUBY_ROOT}/tasks/libmruby.rake"
load "#{MRUBY_ROOT}/tasks/presym.rake"

load "#{MRUBY_ROOT}/tasks/benchmark.rake"

load "#{MRUBY_ROOT}/tasks/gitlab.rake"
load "#{MRUBY_ROOT}/tasks/doc.rake"

##############################
# generic build targets, rules
task :default => :all

bin_path = ENV['INSTALL_DIR'] || "#{MRUBY_ROOT}/bin"

if MRuby.targets['host']
  target = MRuby.targets['host']
  depfiles = target.bins.map do |bin|
    install_path = target.exefile("#{bin_path}/#{bin}")
    source_path = target.exefile("#{target.build_dir}/bin/#{bin}")

    file install_path => source_path do |t|
      install_D t.prerequisites.first, t.name
    end
    install_path
  end
else
  depfiles = []
end

MRuby.each_target do |target|
  gems.each do |gem|
    current_dir = gem.dir.relative_path_from(Dir.pwd)
    relative_from_root = gem.dir.relative_path_from(MRUBY_ROOT)
    current_build_dir = File.expand_path "#{build_dir}/#{relative_from_root}"

    if current_build_dir !~ /^#{Regexp.escape(build_dir)}/
      current_build_dir = "#{build_dir}/mrbgems/#{gem.name}"
    end

    gem.bins.each do |bin|
      exec = exefile("#{build_dir}/bin/#{bin}")
      objs = Dir.glob("#{current_dir}/tools/#{bin}/*.{c,cpp,cxx,cc}").map { |f| objfile(f.pathmap("#{current_build_dir}/tools/#{bin}/%n")) }

      file exec => objs + target.libraries do |t|
        gem_flags = gems.map { |g| g.linker.flags }
        gem_flags_before_libraries = gems.map { |g| g.linker.flags_before_libraries }
        gem_flags_after_libraries = gems.map { |g| g.linker.flags_after_libraries }
        gem_libraries = gems.map { |g| g.linker.libraries }
        gem_library_paths = gems.map { |g| g.linker.library_paths }
        linker.run t.name, t.prerequisites, gem_libraries, gem_library_paths, gem_flags, gem_flags_before_libraries, gem_flags_after_libraries
      end

      if target == MRuby.targets['host']
        install_path = MRuby.targets['host'].exefile("#{bin_path}/#{bin}")

        file install_path => exec do |t|
          install_D t.prerequisites.first, t.name
        end
        depfiles += [ install_path ]
      else
        depfiles += [ exec ]
      end
    end
  end
end

desc "preallocated symbols"
task :gensym => MRuby.targets.values.map(&:presym_file)

depfiles += MRuby.targets.map { |n, t|
  t.libraries
}.flatten

depfiles += MRuby.targets.reject { |n, t| n == 'host' }.map { |n, t|
  t.bins.map { |bin| t.exefile("#{t.build_dir}/bin/#{bin}") }
}.flatten

desc "build all targets, install (locally) in-repo"
task :all => :build do
  puts
  puts "Build summary:"
  puts
  MRuby.each_target do
    print_build_summary
  end
  MRuby::Lockfile.write
end

task :build => :gensym do
  depfiles.each {|dep| Rake::Task[dep].invoke}
end

desc "run all mruby tests"
task :test
MRuby.each_target do
  if test_enabled?
    t = :"test_#{self.name}"
    task t => ["all"] do
      run_test
    end
    task :test => t
  end

  if bintest_enabled?
    t = :"bintest_#{self.name}"
    task t => ["all"] do
      run_bintest
    end
    task :test => t
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.each_target do |t|
    rm_rf t.build_dir
  end
  rm_f depfiles
  puts "Cleaned up target build folder"
end

desc "clean everything!"
task :deep_clean => ["clean", "clean_doc"] do
  MRuby.each_target do |t|
    rm_rf t.gem_clone_dir
  end
  puts "Cleaned up mrbgems build folder"
end
