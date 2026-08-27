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

if ENV["MRB_COMPILER_PRISM"] == "yes" && ENV["MRUBY_CONFIG"].to_s.empty? && ENV["CONFIG"].to_s.empty?
  ENV["MRUBY_CONFIG"] = "prism"
end

# load configuration file
MRUBY_CONFIG = MRuby::Build.mruby_config_path
load MRUBY_CONFIG

# Give every cross build the `mrbc` it compiles with. The whole config has been
# read, so a `host` declared after a cross build is as visible as one declared
# before it, and no gem has been set up yet, so nothing has asked for `mrbc`.
MRuby.resolve_mrbc_hosts

# define MRB_NO_GEMS and set up all gems
MRuby.each_target do |build|
  unless enable_gems? && libmruby_enabled?
    compilers.each do |compiler|
      compiler.defines << "MRB_NO_GEMS"
    end
  end
  gems.setup(self) if enable_gems?

  # The config has been read and every gem's mrbgem.rake body has run, so the
  # defines a gem contributes are all in. `build.has_define?` answers from
  # here on, and refuses before.
  build.defines_final!
end

# load basic rules
MRuby.each_target do |build|
  build.define_rules
end

# load custom rules
load "#{MRUBY_ROOT}/tasks/core.rake"
load "#{MRUBY_ROOT}/tasks/mrblib.rake"
load "#{MRUBY_ROOT}/tasks/mrbgems.rake"
load "#{MRUBY_ROOT}/tasks/libmruby.rake"
load "#{MRUBY_ROOT}/tasks/bin.rake"
# `revision.rake` adds a define to the compilers, and `presym.rake` resolves
# rules as it is loaded: the two are in this order and not the other one.
load "#{MRUBY_ROOT}/tasks/revision.rake"
load "#{MRUBY_ROOT}/tasks/presym.rake"
load "#{MRUBY_ROOT}/tasks/test.rake"
load "#{MRUBY_ROOT}/tasks/benchmark.rake"
load "#{MRUBY_ROOT}/tasks/doc.rake"
load "#{MRUBY_ROOT}/tasks/install.rake"
load "#{MRUBY_ROOT}/tasks/amalgam.rake"
load "#{MRUBY_ROOT}/tasks/compile_commands.rake"
load "#{MRUBY_ROOT}/tasks/unicode.rake"
load "#{MRUBY_ROOT}/tasks/difftest.rake"

##############################
# generic build targets, rules
task :default => :all

desc "build all targets, install (locally) in-repo"
task :all => :gensym do
  Rake::Task[:build].invoke
  puts
  puts "Build summary:"
  puts
  MRuby.each_target do |build|
    build.print_build_summary
  end
  MRuby::Lockfile.write
end

task :build => MRuby.targets.flat_map{|_, build| build.products}

desc "download all gem dependencies without building"
task :fetch do
  MRuby.each_target do |build|
    puts "Dependencies ready for '#{build.name}'"
  end
end

desc "clean all built and in-repo installed artifacts"
task :clean do
  MRuby.each_target do |build|
    rm_rf build.build_dir
    rm_f build.products
  end
  puts "Cleaned up target build directory"
end

desc "clean everything!"
task :deep_clean => %w[clean doc:clean] do
  MRuby.each_target do |build|
    rm_rf build.gem_clone_dir
  end
  rm_rf "#{MRUBY_ROOT}/bin"
  rm_rf "#{MRUBY_ROOT}/build"
  puts "Cleaned up mrbgems build directory"
end

desc "run all pre-commit hooks against all files"
task :check do
  sh "prek run --all-files"
end

desc "install the pre-commit hooks"
task :checkinstall do
  sh "prek install"
end

desc "check the pre-commit hooks for updates"
task :checkupdate do
  sh "prek autoupdate"
end

desc "run all pre-commit hooks against all files with docker-compose"
task :composecheck do
  sh "docker-compose -p mruby run test prek run --all-files"
end

desc "build and run all mruby tests with docker-compose"
task :composetest do
  sh "docker-compose -p mruby run test"
end
