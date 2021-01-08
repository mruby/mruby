desc "build and run all mruby tests"
task :test => "test:build" do
  Rake::Task["test:run"].invoke
end

namespace :test do |test_ns|
  desc "build and run library tests"
  task :lib => "build:lib" do
    test_ns["run:lib"].invoke
  end

  desc "build and run command binaries tests"
  task :bin => :all do
    test_ns["run:bin"].invoke
  end

  desc "build all mruby tests"
  task :build => "build:lib"

  namespace :build do
    desc "build library tests"
    task :lib
  end

  desc "run all mruby tests"
  task :run

  namespace :run do
    desc "run library tests"
    task :lib

    desc "run command binaries tests"
    task :bin
  end
end

MRuby.each_target do |build|
  if build.test_enabled?
    t = task "test:build:lib:#{build.name}" => :all do
      gem = build.gem(core: 'mruby-test')
      gem.setup
      gem.setup_compilers
      Rake::Task[build.define_installer_if_needed("mrbtest")].invoke
    end
    task "test:build:lib" => t

    t = task "test:run:lib:#{build.name}" do
      build.run_test
    end
    task "test:run" => t
    task "test:run:lib" => t
  end
  if build.bintest_enabled?
    t = task "test:run:bin:#{build.name}" do
      build.run_bintest
    end
    task "test:run" => t
    task "test:run:bin" => t
  end
end

task :clean do
  rm_f "#{MRuby::Build.install_dir}/mrbtest"
end
