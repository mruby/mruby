require_relative '../lib/mruby/size_report'

# The report says how big what the build produced is, so it is written where
# the build ends. `:build` has prerequisites and no action of its own, and
# Rake runs the prerequisites of a task before its actions, so this runs once
# every product is up to date, whatever order the task files load in.
task :build do
  MRuby::SizeReport.write
end

# Asking for the report by name is asking for the build that writes it.
desc "build, and write the size.json of each build"
task "size.json" => :all

task :size => "size.json"
