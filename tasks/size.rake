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

# Asking for the size is asking for the report, printed: what CI shows after
# each build, and what a checkout answers with about the last build it made.
# `:build` rather than `:all`, so that a build already made answers with the
# tables alone and not the build summary over again.
desc "build, and print the size.json of each build"
task :size => :build do
  MRuby::SizeReport.print
end
