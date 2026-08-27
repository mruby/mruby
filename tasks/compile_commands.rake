require_relative '../lib/mruby/compile_commands'

# The database answers for the compiles that ran, so the build runs first.
# It is incremental, so asking for the database again after a build costs
# the walk and nothing more.
desc "generate compile_commands.json from the records the build writes"
task "compile_commands.json" => :all do
  MRuby::CompileCommands.write
end

task :compile_commands => "compile_commands.json"
