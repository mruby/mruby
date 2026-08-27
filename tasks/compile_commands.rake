require_relative '../lib/mruby/compile_commands'

# The database says what the compiles of the build were, so it is written
# where the build ends. `:build` has prerequisites and no action of its own,
# and Rake runs the prerequisites of a task before its actions, so this runs
# once every product is up to date, whatever order the task files load in.
task :build do
  MRuby::CompileCommands.write
end

# Asking for the database by name is asking for the build that writes it.
desc "build, and write the compile_commands.json of the build"
task "compile_commands.json" => :all

task :compile_commands => "compile_commands.json"

# The database is one of the things the build leaves behind, and it answers
# for objects that are about to be gone. The one each build keeps of its own
# goes with the build directory that holds it.
task :clean do
  rm_f MRuby::CompileCommands.tree_path
end
