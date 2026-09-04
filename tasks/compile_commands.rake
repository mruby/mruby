require_relative '../lib/mruby/compile_commands'

# The database is written from the rules of the build, so it needs no build
# to be written. Asking for it by name writes it and nothing else: a checkout
# has its database before its first compile.
desc "write the compile_commands.json of every build, without building"
task "compile_commands.json" do
  MRuby::CompileCommands.write
end

task :compile_commands => "compile_commands.json"

# A build writes it again as it ends, since the sources a build generates
# join the database once they are there. `:build` has prerequisites and no
# action of its own, and Rake runs the prerequisites of a task before its
# actions, so this runs once every product is up to date, whatever order the
# task files load in.
task :build do
  MRuby::CompileCommands.write
end

# The database is one of the things the build leaves behind, and it answers
# for objects that are about to be gone. The one each build keeps of its own
# goes with the build directory that holds it.
task :clean do
  rm_f MRuby::CompileCommands.tree_path
end
