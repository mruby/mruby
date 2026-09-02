require_relative '../lib/mruby/define_log'

# The log says where every define of the configuration came from, so it is
# printed where the build starts. Every build entry point runs `:gensym`
# first, its prerequisites (the presym scan) before this action and every
# compile after it, so the log opens the compile output whatever task asked
# for the build. `conf.disable_define_log` leaves a build out.
task :gensym do
  MRuby::DefineLog.print
end

# The Rakefile wires `:all => :gensym` but gives `:build` only the product
# tasks, whose presym needs come through per-target proxies: a direct
# `rake build` would compile without the log. This file loads before
# `:build` gains those products, so `:gensym` sits in front of them.
task :build => :gensym

desc "print where every define of each build came from"
task :defines do
  MRuby::DefineLog.print
end
