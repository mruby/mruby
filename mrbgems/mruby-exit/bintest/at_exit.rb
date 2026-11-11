require 'open3'

# Kernel#at_exit runs when the program ends, so these run the mruby command.
def at_exit_run(code)
  out, err, stat = Open3.capture3(*cmd_list("mruby"), "-e", code)
  [out, err, stat.exitstatus]
end

def at_exit_mruby?
  File.exist?(cmd_bin("mruby"))
end

assert('at_exit runs its blocks when the program ends, the last first') do
  skip "no mruby command" unless at_exit_mruby?
  out, _, status = at_exit_run('at_exit { puts "a" }; at_exit { puts "b" }; puts "main"')
  assert_equal "main\nb\na\n", out
  assert_equal 0, status
end

assert('at_exit blocks registered while they run run too') do
  skip "no mruby command" unless at_exit_mruby?
  out, _, _ = at_exit_run('at_exit { at_exit { puts "nested" }; puts "outer" }')
  assert_equal "outer\nnested\n", out
end

assert('at_exit sees the exception the program ended with, before it is reported') do
  skip "no mruby command" unless at_exit_mruby?
  _, err, status = at_exit_run('at_exit { $stderr.puts "h #{$!.message}" }; raise "boom"')
  assert_true err.start_with?("h boom\n"), err
  assert_true err.include?("boom (RuntimeError)"), err
  assert_equal 1, status
end

assert('exit in an at_exit block sets the exit status') do
  skip "no mruby command" unless at_exit_mruby?
  assert_equal 3, at_exit_run('at_exit { exit 3 }; puts "m"')[2]
  assert_equal 4, at_exit_run('at_exit { exit 4 }; raise "boom"')[2]
  out, _, status = at_exit_run('at_exit { puts $!.class }; exit 5')
  assert_equal "SystemExit\n", out
  assert_equal 5, status
end

assert('an at_exit block that raises is reported, and the rest still run') do
  skip "no mruby command" unless at_exit_mruby?
  _, err, status = at_exit_run('at_exit { $stderr.puts "second" }; at_exit { raise "in handler" }')
  assert_true err.include?("in handler (RuntimeError)"), err
  assert_true err.end_with?("second\n"), err
  assert_equal 1, status
end

assert('exit! skips the at_exit blocks') do
  skip "no mruby command" unless at_exit_mruby?
  out, _, status = at_exit_run('at_exit { puts "no" }; exit! 2')
  assert_equal "", out
  assert_equal 2, status
end

assert('an at_exit block sees, as $!, what a block run before it raised') do
  skip "no mruby command" unless at_exit_mruby?
  out, _, status = at_exit_run('at_exit { puts $!.class }; at_exit { exit 5 }')
  assert_equal "SystemExit\n", out
  assert_equal 5, status
  out, _, _ = at_exit_run('at_exit { puts $!.message }; at_exit { raise "h" }')
  assert_equal "h\n", out
end

assert('the exception the program ended with outlives a block that lets go of it') do
  skip "no mruby command" unless at_exit_mruby?
  _, err, status = at_exit_run('at_exit { GC.start; $stderr.puts "sees #{$!.message}" }; at_exit { $! = nil; GC.start }; raise "boom"')
  assert_true err.start_with?("sees boom\n"), err
  assert_true err.include?("boom (RuntimeError)"), err
  assert_equal 1, status
end
