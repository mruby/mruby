require 'open3'

# The finalizers left to the exit run when the program ends, so these run
# the mruby command.
def finalizer_run(code)
  out, err, stat = Open3.capture3(*cmd_list("mruby"), "-e", code)
  [out, err, stat.exitstatus]
end

def finalizer_mruby?
  File.exist?(cmd_bin("mruby"))
end

FINALIZER_LIVE = '$o = Object.new; ObjectSpace.define_finalizer($o, Proc.new { puts "fin" }); '

assert('the finalizers of the objects still alive run at exit, after the at_exit blocks') do
  skip "no mruby command" unless finalizer_mruby?
  out, _, status = finalizer_run(FINALIZER_LIVE + 'at_exit { puts "at_exit" }')
  assert_equal "at_exit\nfin\n", out
  assert_equal 0, status
end

assert('the finalizers run at exit after an at_exit block that calls exit') do
  skip "no mruby command" unless finalizer_mruby?
  out, _, status = finalizer_run(FINALIZER_LIVE + 'at_exit { exit 3 }')
  assert_equal "fin\n", out
  assert_equal 3, status
end

assert('the finalizers run at exit after an at_exit block that raises') do
  skip "no mruby command" unless finalizer_mruby?
  out, err, status = finalizer_run(FINALIZER_LIVE + 'at_exit { raise "in handler" }')
  assert_equal "fin\n", out
  assert_true err.include?("in handler (RuntimeError)"), err
  assert_equal 1, status
end

assert('a finalizer at exit cannot reach the batch the others wait in') do
  skip "no mruby command" unless finalizer_mruby?
  code = <<~'RUBY'
    $o = [Object.new, Object.new]
    ObjectSpace.define_finalizer($o[0], Proc.new {
      ObjectSpace.each_object(Array) { |a| a[1] = 42 if a.size >= 2 && a[0].is_a?(Array) }
    })
    ObjectSpace.define_finalizer($o[1], Proc.new { puts "B" })
  RUBY
  out, _, status = finalizer_run(code)
  assert_equal "B\n", out
  assert_equal 0, status
end
