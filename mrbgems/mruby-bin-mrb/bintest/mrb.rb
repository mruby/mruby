require 'tempfile'
require 'open3'

def assert_mrb(exp_out, exp_err, exp_success, args)
  out, err, stat = Open3.capture3(*(cmd_list("mrb") + args))
  assert "assert_mrb" do
    assert_operator(exp_out, :===, out, "standard output")
    assert_operator(exp_err, :===, err, "standard error")
    assert_equal(exp_success, stat.success?, "exit success?")
  end
end

assert('mrb can execute .mrb files') do
  script = Tempfile.new(['test', '.rb'])
  bin = Tempfile.new(['test', '.mrb'])
  File.write(script.path, 'print "hello from mrb"')
  assert_run('mrbc', '-o', bin.path, script.path)
  o, = Open3.capture2(*(cmd_list('mrb') + [bin.path]))
  assert_equal 'hello from mrb', o.strip
end

assert('mrb $0 value') do
  script = Tempfile.new(['test', '.rb'])
  bin = Tempfile.new(['test', '.mrb'])
  File.write(script.path, 'print $0')
  assert_run('mrbc', '-o', bin.path, script.path)
  o, = Open3.capture2(*(cmd_list('mrb') + [bin.path]))
  assert_equal bin.path, o.strip
end

assert('mrb ARGV value') do
  script = Tempfile.new(['test', '.rb'])
  bin = Tempfile.new(['test', '.mrb'])
  File.write(script.path, 'p ARGV')
  assert_run('mrbc', '-o', bin.path, script.path)
  o, = Open3.capture2(*(cmd_list('mrb') + [bin.path, 'foo', 'bar']))
  assert_equal '["foo", "bar"]', o.strip
end

assert('mrb with no arguments prints error') do
  assert_mrb("", /no program file given/, false, [])
end

assert('mrb --version') do
  assert_mrb(/\Amruby \d+\.\d+/, "", true, %w[--version])
end

assert('mrb -r option loads library') do
  lib = Tempfile.new(['lib', '.rb'])
  main = Tempfile.new(['main', '.rb'])
  lib_mrb = Tempfile.new(['lib', '.mrb'])
  main_mrb = Tempfile.new(['main', '.mrb'])

  File.write(lib.path, '$lib_loaded = true')
  File.write(main.path, 'print $lib_loaded')
  assert_run('mrbc', '-o', lib_mrb.path, lib.path)
  assert_run('mrbc', '-o', main_mrb.path, main.path)
  o, = Open3.capture2(*(cmd_list('mrb') + ['-r', lib_mrb.path, main_mrb.path]))
  assert_equal 'true', o.strip
end

assert('mrb -d sets $DEBUG') do
  script = Tempfile.new(['test', '.rb'])
  bin = Tempfile.new(['test', '.mrb'])
  File.write(script.path, 'print $DEBUG')
  assert_run('mrbc', '-o', bin.path, script.path)
  o, = Open3.capture2(*(cmd_list('mrb') + ['-d', bin.path]))
  assert_equal 'true', o.strip
end

assert('mrb nonexistent file') do
  assert_mrb("", /Cannot open/, false, %w[nonexistent.mrb])
end
