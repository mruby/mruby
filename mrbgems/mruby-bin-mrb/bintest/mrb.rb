require 'tempfile'
require 'open3'
require 'tmpdir'

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

# Every case below needs a program to run, and an empty file is one the tool
# refuses for a reason of its own, so a failed compile has to be reported here
# rather than left to surface as whatever the case asserts.
def mrb_program
  script = Tempfile.new(['test', '.rb'])
  bin = Tempfile.new(['test', '.mrb'])
  File.write(script.path, 'print "prog"')
  assert_true system(*(cmd_list('mrbc') + ['-o', bin.path, script.path])),
              "mrbc could not compile #{script.path}"
  bin
end

assert('a -r library that does not load is fatal') do
  # The library's exception was left on the state and nowhere else, and the
  # program's first successful run overwrote it, so `mrb -r junk prog.mrb`
  # printed the program's output and exited 0 as if the library had loaded.
  # The irep loader has always raised here; only the check was missing.
  bin = mrb_program
  junk = Tempfile.new(['junk', '.mrb'])
  File.write(junk.path, "not an irep\n")
  assert_mrb("", /irep load error/, false, ["-r", junk.path, bin.path])
end

assert('a directory as a -r library is fatal') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and reports that instead.
  skip 'fopen() refuses a directory' if target_win?
  bin = mrb_program
  Dir.mktmpdir do |dir|
    assert_mrb("", /irep load error/, false, ["-r", dir, bin.path])
  end
end

assert('a -r library that loads still runs the program') do
  bin = mrb_program
  lib = mrb_program
  assert_mrb("progprog", "", true, ["-r", lib.path, bin.path])
end
