require 'open3'
require 'tempfile'
require 'tmpdir'

class BinTest_MRubyBinDebugger
  @debug1=false
  @debug2=true
  @debug3=true
  def self.test(rubysource, testcase)
    script, bin = Tempfile.new(['test', '.rb']), Tempfile.new(['test', '.mrb'])

    # .rb
    script.write rubysource
    script.flush

    # compile
    assert_run('mrbc', '-g', '-o', bin.path, script.path)

    # add mrdb quit
    testcase << {:cmd=>"quit"}

    stdin_data = testcase.map{|t| t[:cmd]}.join("\n") << "\n"

    # Both arms of the same program: the source, and the compiled form of it.
    [[script.path], ['-b', bin.path]].each do |args|
      o, s = Open3.capture2(*(cmd_list('mrdb') + args), :stdin_data => stdin_data)

      exp_vals = testcase.map{|t| t.fetch(:exp, nil)}
      unexp_vals = testcase.map{|t| t.fetch(:unexp, nil)}

if @debug1
  o.split("\n").each_with_index do |i,actual|
    p [i,actual]
  end
end
      # compare actual / expected
      o.split("\n").each do |actual|
        next if actual.empty?
        exp = exp_vals.shift
if @debug2
  a = true
  a = actual.include?(exp) unless exp.nil?
  p [actual, exp] unless a
end
        assert_true actual.include?(exp) unless exp.nil?
      end
      # compare actual / unexpected
      o.split("\n").each do |actual|
        next if actual.empty?
        unexp = unexp_vals.shift
if @debug3
  a = false
  a = actual.include?(unexp) unless unexp.nil?
  p [actual, unexp] if a
end
        assert_false actual.include?(unexp) unless unexp.nil?
      end
    end
  end
end

INVCMD = "invalid command"

assert('mruby-bin-debugger(mrdb) command line') do
  # ruby source
  src = "foo = 'foo'\n"

  str = ":#{'abcdefghij' * 103}"
  cmd = "p a=#{str}"

  # test case
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>cmd[0...1023], :unexp=>'command line too long.'}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>cmd[0...1024], :unexp=>'command line too long.'}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>cmd[0...1025], :exp=>'command line too long.'}])
end

assert('mruby-bin-debugger(mrdb) command: "break"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"b",     :unexp=>INVCMD}
  tc << {:cmd=>"br",    :unexp=>INVCMD}
  tc << {:cmd=>"brea",  :unexp=>INVCMD}
  tc << {:cmd=>"break", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"bl",     :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"breaka", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "continue"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"c",         :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"co",        :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"continu",   :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"continue",  :unexp=>INVCMD}])

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"cn",        :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"continuee", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "delete"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"d 1",      :unexp=>INVCMD}
  tc << {:cmd=>"de 1",     :unexp=>INVCMD}
  tc << {:cmd=>"delet 1",  :unexp=>INVCMD}
  tc << {:cmd=>"delete 1", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"dd 1",      :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"deletee 1", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "disable"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"dis",     :unexp=>INVCMD}
  tc << {:cmd=>"disa",    :unexp=>INVCMD}
  tc << {:cmd=>"disabl",  :unexp=>INVCMD}
  tc << {:cmd=>"disable", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"di",       :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"disb",     :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"disablee", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "enable"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"en",     :unexp=>INVCMD}
  tc << {:cmd=>"ena",    :unexp=>INVCMD}
  tc << {:cmd=>"enabl",  :unexp=>INVCMD}
  tc << {:cmd=>"enable", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"e",       :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"enb",     :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"enablee", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "eval"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"ev",   :unexp=>INVCMD}
  tc << {:cmd=>"eva",  :unexp=>INVCMD}
  tc << {:cmd=>"eval", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"e",     :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"evl",   :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"evall", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "help"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"h",    :unexp=>INVCMD}
  tc << {:cmd=>"he",   :unexp=>INVCMD}
  tc << {:cmd=>"hel",  :unexp=>INVCMD}
  tc << {:cmd=>"help", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"hl",    :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"helpp", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "info breakpoints"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"i b",              :unexp=>INVCMD}
  tc << {:cmd=>"in  b",            :unexp=>INVCMD}
  tc << {:cmd=>"i    br",          :unexp=>INVCMD}
  tc << {:cmd=>"inf breakpoint",   :unexp=>INVCMD}
  tc << {:cmd=>"info breakpoints", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"ii b",              :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"i bb",              :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"infoo breakpoints", :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"info breakpointss", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "list"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"l",    :unexp=>INVCMD}
  tc << {:cmd=>"li",   :unexp=>INVCMD}
  tc << {:cmd=>"lis",  :unexp=>INVCMD}
  tc << {:cmd=>"list", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"ll",    :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"listt", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "print"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  tc = []
  tc << {:cmd=>"p",     :unexp=>INVCMD}
  tc << {:cmd=>"pr",    :unexp=>INVCMD}
  tc << {:cmd=>"prin",  :unexp=>INVCMD}
  tc << {:cmd=>"print", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"pp",     :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"printt", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "quit"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"q",    :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"qu",   :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"qui",  :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"quit", :unexp=>INVCMD}])

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"qq",    :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"quitt", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "run"') do
  # ruby source
  src = "foo = 'foo'\n"

  # test case
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"r",   :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"ru",  :unexp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"run", :unexp=>INVCMD}])

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"rr",   :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"runn", :exp=>INVCMD}])
end

assert('mruby-bin-debugger(mrdb) command: "step"') do
  # ruby source
  src = <<"SRC"
while true
  foo = 'foo'
end
SRC

  # test case
  tc = []
  tc << {:cmd=>"s",    :unexp=>INVCMD}
  tc << {:cmd=>"st",   :unexp=>INVCMD}
  tc << {:cmd=>"ste",  :unexp=>INVCMD}
  tc << {:cmd=>"step", :unexp=>INVCMD}
  BinTest_MRubyBinDebugger.test(src, tc)

  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"ss",    :exp=>INVCMD}])
  BinTest_MRubyBinDebugger.test(src, [{:cmd=>"stepp", :exp=>INVCMD}])
end

assert('a directory as the program file is refused') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and reports that instead.
  skip 'fopen() refuses a directory' if target_win?
  # A directory opens for reading and then fails every read, and both loaders
  # answer without raising, so without a check the debugger started on an
  # empty program and exited 0.  Both arms take the same fopen().
  Dir.mktmpdir do |dir|
    [[dir], ["-b", dir]].each do |args|
      o, s = Open3.capture2(*(cmd_list('mrdb') + args), :stdin_data => "")
      assert_false s.success?
      assert_include o, "Cannot read program file. (#{dir})"
    end
  end
end

# Builds ROOT/src/prog.rb and ROOT/prog.mrb (compiled with -g, so its debug
# info names ROOT/src/prog.rb), and yields the two paths.
def with_debuggable_program
  Dir.mktmpdir do |root|
    src = File.join(root, 'src')
    Dir.mkdir(src)
    rb = File.join(src, 'prog.rb')
    File.write(rb, "a = 1\nb = 2\n")
    bin = File.join(root, 'prog.mrb')
    assert_run('mrbc', '-g', '-o', bin, rb)
    yield root, rb, bin
  end
end

def mrdb_list(args)
  o, _ = Open3.capture2(*(cmd_list('mrdb') + args), :stdin_data => "l\nq\n")
  o
end

assert('the source search passes over a directory') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and never reaches the check this covers.
  skip 'fopen() refuses a directory' if target_win?
  # fopen() alone was the existence test, so a directory named like the source
  # was a hit: the search stopped on a path `list` cannot show, and the
  # readable file next in the search order was never reached.
  with_debuggable_program do |root, rb, bin|
    decoy = File.join(root, 'decoy')
    Dir.mkdir(decoy)
    Dir.mkdir(File.join(decoy, 'prog.rb'))

    o = mrdb_list(['-d', decoy, '-b', bin])
    assert_include o, 'a = 1'
    assert_include o, 'b = 2'
  end
end

assert('list reports a source it cannot read') do
  skip 'fopen() refuses a directory' if target_win?
  # With nothing readable anywhere in the search order, source_file_new()
  # still answered a handle for the directory, show_lines() printed nothing
  # and mrb_debug_list() answered OK, so `list` was silent instead of reaching
  # its own message.
  with_debuggable_program do |root, rb, bin|
    File.delete(rb)
    Dir.mkdir(rb)

    o = mrdb_list(['-d', File.dirname(rb), '-b', bin])
    assert_include o, 'Invalid source file named'
  end
end

def mrdb_status(args, stdin_data)
  _, _, s = Open3.capture3(*(cmd_list('mrdb') + args), :stdin_data => stdin_data)
  s
end

assert('the exit status reports how the program ended') do
  # The command loop decided the status by itself, with an unconditional 0, so
  # `mrdb prog.rb` answered success for a program that raised and for one that
  # would not even compile, where `mruby prog.rb` answers 1 for both.
  script = Tempfile.new(['test', '.rb'])

  File.write(script.path, "puts 'ok'\n")
  assert_true mrdb_status([script.path], "r\nq\n").success?

  File.write(script.path, "raise 'boom'\n")
  assert_false mrdb_status([script.path], "r\nq\n").success?

  # A program that does not compile never runs, and the failed load is still
  # the program's outcome.
  File.write(script.path, "def f(\n")
  assert_false mrdb_status([script.path], "q\n").success?
end

assert('the exit status reports a compiled program too') do
  script, bin = Tempfile.new(['test', '.rb']), Tempfile.new(['test', '.mrb'])

  File.write(script.path, "raise 'boom'\n")
  assert_run('mrbc', '-g', '-o', bin.path, script.path)
  assert_false mrdb_status(['-b', bin.path], "r\nq\n").success?

  File.write(script.path, "puts 'ok'\n")
  assert_run('mrbc', '-g', '-o', bin.path, script.path)
  assert_true mrdb_status(['-b', bin.path], "r\nq\n").success?
end

assert('quitting before the program runs is not a failure') do
  # Leaving the debugger without running is the user's own exit, not the
  # program's, and it returns through DebuggerExit rather than the path above.
  script = Tempfile.new(['test', '.rb'])
  File.write(script.path, "raise 'boom'\n")
  assert_true mrdb_status([script.path], "q\n").success?
end
