require 'open3'
require 'tempfile'
require 'tmpdir'

MIRB_BIN = "mirb"

assert('mirb normal operations') do
  o, s = Open3.capture2(*cmd_list(MIRB_BIN), :stdin_data => "a=1\nb=2\na+b\n")
  assert_true o.include?('=> 3')
  assert_true o.include?('=> 2')
end

assert('mirb multi-line') do
  o, s = Open3.capture2(*cmd_list(MIRB_BIN), :stdin_data => "def a(b)\n return b\n end\na(1)\n")
  assert_true o.include?('=> :a')
  assert_true o.include?('=> 1')
end

assert('regression for #1563') do
  o, s = Open3.capture2(*cmd_list(MIRB_BIN), :stdin_data => "a=1;b=2;c=3\nb\nc")
  assert_true o.include?('=> 3')
end

assert('mirb -d option') do
  o, _ = Open3.capture2(*cmd_list(MIRB_BIN), :stdin_data => "$DEBUG\n")
  assert_true o.include?('=> false')
  o, _ = Open3.capture2(*(cmd_list(MIRB_BIN) + ['-d']), :stdin_data => "$DEBUG\n")
  assert_true o.include?('=> true')
end

assert('mirb -r option') do
  lib = Tempfile.new('lib.rb')
  lib.write <<EOS
class Hoge
  def hoge
    :hoge
  end
end
EOS
  lib.flush

  o, _ = Open3.capture2(*(cmd_list(MIRB_BIN) + ['-r', lib.path]), :stdin_data => "Hoge.new.hoge\n")
  assert_true o.include?('=> :hoge')
end

assert('top level local variables are in file scope') do
  lib = Tempfile.new('lib.rb')
  lib.write <<-TESTLIB
a = 1
A = -> { a }
  TESTLIB
  lib.flush

  o, _ = Open3.capture2(*(cmd_list(MIRB_BIN) + ['-r', lib.path]), :stdin_data => <<-TESTCODE)
a
a = 5
A.call
  TESTCODE

  assert_kind_of Integer, o =~ /\bundefined method 'a' .*\(NoMethodError\).*=> 5\b.*=> 1\b/m
end

assert('a directory as the program file is refused') do
  # Only POSIX systems open a directory for reading; Windows refuses it at
  # fopen() and reports that instead.
  skip 'fopen() refuses a directory' if target_win?
  # A directory opens for reading and then fails every read, and the REPL loop
  # takes that failure for end of input, so without a check it ran as an empty
  # program: no diagnostic and exit status 0.
  Dir.mktmpdir do |dir|
    o, s = Open3.capture2(*(cmd_list(MIRB_BIN) + [dir]), :stdin_data => "")
    assert_false s.success?
    assert_include o, "Cannot read program file. (#{dir})"
  end
end

assert('a directory as a library file is refused') do
  skip 'fopen() refuses a directory' if target_win?
  # -r took the same swallowed read (mrb_load_file_cxt() sets no exception for
  # it) and went on into the REPL.
  Dir.mktmpdir do |dir|
    o, s = Open3.capture2(*(cmd_list(MIRB_BIN) + ["-r", dir]), :stdin_data => "")
    assert_false s.success?
    assert_include o, "Cannot read library file. (#{dir})"
  end
end
