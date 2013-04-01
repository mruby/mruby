##
# IO Test

assert('File', '15.2.21') do
  File.class == Class
end

assert('File', '15.2.21.2') do
  File.superclass == IO
end

assert('File TEST SETUP') do
  MRubyIOTestUtil.io_test_setup
end

assert('File#initialize', '15.2.21.4.1') do
  io = File.open($mrbtest_io_rfname, "r")
  assert_nil io.close
  assert_raise IOError do
    io.close
  end
end

assert('File#path', '15.2.21.4.2') do
  io = File.open($mrbtest_io_rfname, "r")
  assert_equal $mrbtest_io_msg + "\n", io.read
  assert_equal $mrbtest_io_rfname, io.path
  io.close
  assert_equal $mrbtest_io_rfname, io.path
  io.closed?
end

assert('File.dirname') do
  path = File.dirname("filename")
  "." == path
end

assert('File.basename') do
  name = File.basename("../somewhere/filename")
  name == "filename"
end

assert('File.extname') do
  assert_equal '.txt', File.extname('foo/foo.txt')
  assert_equal '.gz',  File.extname('foo/foo.tar.gz')
  assert_equal '', File.extname('foo/bar')
  assert_equal '', File.extname('foo/.bar')
  assert_equal '', File.extname('foo.txt/bar')
  assert_equal '', File.extname('.foo')
end

assert('File.size') do
  File.size($mrbtest_io_rfname) == $mrbtest_io_msg.size + 1  and
  File.size($mrbtest_io_wfname) == 0
end

assert('File.join') do
  File.join() == "" and
  File.join("a") == "a" and
  File.join("/a") == "/a" and
  File.join("a/") == "a/" and
  File.join("a", "b", "c") == "a/b/c" and
  File.join("/a", "b", "c") == "/a/b/c" and
  File.join("a", "b", "c/") == "a/b/c/" and
  File.join("a/", "/b/", "/c") == "a/b/c"
end

assert('File.realpath') do
  usrbin = IO.popen("cd bin; /bin/pwd -P") { |f| f.read.chomp }
  assert_equal usrbin, File.realpath("bin")
end

assert('File TEST CLEANUP') do
  assert_nil MRubyIOTestUtil.io_test_cleanup
end
