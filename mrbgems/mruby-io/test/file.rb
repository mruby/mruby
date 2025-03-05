##
# File Test

MRubyIOTestUtil.io_test_setup

def assert_dirname_with_level(path, results)
  assert('dirname with level') do
    assert_raise(ArgumentError) { File.dirname path, -1 }
    results.each do |level, expect|
      assert_equal expect, File.dirname(path, level)
    end
  end
end

assert('File.class', '15.2.21') do
  assert_equal Class, File.class
end

assert('File.superclass', '15.2.21.2') do
  assert_equal IO, File.superclass
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
  assert_equal $mrbtest_io_msg, io.read
  assert_equal $mrbtest_io_rfname, io.path
  io.close
  assert_equal $mrbtest_io_rfname, io.path
  assert_true io.closed?
end

assert('File.basename') do
  assert_equal '/', File.basename('//')
  assert_equal 'a', File.basename('/a/')
  assert_equal 'b', File.basename('/a/b')
  assert_equal 'b', File.basename('../a/b')
  assert_raise(ArgumentError) { File.basename("/a/b\0") }
end

assert('File.dirname') do
  assert_equal '.',    File.dirname('')
  assert_equal '.',    File.dirname('a')
  assert_equal '/',    File.dirname('/a')
  assert_equal '/',    File.dirname('/a/')
  assert_equal 'a',    File.dirname('a/b')
  assert_equal 'a',    File.dirname('a/b/')
  assert_equal 'a/b',  File.dirname('a/b/c')
  assert_equal '/a',   File.dirname('/a/b')
  assert_equal '/a',   File.dirname('/a/b/')
  assert_equal '/a/b', File.dirname('/a/b/c')
  assert_equal '/a/b', File.dirname('/a/b/c/')
  assert_equal '/',    File.dirname('/a//')
  assert_equal '/a',   File.dirname('/a//b')
  assert_equal '/a/b', File.dirname('/a/b//c//')
end

assert('File.dirname (with level)') do
  assert_dirname_with_level '', { 0 => '.', 1 => '.', 2 => '.' }
  assert_dirname_with_level 'a', { 0 => 'a', 1 => '.', 2 => '.' }
  assert_dirname_with_level '/a', { 0 => '/a', 1 => '/', 2 => '/' }
  assert_dirname_with_level '/a/', { 0 => '/a/', 1 => '/', 2 => '/' }
  assert_dirname_with_level 'a/b', { 0 => 'a/b', 1 => 'a', 2 => '.' }
  assert_dirname_with_level 'a/b/', { 0 => 'a/b/', 1 => 'a', 2 => '.' }
  assert_dirname_with_level 'a/b/c', { 0 => 'a/b/c', 1 => 'a/b', 2 => 'a' }
  assert_dirname_with_level 'a/b/c/d', { 0 => 'a/b/c/d', 1 => 'a/b/c', 2 => 'a/b' }
end

unless MRubyIOTestUtil.win?
  assert('File.dirname (not Windows)') do
    assert_equal '/a',   File.dirname('//a//b/')
  end
else
  assert('File.dirname (on Windows)') do
    assert_equal 'c:.',           File.dirname('c:')
    assert_equal 'c:.',           File.dirname('c:a')
    assert_equal 'c:.',           File.dirname('c:a/')
    assert_equal 'c:a',           File.dirname('c:a/b')
    assert_equal 'c:/',           File.dirname('c:/')
    assert_equal 'c:/',           File.dirname('c:/a')
    assert_equal 'c:/',           File.dirname('c:/a/')
    assert_equal 'c:/a',          File.dirname('c:/a/b')
    assert_equal '//.',           File.dirname('//.')
    assert_equal '//.',           File.dirname('//./')
    assert_equal '//./a',         File.dirname('//./a')
    assert_equal '//./a',         File.dirname('//./a/')
    assert_equal '//./a',         File.dirname('//./a/b')
    assert_equal '//./a/b',       File.dirname('//./a/b/c')
    assert_equal '//?',           File.dirname('//?/')
    assert_equal '//?/a',         File.dirname('//?/a')
    assert_equal '//?/a',         File.dirname('//?/a/')
    assert_equal '//?/a',         File.dirname('//?/a/b')
    assert_equal '//host1',       File.dirname('//host1/')
    assert_equal '//host1/a',     File.dirname('//host1/a')
    assert_equal '//host1/a',     File.dirname('//host1/a/')
    assert_equal '//host1/a',     File.dirname('//host1/a/b')
  end
end

assert('File.extname') do
  assert_equal '.txt', File.extname('foo/foo.txt')
  assert_equal '.gz', File.extname('foo/foo.tar.gz')
  assert_equal '', File.extname('foo/bar')
  assert_equal '', File.extname('foo/.bar')
  assert_equal '', File.extname('foo.txt/bar')
  assert_equal '', File.extname('.foo')
  assert_equal '.rb', File.extname('.a.rb')
  assert_equal '.', File.extname('foo.')
end

assert('File#flock') do
  f = File.open $mrbtest_io_rfname
  begin
    assert_equal(f.flock(File::LOCK_SH), 0)
    assert_equal(f.flock(File::LOCK_UN), 0)
    assert_equal(f.flock(File::LOCK_EX | File::LOCK_NB), 0)
    assert_equal(f.flock(File::LOCK_UN), 0)
  rescue NotImplementedError => e
    skip e.message
  ensure
    f.close
  end
end

assert('File#atime') do
  begin
    File.open("#{$mrbtest_io_wfname}.atime", 'w') do |f|
      assert_equal Time, f.mtime.class
      File.open("#{$mrbtest_io_wfname}.atime", 'r') do |f2|
        assert_equal true, f.atime == f2.atime
      end
    end
  ensure
    File.delete("#{$mrbtest_io_wfname}.atime")
  end
end

assert('File#ctime') do
  begin
    File.open("#{$mrbtest_io_wfname}.ctime", 'w') do |f|
      assert_equal Time, f.ctime.class
      File.open("#{$mrbtest_io_wfname}.ctime", 'r') do |f2|
        assert_equal true, f.ctime == f2.ctime
      end
    end
  ensure
    File.delete("#{$mrbtest_io_wfname}.ctime")
  end
end

assert('File#mtime') do
  begin
    File.open("#{$mrbtest_io_wfname}.mtime", 'w') do |f|
      assert_equal Time, f.mtime.class
      File.open("#{$mrbtest_io_wfname}.mtime", 'r') do |f2|
        assert_equal true, f.mtime == f2.mtime
      end
    end
  ensure
    File.delete("#{$mrbtest_io_wfname}.mtime")
  end
end

assert('File#size and File#truncate') do
  fname = "#{$mrbtest_io_wfname}.resize"
  begin
    File.open(fname, 'w') do |f|
      assert_equal 0, f.size
      assert_equal 0, f.truncate(100)
      assert_equal 100, f.size
      assert_equal 0, f.pos
      assert_equal 0, f.truncate(5)
      assert_equal 5, f.size
    end
  ensure
    File.delete(fname)
  end
end

assert('File.join') do
  assert_equal "", File.join()
  assert_equal "a", File.join("a")
  assert_equal "/a", File.join("/a")
  assert_equal "a/", File.join("a/")
  assert_equal "a/b/c", File.join("a", "b", "c")
  assert_equal "/a/b/c", File.join("/a", "b", "c")
  assert_equal "a/b/c/", File.join("a", "b", "c/")
  assert_equal "a/b/c", File.join("a/", "/b/", "/c")
  assert_equal "a/b/c", File.join(["a", "b", "c"])
  assert_equal "a/b/c", File.join("a", ["b", ["c"]])
end

assert('File.realpath') do
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    sep = File::ALT_SEPARATOR || File::SEPARATOR
    relative_path = "#{File.basename(dir)}#{sep}realpath_test"
    path = "#{MRubyIOTestUtil.getwd}#{sep}#{relative_path}"
    File.open(path, "w"){}
    assert_equal path, File.realpath(relative_path)

    unless MRubyIOTestUtil.win?
      path1 = File.realpath($mrbtest_io_rfname)
      path2 = File.realpath($mrbtest_io_symlinkname)
      assert_equal path1, path2
    end
  ensure
    File.delete path rescue nil
    MRubyIOTestUtil.rmdir dir
  end

  assert_raise(ArgumentError) { File.realpath("TO\0DO") }
end

assert("File.readlink") do
  begin
    exp = File.basename($mrbtest_io_rfname)
    act = File.readlink($mrbtest_io_symlinkname)
    assert_equal exp, act
  rescue NotImplementedError => e
    skip e.message
  end
end

assert("File.readlink fails with non-symlink") do
  skip "readlink is not supported on this platform" if MRubyIOTestUtil.win?
  begin
    e2 = nil
    assert_raise(RuntimeError) {
      begin
        File.readlink($mrbtest_io_rfname)
      rescue => e
        if Object.const_defined?(:SystemCallError) and e.kind_of?(SystemCallError)
          raise RuntimeError, "SystemCallError converted to RuntimeError"
        end
        raise e
      rescue NotImplementedError => e
        e2 = e
      end
    }
    raise e2 if e2
  rescue NotImplementedError => e
    skip e.message
  end
end

if MRubyIOTestUtil.win?
  assert('File.expand_path (for windows)') do
    drive = MRubyIOTestUtil.getwd[0, 2]
    alt1 = (drive.downcase != "c:") ? "c:" : "x:"
    alt2 = (drive.downcase != "d:") ? "d:" : "y:"

    assert_equal "#{drive}/",    File.expand_path("..", "/tmp"),       "parent path with base_dir (1)"
    assert_equal "#{drive}/tmp", File.expand_path("..", "/tmp/mruby"), "parent path with base_dir (2)"

    assert_equal "#{drive}/home", File.expand_path("/home"),      "absolute"
    assert_equal "#{drive}/home", File.expand_path("/home", "."), "absolute with base_dir"

    assert_equal "#{drive}/hoge", File.expand_path("/tmp/..//hoge")
    assert_equal "//tmp/hoge", File.expand_path("////tmp/..///////hoge")

    assert_equal "#{drive}/", File.expand_path("../../../..", "/")

    assert_equal "#{drive}/", File.expand_path(([".."] * 100).join("/"))

    assert_equal "#{alt1}/x/y", File.expand_path("#{alt1}y", "/x")
    assert_equal "#{alt2}/x/y", File.expand_path("#{alt2}y", "/x")
    assert_equal "#{alt1}/x", File.expand_path("#{alt1}x", "#{alt2}y")
    assert_equal "#{alt1}/y/x", File.expand_path("#{alt1}x", "./y")
  end
else
  assert('File.expand_path') do
    assert_equal "/",    File.expand_path("..", "/tmp"),       "parent path with base_dir (1)"
    assert_equal "/tmp", File.expand_path("..", "/tmp/mruby"), "parent path with base_dir (2)"

    assert_equal "/home", File.expand_path("/home"),      "absolute"
    assert_equal "/home", File.expand_path("/home", "."), "absolute with base_dir"

    assert_equal "/hoge", File.expand_path("/tmp/..//hoge")
    assert_equal "/hoge", File.expand_path("////tmp/..///////hoge")

    assert_equal "/", File.expand_path("../../../..", "/")

    assert_equal "/", File.expand_path(([".."] * 100).join("/"))

    assert_equal "/x/c:y", File.expand_path("c:y", "/x")
  end
end

assert('File.expand_path (with getenv(3))') do
  skip unless MRubyIOTestUtil.const_defined?(:ENV_HOME)

  assert_equal MRubyIOTestUtil::ENV_HOME, File.expand_path("~/"),      "home"
  assert_equal MRubyIOTestUtil::ENV_HOME, File.expand_path("~/", "/"), "home with base_dir"

  assert_equal "#{MRubyIOTestUtil::ENV_HOME}/user", File.expand_path("user", MRubyIOTestUtil::ENV_HOME), "relative with base_dir"
end

if MRubyIOTestUtil.win?
  assert('File.absolute_path? (for windows)') do
    assert_true File.absolute_path?("c:/")
    assert_true File.absolute_path?("c:/a")
    assert_true File.absolute_path?("//")
    assert_true File.absolute_path?("//?")
    assert_true File.absolute_path?("//?/")
    assert_false File.absolute_path?("")
    assert_false File.absolute_path?("/")
    assert_false File.absolute_path?("c:")
  end
else
  assert('File.absolute_path?') do
    assert_true File.absolute_path?("/")
    assert_true File.absolute_path?("/a")
    assert_true File.absolute_path?("/a/b/")
    assert_false File.absolute_path?("")
    assert_false File.absolute_path?("a")
    assert_false File.absolute_path?("a/b/")
    assert_false File.absolute_path?("c:/")
  end
end

assert('File.path') do
  assert_equal "", File.path("")
  assert_equal "a/b/c", File.path("a/b/c")
  assert_equal "a/../b/./c", File.path("a/../b/./c")
  assert_raise(TypeError) { File.path(nil) }
  assert_raise(TypeError) { File.path(123) }
end

assert('File.symlink') do
  target_name = "/usr/bin"
  if !File.exist?(target_name)
    skip("target directory of File.symlink is not found")
  end

  begin
    tmpdir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  rescue => e
    skip e.message
  end

  symlink_name = "#{tmpdir}/test-bin-dummy"
  begin
    assert_equal 0, File.symlink(target_name, symlink_name)
    assert_equal true, File.symlink?(symlink_name)
  rescue NotImplementedError => e
    skip e.message
  ensure
    File.delete symlink_name rescue nil
    MRubyIOTestUtil.rmdir tmpdir rescue nil
  end
end

assert('File.chmod') do
  File.open("#{$mrbtest_io_wfname}.chmod-test", 'w') {}
  begin
    assert_equal 1, File.chmod(0400, "#{$mrbtest_io_wfname}.chmod-test")
  ensure
    File.delete("#{$mrbtest_io_wfname}.chmod-test")
  end
end

assert('File.open with "x" mode') do
  File.unlink $mrbtest_io_wfname rescue nil
  assert_nothing_raised do
    File.open($mrbtest_io_wfname, "wx") {}
  end
  assert_raise(RuntimeError) do
    File.open($mrbtest_io_wfname, "wx") {}
  end

  File.unlink $mrbtest_io_wfname rescue nil
  assert_nothing_raised do
    File.open($mrbtest_io_wfname, "w+x") {}
  end
  assert_raise(RuntimeError) do
    File.open($mrbtest_io_wfname, "w+x") {}
  end

  assert_raise(ArgumentError) do
    File.open($mrbtest_io_wfname, "rx") {}
  end

  assert_raise(ArgumentError) do
    File.open($mrbtest_io_wfname, "ax") {}
  end
end

MRubyIOTestUtil.io_test_cleanup
