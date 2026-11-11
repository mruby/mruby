##
# FileTest

MRubyIOTestUtil.io_test_setup

assert("FileTest.directory?") do
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    assert_true  FileTest.directory?(dir)
    assert_false FileTest.directory?($mrbtest_io_rfname)
  ensure
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest trailing path separators") do
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    paths = [dir + "/", dir + "///", dir + "/./", dir + "/../"]
    if MRubyIOTestUtil.win?
      paths << dir + "\\"
      paths << dir + "/\\\\"
    end
    paths.each do |path|
      assert_true File.directory?(path), path
      assert_true FileTest.directory?(path), path
      assert_true File.exist?(path), path
      assert_false File.file?(path), path
    end

    missing = dir + "/missing/"
    missing_paths = [missing]
    missing_paths << dir + "\\missing\\" if MRubyIOTestUtil.win?
    missing_paths.each do |path|
      assert_false File.directory?(path)
      assert_false File.exist?(path)
      assert_false File.file?(path)
    end

    file = $mrbtest_io_rfname + "/"
    file_paths = [file]
    file_paths << $mrbtest_io_rfname + "\\" if MRubyIOTestUtil.win?
    file_paths.each do |path|
      assert_false File.directory?(path)
      assert_false File.exist?(path)
      assert_false File.file?(path)
    end

    frozen_path = dir + "/"
    frozen_dir = frozen_path.freeze
    assert_true File.directory?(frozen_dir)
    assert_true File.exist?(frozen_dir)
    assert_false File.file?(frozen_dir)
    assert_equal dir + "/", frozen_dir

    root = File.expand_path("/")
    assert_true File.directory?(root)
    assert_true File.directory?(root + "/")
    assert_true File.directory?(File.dirname(dir) + "/")
    assert_true File.directory?(root + "\\") if MRubyIOTestUtil.win?
  ensure
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest trailing separators with a DBCS trail byte") do
  skip 'requires Windows code page 932' unless MRubyIOTestUtil.win? && MRubyIOTestUtil::WINDOWS_CODEPAGE == 932

  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  japanese_dir = dir + "/表"
  created = false
  begin
    MRubyIOTestUtil.mkdir(japanese_dir)
    created = true
    paths = [japanese_dir, japanese_dir + "/", japanese_dir + "\\"]
    paths.each do |path|
      assert_true File.directory?(path), path
      assert_true FileTest.directory?(path), path
      assert_true File.exist?(path), path
      assert_false File.file?(path), path
    end
  ensure
    MRubyIOTestUtil.rmdir japanese_dir if created
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest.exist?") do
  assert_equal true,  FileTest.exist?($mrbtest_io_rfname), "filename - exist"
  assert_equal false, FileTest.exist?($mrbtest_io_rfname + "-"), "filename - not exist"
  io = IO.new(IO.sysopen($mrbtest_io_rfname))
  assert_equal true,  FileTest.exist?(io), "io obj - exist"
  io.close
  assert_equal true, io.closed?
  assert_raise(IOError) { FileTest.exist?(io) }
  assert_raise(TypeError) { File.exist?($mrbtest_io_rfname.to_sym) }
end

assert("FileTest.file?") do
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    assert_true  FileTest.file?($mrbtest_io_rfname)
    assert_false FileTest.file?(dir)
  ensure
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest.pipe?") do
  skip 'pipe? is not implemented on this platform' unless FileTest.respond_to?(:pipe?)
  assert_equal false, FileTest.pipe?("/tmp")
  begin
    io = IO.popen("ls")
  rescue NotImplementedError => e
    skip e.message
  end
  assert_equal true,  FileTest.pipe?(io)
end

assert('FileTest.size') do
  assert_equal FileTest.size($mrbtest_io_rfname), $mrbtest_io_msg.size
  assert_equal FileTest.size($mrbtest_io_wfname), 0
end

assert("FileTest.size?") do
  assert_equal $mrbtest_io_msg.size, FileTest.size?($mrbtest_io_rfname)
  assert_equal nil, FileTest.size?($mrbtest_io_wfname)
  assert_equal nil, FileTest.size?("not-exist-test-target-file")

  fp1 = File.open($mrbtest_io_rfname)
  fp2 = File.open($mrbtest_io_wfname)
  assert_equal $mrbtest_io_msg.size,  FileTest.size?(fp1)
  assert_equal nil, FileTest.size?(fp2)
  fp1.close
  fp2.close

  assert_raise IOError do
    FileTest.size?(fp1)
  end
  assert_true fp1.closed?
  assert_raise IOError do
    FileTest.size?(fp2)
  end
  assert_true fp2.closed?
end

assert("FileTest.socket?") do
  skip 'socket? is not implemented on this platform' unless FileTest.respond_to?(:socket?)
  assert_true FileTest.socket?($mrbtest_io_socketname)
end

assert("FileTest.symlink?") do
  skip 'symlink? is not implemented on this platform' unless FileTest.respond_to?(:symlink?)
  assert_true FileTest.symlink?($mrbtest_io_symlinkname)
end

assert("FileTest.readable?, writable? and executable?") do
  assert_true  FileTest.readable?($mrbtest_io_rfname)
  assert_true  File.readable?($mrbtest_io_rfname)
  assert_true  FileTest.writable?($mrbtest_io_wfname)
  assert_true  File.writable?($mrbtest_io_wfname)
  assert_false FileTest.readable?("not-exist-test-target-file")
  assert_false FileTest.writable?("not-exist-test-target-file")
  assert_false FileTest.executable?("not-exist-test-target-file")

  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    # a directory is searchable
    assert_true File.executable?(dir)
    name = dir + "/run.bat"
    File.open(name, "w") {}
    begin
      if MRubyIOTestUtil.win?
        # Windows runs a file by its extension
        assert_true File.executable?(name)
      else
        File.chmod(0644, name)
        assert_false File.executable?(name)
        File.chmod(0755, name)
        assert_true File.executable?(name)
      end
      assert_true File.readable?(name)
    ensure
      File.delete(name)
    end
  ensure
    MRubyIOTestUtil.rmdir dir
  end

  # a File, by its path
  File.open($mrbtest_io_rfname) do |f|
    assert_true File.readable?(f)
  end
end

assert("FileTest.zero?") do
  assert_equal false, FileTest.zero?($mrbtest_io_rfname)
  assert_equal true,  FileTest.zero?($mrbtest_io_wfname)
  assert_equal false, FileTest.zero?("not-exist-test-target-file")

  fp1 = File.open($mrbtest_io_rfname)
  fp2 = File.open($mrbtest_io_wfname)
  assert_equal false, FileTest.zero?(fp1)
  assert_equal true,  FileTest.zero?(fp2)
  fp1.close
  fp2.close

  assert_raise IOError do
    FileTest.zero?(fp1)
  end
  assert_true fp1.closed?
  assert_raise IOError do
    FileTest.zero?(fp2)
  end
  assert_true fp2.closed?
end

assert("File.ftype") do
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  f = "#{dir}/f"
  l = "#{dir}/l"
  begin
    File.open(f, "w") { |io| io.write "x" }
    assert_equal "file", File.ftype(f)
    assert_equal "directory", File.ftype(dir)
    File.open(f) { |io| assert_equal "file", File.ftype(io) }
    if File.respond_to?(:symlink)
      File.symlink(f, l)
      assert_equal "link", File.ftype(l)
    end
    assert_equal "characterSpecial", File.ftype("/dev/null") if File.exist?("/dev/null")
    assert_raise(SystemCallError) { File.ftype("#{dir}/none") }
  ensure
    File.delete(l) rescue nil
    File.delete(f) rescue nil
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest.blockdev?, chardev?, setuid?, setgid?, sticky?") do
  assert_false FileTest.blockdev?($mrbtest_io_rfname)
  assert_false FileTest.chardev?($mrbtest_io_rfname)
  assert_true FileTest.chardev?("/dev/null") if File.exist?("/dev/null")
  assert_false FileTest.setuid?($mrbtest_io_rfname)
  assert_false FileTest.setgid?($mrbtest_io_rfname)
  assert_false FileTest.sticky?($mrbtest_io_rfname)
  assert_false FileTest.chardev?("/no/such/file")
  assert_false FileTest.sticky?("/no/such/file")
end

assert("FileTest.world_readable? and world_writable?") do
  skip "no permission bits for others" if MRubyIOTestUtil.win?
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  f = "#{dir}/f"
  begin
    File.open(f, "w") {}
    File.chmod(0644, f)
    assert_equal 0644, FileTest.world_readable?(f)
    assert_nil FileTest.world_writable?(f)
    File.chmod(0600, f)
    assert_nil FileTest.world_readable?(f)
    assert_nil FileTest.world_readable?("#{dir}/none")
  ensure
    File.delete(f) rescue nil
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest.empty?") do
  assert_false FileTest.empty?($mrbtest_io_rfname)
  assert_false FileTest.empty?("/no/such/file")
end

assert("FileTest.readable_real?, writable_real?, executable_real?") do
  assert_true FileTest.readable_real?($mrbtest_io_rfname)
  assert_true FileTest.writable_real?($mrbtest_io_wfname)
  assert_false FileTest.readable_real?("/no/such/file")
  dir = MRubyIOTestUtil.mkdtemp("mruby-io-test.XXXXXX")
  begin
    assert_true FileTest.executable_real?(dir)
  ensure
    MRubyIOTestUtil.rmdir dir
  end
end

assert("FileTest.owned? and grpowned?") do
  skip "the port has no owner" unless FileTest.respond_to?(:owned?)
  assert_true FileTest.owned?($mrbtest_io_wfname)
  assert_false FileTest.owned?("/no/such/file")
  assert_false FileTest.grpowned?("/no/such/file")
end

assert("FileTest.identical?") do
  skip "the port has no inode" unless FileTest.respond_to?(:identical?)
  assert_true FileTest.identical?($mrbtest_io_rfname, $mrbtest_io_rfname)
  assert_false FileTest.identical?($mrbtest_io_rfname, $mrbtest_io_wfname)
  assert_false FileTest.identical?($mrbtest_io_rfname, "/no/such/file")
end

MRubyIOTestUtil.io_test_cleanup
