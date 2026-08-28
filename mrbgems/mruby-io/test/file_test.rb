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

MRubyIOTestUtil.io_test_cleanup
