##
# File.ftype and the FileTest predicates of mruby-io-ftype

def ftype_test_files
  dir = FtypeTest.mkdtemp
  r = "#{dir}/r"
  w = "#{dir}/w"
  begin
    File.open(r, "w") { |io| io.write "x" }
    File.open(w, "w") {}
    yield dir, r, w
  ensure
    File.delete(r) rescue nil
    File.delete(w) rescue nil
    FtypeTest.rmdir dir
  end
end

assert("File.ftype") do
  ftype_test_files do |dir, r, w|
    l = "#{dir}/l"
    begin
      assert_equal "file", File.ftype(r)
      assert_equal "directory", File.ftype(dir)
      File.open(r) { |io| assert_equal "file", File.ftype(io) }
      if File.respond_to?(:symlink)
        File.symlink(r, l)
        assert_equal "link", File.ftype(l)
      end
      assert_equal "characterSpecial", File.ftype("/dev/null") if File.exist?("/dev/null")
      assert_raise(SystemCallError) { File.ftype("#{dir}/none") }
    ensure
      File.delete(l) rescue nil
    end
  end
end

assert("FileTest.blockdev?, chardev?, setuid?, setgid?, sticky?") do
  ftype_test_files do |dir, r, w|
    assert_false FileTest.blockdev?(r)
    assert_false FileTest.chardev?(r)
    assert_true FileTest.chardev?("/dev/null") if File.exist?("/dev/null")
    assert_false FileTest.setuid?(r)
    assert_false FileTest.setgid?(r)
    assert_false FileTest.sticky?(r)
    assert_false FileTest.chardev?("#{dir}/none")
    assert_false FileTest.sticky?("#{dir}/none")
    assert_false File.sticky?(r)
  end
end

assert("FileTest.world_readable? and world_writable?") do
  skip "no permission bits for others" if FtypeTest.win?
  ftype_test_files do |dir, r, w|
    File.chmod(0644, r)
    assert_equal 0644, FileTest.world_readable?(r)
    assert_nil FileTest.world_writable?(r)
    File.chmod(0600, r)
    assert_nil FileTest.world_readable?(r)
    assert_nil File.world_readable?("#{dir}/none")
  end
end

assert("FileTest.empty?") do
  ftype_test_files do |dir, r, w|
    assert_false FileTest.empty?(r)
    assert_true FileTest.empty?(w)
    assert_true File.empty?(w)
    assert_false FileTest.empty?("#{dir}/none")
  end
end

assert("FileTest.readable_real?, writable_real?, executable_real?") do
  ftype_test_files do |dir, r, w|
    assert_true FileTest.readable_real?(r)
    assert_true FileTest.writable_real?(w)
    assert_false FileTest.readable_real?("#{dir}/none")
    assert_true FileTest.executable_real?(dir)
  end
end

assert("FileTest.owned? and grpowned?") do
  skip "the port has no owner" unless FileTest.respond_to?(:owned?)
  ftype_test_files do |dir, r, w|
    assert_true FileTest.owned?(w)
    assert_false FileTest.owned?("#{dir}/none")
    assert_false FileTest.grpowned?("#{dir}/none")
  end
end

assert("FileTest.identical?") do
  skip "the port has no inode" unless FileTest.respond_to?(:identical?)
  ftype_test_files do |dir, r, w|
    assert_true FileTest.identical?(r, r)
    assert_false FileTest.identical?(r, w)
    assert_false File.identical?(r, "#{dir}/none")
  end
end
