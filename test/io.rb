##
# IO Test

assert('IO', '15.2.20') do
  assert_equal(Class, IO.class)
end

assert('IO', '15.2.20.2') do
  assert_equal(Object, IO.superclass)
end

assert('IO', '15.2.20.3') do
  assert_include(IO.included_modules, Enumerable)
end

assert('IO.new') do
  IO.new(0)
end

assert('IO gc check') do
  100.times { IO.new(0) }
end

assert('IO TEST SETUP') do
  MRubyIOTestUtil.io_test_setup
end

assert('IO.sysopen, IO#close, IO#closed?') do
  fd = IO.sysopen $mrbtest_io_rfname
  assert_equal Fixnum, fd.class
  io = IO.new fd
  assert_equal IO, io.class
  assert_equal false, io.closed?, "IO not closed"
  assert_equal nil,   io.close,   "IO#close should return nil"
  assert_equal true,  io.closed?, "IO#closed? should return true"
end

assert('IO.sysopen, IO#sysread') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  str1 = "     "
  str2 = io.sysread(5, str1)
  assert_equal $mrbtest_io_msg[0,5], str1
  assert_equal $mrbtest_io_msg[0,5], str2
  assert_raise EOFError do
    io.sysread(10000)
    io.sysread(10000)
  end
  io.close
  io.closed?
end

assert('IO.sysopen, IO#syswrite') do
  fd = IO.sysopen $mrbtest_io_wfname, "w"
  io = IO.new fd, "w"
  str = "abcdefg"
  len = io.syswrite(str)
  assert_equal str.size, len
  io.close
  io.closed?
end

assert('IO#_read_buf') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  def io._buf
    @buf
  end
  msg_len = $mrbtest_io_msg.size + 1
  assert_equal '', io._buf
  assert_equal $mrbtest_io_msg + "\n", io._read_buf
  assert_equal $mrbtest_io_msg + "\n", io._buf
  assert_equal 'mruby', io.read(5)
  assert_equal 5, io.pos
  assert_equal msg_len - 5, io._buf.size
  assert_equal $mrbtest_io_msg[5,100] + "\n", io.read
  assert_equal 0, io._buf.size
  assert_raise EOFError do
    io._read_buf
  end
  assert_equal true, io.eof
  assert_equal true, io.eof?
  io.close
  io.closed?
end

assert('IO#read argument check') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  assert_raise TypeError do
    io.read("str")
  end
  assert_raise ArgumentError do
    io.read(-5)
  end
  io.close
  io.closed?
end

assert('IO#read') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  assert_equal 'mruby', io.read(5)
  assert_equal $mrbtest_io_msg[5,100] + "\n", io.read
  assert_equal nil,  io.read
  io.close
  io.closed?
end

assert('IO#readchar, IO#getc') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  def io._buf
    @buf
  end
  assert_equal 'm', io.readchar
  assert_equal 1, io.pos
  assert_equal 'r', io.getc
  assert_equal 2, io.pos
  io.gets
  assert_raise EOFError do
    io.readchar
  end
  assert_equal nil, io.getc
  io.close
  io.closed?
end

assert('IO#pos=, IO#seek') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  def io._buf
    @buf
  end
  assert_equal 'm', io.getc
  assert_equal 1, io.pos
  assert_equal 0, io.seek(0)
  assert_equal 0, io.pos
  io.close
  io.closed?
end


assert('IO#gets') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd

  # gets without arguments
  assert_equal $mrbtest_io_msg + "\n", io.gets, "gets without arguments"
  assert_equal nil, io.gets, "gets returns nil, when EOF"

  # gets with limit
  io.pos = 0
  assert_equal $mrbtest_io_msg[0, 5], io.gets(5), "gets with limit"

  # gets with rs
  io.pos = 0
  assert_equal $mrbtest_io_msg[0, 6], io.gets(' '), "gets with rs"

  # gets with rs, limit
  io.pos = 0
  assert_equal $mrbtest_io_msg[0, 5], io.gets(' ', 5), "gets with rs, limit"
  io.close
  assert_equal true, io.closed?, "close success"

  # reading many-lines file.
  fd = IO.sysopen $mrbtest_io_wfname, "w"
  io = IO.new fd, "w"
  io.write "0123456789" * 2 + "\na"
  assert_equal 22, io.pos
  io.close
  assert_equal true, io.closed?

  fd = IO.sysopen $mrbtest_io_wfname
  io = IO.new fd
  line = io.gets

  # gets first line
  assert_equal "0123456789" * 2 + "\n", line, "gets first line"
  assert_equal 21, line.size
  assert_equal 21, io.pos

  # gets second line
  assert_equal "a", io.gets, "gets second line"

  # gets third line
  assert_equal nil, io.gets, "gets third line; returns nil"

  io.close
  io.closed?
end

assert('IO#gets - paragraph mode') do
  fd = IO.sysopen $mrbtest_io_wfname, "w"
  io = IO.new fd, "w"
  io.write "0" * 10 + "\n"
  io.write "1" * 10 + "\n\n"
  io.write "2" * 10 + "\n"
  assert_equal 34, io.pos
  io.close
  assert_equal true, io.closed?

  fd = IO.sysopen $mrbtest_io_wfname
  io = IO.new fd
  para1 = "#{'0' * 10}\n#{'1' * 10}\n\n"
  text1 = io.gets("")
  assert_equal para1, text1
  para2 = "#{'2' * 10}\n"
  text2 = io.gets("")
  assert_equal para2, text2
  io.close
  io.closed?
end

assert('IO.popen') do
  io = IO.popen("ls")
  assert_equal Fixnum, io.pid.class
  ls = io.read
  assert_equal ls.class, String
  assert_include ls, 'AUTHORS'
  assert_include ls, 'mrblib'
  io.close
  io.closed?
end

assert('IO#fileno') do
  fd = IO.sysopen $mrbtest_io_rfname
  io = IO.new fd
  assert_equal io.fileno, fd
  assert_equal io.to_i, fd
  io.close
  io.closed?
end

assert('IO TEST CLEANUP') do
  assert_nil MRubyIOTestUtil.io_test_cleanup
end
