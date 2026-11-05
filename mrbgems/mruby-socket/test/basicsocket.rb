assert('BasicSocket') do
  assert_equal(Class, BasicSocket.class)
end

assert('super class of BasicSocket') do
  assert_equal(IO, BasicSocket.superclass)
end

assert('BasicSocket.do_not_reverse_lookup') do
  assert_equal(BasicSocket.do_not_reverse_lookup, true)
end

assert('BasicSocket.do_not_reverse_lookup=') do
  BasicSocket.do_not_reverse_lookup = false
  assert_equal(BasicSocket.do_not_reverse_lookup, false)
  BasicSocket.do_not_reverse_lookup = true
end

assert('BasicSocket#setsockopt with a Float level') do
  skip "no Float in this build" unless Object.const_defined?(:Float)
  s = UDPSocket.new
  begin
    s.setsockopt(Socket::SOL_SOCKET.to_f, Socket::SO_REUSEADDR, true)
    assert_true s.getsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR).bool
  ensure
    s.close
  end
end

assert('a socket is closed across exec') do
  skip "no FD_CLOEXEC on Windows" if SocketTest.win?
  skip "IO#close_on_exec? is not available" unless IO.method_defined?(:close_on_exec?)
  srv = TCPServer.new("127.0.0.1", 0)
  cli = TCPSocket.new("127.0.0.1", srv.local_address.ip_port)
  acc = srv.accept
  u = UDPSocket.new
  socks = [srv, cli, acc, u]
  socks.concat(UNIXSocket.pair) if Object.const_defined?(:UNIXSocket)
  begin
    socks.each { |s| assert_true s.close_on_exec?, s.class.to_s }
  ensure
    socks.each(&:close)
  end
end

# (1 << 32) + 1 was cut to 1 on its way to the OS. Computed, not written out, so
# the file still compiles where mrb_int is 32 bits.
def socket_test_past_int
  shift = 32
  1 << shift
rescue RangeError
  nil
end

assert('BasicSocket methods with an Integer past C int') do
  big = socket_test_past_int
  skip "no integer past 32 bits" unless big

  s = UDPSocket.new
  begin
    sol = Socket::SOL_SOCKET
    opt = Socket::SO_REUSEADDR
    assert_raise(RangeError) { s.setsockopt(big + sol, opt, true) }
    assert_raise(RangeError) { s.setsockopt(sol, big + opt, true) }
    # where a value past C int is a Bigint, not an Integer optval, it is
    # refused as an optval of no kind setsockopt takes
    assert_raise(RangeError, ArgumentError) { s.setsockopt(sol, opt, big + 1) }
    assert_raise(RangeError) { s.getsockopt(big + sol, opt) }
    assert_raise(RangeError) { s.getsockopt(sol, big + opt) }
    assert_raise(RangeError) { s.send("", big) }
    assert_raise(RangeError) { s.shutdown(big) }
    assert_raise(RangeError) { Socket::Option.int(Socket::AF_INET, sol, opt, big + 1) }

    # an int option still round-trips
    assert_equal 0, s.setsockopt(sol, opt, 1)
    assert_true s.getsockopt(sol, opt).bool
    assert_equal 0, s.setsockopt(sol, opt, false)
    assert_false s.getsockopt(sol, opt).bool
  ensure
    s.close
  end
end

assert('BasicSocket#recv with an Integer past C int') do
  big = socket_test_past_int
  skip "no integer past 32 bits" unless big

  # Something to read, so that a recv which ignores the check doesn't block.
  r = UDPSocket.new
  w = UDPSocket.new
  begin
    r.bind("127.0.0.1", 0)
    port = Addrinfo.new(r.getsockname).ip_port
    4.times { w.send("x", 0, "127.0.0.1", port) }

    assert_raise(RangeError) { r.recv(1, big) }
    assert_raise(RangeError) { r.recvfrom(1, big) }
    assert_raise(RangeError) { r._recvfrom(1, big) }
    assert_equal "x", r.recv(1)
  ensure
    r.close
    w.close
  end
end

assert('Socket file descriptor and option arguments past C int') do
  big = socket_test_past_int
  skip "no integer past 32 bits" unless big

  s = UDPSocket.new
  begin
    s.bind("127.0.0.1", 0)
    fd = s.fileno
    sa = s.getsockname
    af = Socket::AF_INET
    type = Socket::SOCK_DGRAM
    assert_raise(RangeError) { Socket._socket(big + af, type, 0) }
    assert_raise(RangeError) { Socket._socket(af, big + type, 0) }
    assert_raise(RangeError) { Socket._socket(af, type, big) }
    assert_raise(RangeError) { Socket._bind(big + fd, sa) }
    assert_raise(RangeError) { Socket._connect(big + fd, sa) }
    assert_raise(RangeError) { Socket._listen(big + fd, 1) }
    assert_raise(RangeError) { Socket._listen(fd, big + 1) }
    assert_raise(RangeError) { Socket._accept(big + fd) }
    assert_raise(RangeError) { Socket._accept2(big + fd) }
    assert_raise(RangeError) { Addrinfo.ip("127.0.0.1").getnameinfo(big) }
  ensure
    s.close
  end
end
