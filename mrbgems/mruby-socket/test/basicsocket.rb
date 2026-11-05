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
