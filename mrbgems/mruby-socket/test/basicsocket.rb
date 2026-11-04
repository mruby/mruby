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
  s = UDPSocket.new
  begin
    s.setsockopt(Socket::SOL_SOCKET.to_f, Socket::SO_REUSEADDR, true)
    assert_true s.getsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR).bool
  ensure
    s.close
  end
end
