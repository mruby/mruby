unless SocketTest.win?

# Note: most of tests below will fail if UDPSocket is broken.

assert('IPSocket.getaddress') do
  l = IPSocket.getaddress("localhost")
  assert_true (l == "127.0.0.1" or l == "::1")
end

assert('IPSocket.pton') do
  assert_equal "\x01\x02\x03\x04", IPSocket.pton(Socket::AF_INET, "1.2.3.4")
  assert_raise(ArgumentError) { IPSocket.pton(Socket::AF_INET, "1.2.3.4junk") }
  assert_raise(ArgumentError) { IPSocket.pton(Socket::AF_INET, "1.2.3.4\0junk") }
  assert_raise(ArgumentError) { IPSocket.pton(Socket::AF_INET6, "::1\0junk") }
end

assert('IPSocket.ntop') do
  assert_equal "1.2.3.4", IPSocket.ntop(Socket::AF_INET, "\x01\x02\x03\x04")
  assert_equal "::1", IPSocket.ntop(Socket::AF_INET6, "\0" * 15 + "\x01")
  assert_raise(ArgumentError) { IPSocket.ntop(Socket::AF_INET, "\x01\x02\x03") }
  assert_raise(ArgumentError) { IPSocket.ntop(Socket::AF_INET6, "\x01\x02\x03\x04") }
  assert_raise(ArgumentError) { IPSocket.ntop(Socket::AF_UNSPEC, "\x01\x02\x03\x04") }

  # A family past C int is not the family it would be cut down to. Computed, not
  # written out, so the file still compiles where mrb_int is 32 bits.
  shift = 32
  big = begin
    1 << shift
  rescue RangeError
    nil
  end
  assert_raise(ArgumentError) { IPSocket.ntop(big + Socket::AF_INET6, "\x01") } if big
end

assert('IPSocket.addr') do
  localhost = "127.0.0.1"
  s = UDPSocket.new
  s.bind(localhost, 0)
  port = Addrinfo.new(s.getsockname).ip_port

  a = s.addr
  assert_equal "AF_INET", a[0]
  assert_equal port,      a[1]
  assert_equal localhost, a[2]
  assert_equal localhost, a[3]
  s.close
  true
end

assert('IPSocket.peeraddr') do
  localhost = "127.0.0.1"
  server = UDPSocket.new
  server.bind(localhost, 0)
  port = server.local_address.ip_port

  client = UDPSocket.new
  client.connect(localhost, port)

  a = client.peeraddr
  assert_equal "AF_INET", a[0]
  assert_equal port,      a[1]
  assert_equal localhost, a[2]
  assert_equal localhost, a[3]
  client.close
  server.close
  true
end

end # win?
