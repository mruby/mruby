assert('Addrinfo') do
  assert_equal(Class, Addrinfo.class)
end

assert('super class of Addrinfo') do
  assert_equal(Object, Addrinfo.superclass)
end

assert('Addrinfo.getaddrinfo') do
  skip "localhost resolution unreliable in Windows getaddrinfo" if SocketTest.win?
  ary = Addrinfo.getaddrinfo("localhost", 53, Socket::AF_INET, Socket::SOCK_STREAM)
  assert_true(ary.size >= 1)
  ai = ary[0]
  assert_equal(ai.afamily, Socket::AF_INET)
  assert_equal(ai.pfamily, Socket::PF_INET)
  assert_equal(ai.socktype, Socket::SOCK_STREAM)
  assert_equal(ai.ip_address, '127.0.0.1')
  assert_equal(ai.ip_port, 53)
end

assert('Addrinfo.getaddrinfo rejects out-of-range integer hints') do
  # a 64-bit mrb_int that does not fit C int must raise, not be silently
  # truncated into a different but still-valid-looking hint (#6960)
  # The shift width comes from a variable because `1 << 40` written out is
  # constant folded, and the fold fails while this file is compiled on
  # MRB_INT32 without bigint, dropping every test in it.
  shift = 40
  big = nil
  wide = begin
    big = 1 << shift  # RangeError where mrb_int is 32 bits and bigint is absent
    [][big]           # nil for an mrb_int index, RangeError for a big integer
    true
  rescue RangeError
    false
  end
  # A big integer is not an mrb_int either: the hints keep their unconverted
  # type, `Addrinfo.getaddrinfo` leaves the field at its default, and the
  # narrowing under test never runs.
  skip "needs 64-bit mrb_int" unless wide
  assert_raise(RangeError) { Addrinfo.getaddrinfo("localhost", nil, big) }
  assert_raise(RangeError) { Addrinfo.getaddrinfo("localhost", nil, nil, nil, nil, big) }
end

assert('Addrinfo.getaddrinfo rejects a big integer hint') do
  # a hint too wide for mrb_int arrives as a big integer, which used to miss
  # the `mrb_integer_p` check and leave the field at its default: the caller
  # asked for something unrepresentable and got an unhinted lookup instead
  # The shift width comes from a variable because a literal wide shift is
  # constant folded, and the fold fails where bigint is absent, dropping every
  # test in this file.
  shift = 70
  big = begin
    1 << shift
  rescue RangeError
    nil
  end
  skip "needs mruby-bigint" unless big
  assert_raise(RangeError) { Addrinfo.getaddrinfo("localhost", nil, big) }
  assert_raise(RangeError) { Addrinfo.getaddrinfo("localhost", nil, nil, big) }
  assert_raise(RangeError) { Addrinfo.getaddrinfo("localhost", nil, nil, nil, big) }
end

assert('Addrinfo.foreach') do
  skip "localhost resolution unreliable in Windows getaddrinfo" if SocketTest.win?
  # assume Addrinfo.getaddrinfo works well
  a = Addrinfo.getaddrinfo("localhost", 80)
  b = []
  Addrinfo.foreach("localhost", 80) { |ai| b << ai }
  assert_equal(a.size, b.size)
end

assert('Addrinfo.ip') do
  ai = Addrinfo.ip('127.0.0.1')
  assert_equal('127.0.0.1', ai.ip_address)
  assert_equal(Socket::AF_INET, ai.afamily)
  assert_equal(0, ai.ip_port)
  assert_equal(0, ai.socktype)
  assert_equal(0, ai.protocol)
end

assert('Addrinfo.tcp') do
  ai = Addrinfo.tcp('127.0.0.1', 25)
  assert_equal('127.0.0.1', ai.ip_address)
  assert_equal(Socket::AF_INET, ai.afamily)
  assert_equal(25, ai.ip_port)
  assert_equal(Socket::SOCK_STREAM, ai.socktype)
  assert_equal(Socket::IPPROTO_TCP, ai.protocol)
end

assert('Addrinfo.udp') do
  ai = Addrinfo.udp('127.0.0.1', 53)
  assert_equal('127.0.0.1', ai.ip_address)
  assert_equal(Socket::AF_INET, ai.afamily)
  assert_equal(53, ai.ip_port)
  assert_equal(Socket::SOCK_DGRAM, ai.socktype)
  assert_equal(Socket::IPPROTO_UDP, ai.protocol)
end

assert('Addrinfo.unix') do
  skip "unix is not supported on Windows" if SocketTest.win?
  a1 = Addrinfo.unix('/tmp/sock')
  assert_true(a1.unix?)
  assert_equal('/tmp/sock', a1.unix_path)
  assert_equal(Socket::SOCK_STREAM, a1.socktype)
  a2 = Addrinfo.unix('/tmp/sock', Socket::SOCK_DGRAM)
  assert_equal(Socket::SOCK_DGRAM, a2.socktype)
end

assert('Addrinfo#afamily') do
  skip "afamily is not supported on Windows" if SocketTest.win?
  ai4 = Addrinfo.new(Socket.sockaddr_in(1, '127.0.0.1'))
  ai6 = Addrinfo.new(Socket.sockaddr_in(1, '::1'))
  aiu = Addrinfo.new(Socket.sockaddr_un('/tmp/sock'))
  assert_equal(Socket::AF_INET, ai4.afamily)
  assert_equal(Socket::AF_INET6, ai6.afamily)
  assert_equal(Socket::AF_UNIX, aiu.afamily)
end

# assert('Addrinfo#canonname') do

# #getnameinfo
# assert('Addrinfo#inspect') do
# assert('Addrinfo#inspect_socket') do
# assert('Addrinfo#ip?') do
# assert('Addrinfo#ip_address') do
# assert('Addrinfo#ip_port') do
# assert('Addrinfo#ip_unpack') do
# assert('Addrinfo#ipv4?') do
# assert('Addrinfo#ipv6?') do
# assert('Addrinfo#pfamily') do
# assert('Addrinfo#protocol') do
# assert('Addrinfo#socktype') do
# assert('Addrinfo#to_sockaddr') do
# assert('Addrinfo#unix?') do
# #unix_path
