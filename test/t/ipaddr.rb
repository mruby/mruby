##
# IPAddr Test

if Object.const_defined?(:IPAddr)
  assert('IPAddr') do
    IPAddr.class == Class
  end

  assert('IPAddr#initialize') do
    a = IPAddr.new('192.0.2.1/24')
    b = IPAddr.new('192.0.2.1/24', Socket::AF_INET)
    c = IPAddr.new('2001:db8::1/64')
    d = IPAddr.new('2001:db8::1/64', Socket::AF_INET6)
    a.inspect == b.inspect and c.inspect == d.inspect
  end

  assert('IPAddr#family') do
    a = IPAddr.new('192.0.2.1', Socket::AF_INET)
    b = IPAddr.new('2001:db8::1', Socket::AF_INET6)
    c = IPAddr.new('192.0.2.1', Socket::AF_UNSPEC)
    d = IPAddr.new('2001:db8::1', Socket::AF_UNSPEC)
    a.family == Socket::AF_INET and
    b.family == Socket::AF_INET6 and
    c.family == Socket::AF_INET and
    d.family == Socket::AF_INET6
  end

  assert('IPAddr#hton') do
    IPAddr.new('1.2.3.4').hton == "\x01\x02\x03\x04"
  end

  assert('IPAddr#ipv4?') do
    a = IPAddr.new('192.0.2.1', Socket::AF_INET)
    b = IPAddr.new('2001:db8::1', Socket::AF_INET6)
    c = IPAddr.new('192.0.2.1', Socket::AF_UNSPEC)
    d = IPAddr.new('2001:db8::1', Socket::AF_UNSPEC)
    a.ipv4? and (not b.ipv4?) and c.ipv4? and (not d.ipv4?)
  end

  assert('IPAddr#ipv6?') do
    a = IPAddr.new('192.0.2.1', Socket::AF_INET)
    b = IPAddr.new('2001:db8::1', Socket::AF_INET6)
    c = IPAddr.new('192.0.2.1', Socket::AF_UNSPEC)
    d = IPAddr.new('2001:db8::1', Socket::AF_UNSPEC)
    (not a.ipv6?) and b.ipv6? and (not c.ipv6?) and d.ipv6?
  end

  assert('IPAddr#mask') do
    IPAddr.new('192.0.2.1').mask(24).hton == "\xc0\x00\x02\x00"
  end

  assert('IPAddr#to_s') do
    IPAddr.new('192.0.2.1').to_s == '192.0.2.1' and
    IPAddr.new('2001:db8::3').to_s == '2001:db8::3'
  end

  assert('IPAddr#<=>') do
    x = IPAddr.new('1.1.1.1')
    a = IPAddr.new('1.1.1.0')
    b = IPAddr.new('1.1.1.2')
    c = IPAddr.new('1.1.1.1')
    d = IPAddr.new('2001:db8::1')
    (x <=> a) > 0 and (x <=> b) < 0 and (x <=> c) == 0 and (x <=> d) == nil
  end

  assert('IPAddr#~') do
    (~IPAddr.new('192.0.2.1')).to_s == '63.255.253.254'
  end
end
