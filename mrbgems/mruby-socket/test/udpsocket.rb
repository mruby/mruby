# TEMPORARY diagnostic, to be removed before this branch is merged. The
# 32-bit Windows job rejects UDPSocket.new(Socket::AF_INET6) with EBADF from
# IO#initialize's descriptor check while the AF_INET case beside it passes,
# and the descriptor Socket._socket hands over is the only thing that differs.
# Print it.
assert('DIAGNOSTIC descriptors') do
  [["AF_INET", Socket::AF_INET], ["AF_INET6", Socket::AF_INET6]].each do |name, af|
    begin
      fd = Socket._socket(af, Socket::SOCK_DGRAM, 0)
      puts "DIAG #{name}: af=#{af} fd=#{fd} (0x#{fd < 0 ? (fd + (1 << 32)).to_s(16) : fd.to_s(16)})"
      begin
        UDPSocket.for_fd(fd).close
        puts "DIAG #{name}: wrapped and closed"
      rescue Exception => e
        puts "DIAG #{name}: wrap raised #{e.class}: #{e.message}"
      end
    rescue Exception => e
      puts "DIAG #{name}: _socket raised #{e.class}: #{e.message}"
    end
  end
  true
end

assert('UDPSocket.new') do
  s = UDPSocket.new
  assert_true(s.is_a? UDPSocket)
  s.close
  true
end

assert('UDPSocket.new(AF_INET6)') do
  # A kernel built without IPv6, or booted with ipv6.disable=1, fails
  # socket(AF_INET6, SOCK_DGRAM, 0) with EAFNOSUPPORT. That is the host
  # reporting it has no IPv6, not UDPSocket misbehaving, so skip rather
  # than let the exception escape and be counted as a crash - the same
  # shape as the rescue-then-skip in mruby-io's File tests.
  #
  # Separate from the AF_INET case above on purpose: sharing one assert
  # meant that a host without IPv6 lost the IPv4 coverage as well.
  # EAFNOSUPPORT specifically, not any failure: every other errno from
  # socket() is a real fault and should still fail the run. Naming it
  # needs Errno, which is why mrbgem.rake takes mruby-errno as a test
  # dependency - without it this raises a RuntimeError that spells the
  # errno as a bare number, and there is no class to match on.
  begin
    s = UDPSocket.new(Socket::AF_INET6)
  rescue Errno::EAFNOSUPPORT => e
    skip e.message
  end
  assert_true(s.is_a? UDPSocket)
  s.close
  true
end

#assert('UDPSocket#connect') do
#assert('UDPSocket#send') do
#assert('UDPSocket#recv') do

#assert('UDPSocket#bind') do
#assert('UDPSocket#recvfrom_nonblock') do
