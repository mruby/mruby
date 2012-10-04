if Object.const_defined? :Socket
  class IPAddr
    def initialize(addr='::', family=Socket::AF_UNSPEC)
      a, m = addr.split('/', 2)
      if family == Socket::AF_UNSPEC
        initialize_auto_family(a)
      else
	@addr = IPSocket.pton(family, a)
	@family = family
      end
      if m
        initialize_makemask(m)
        applymask
      else
        if @family == Socket::AF_INET
          @mask = "\xff" * 4
        elsif @family == Socket::AF_INET6
          @mask = "\xff" * 16
        end
      end
    end

    # to be removed when issue #480 is fixed
    def initialize_auto_family(addr)
      begin
        @addr = IPSocket.pton(Socket::AF_INET, addr)
        @family = Socket::AF_INET
      rescue ArgumentError
	@addr = IPSocket.pton(Socket::AF_INET6, addr)
        @family = Socket::AF_INET6
      end
    end

    def initialize_makemask(m)
      if @family == Socket::AF_INET and m.include?('.')
        @mask = IPSocket.pton(Socket::AF_INET, m)
      elsif @family == Socket::AF_INET6 and m.include?(':')
        @mask = IPSocket.pton(Socket::AF_INET6, m)
      else
	ma = []
        mi = m.to_i
	mlen = (ipv4?) ? 32 : 128
	while mlen > 0
	  if mi >= 8
	    ma << 0xff
	  elsif mi <= 0
	    ma << 0
	  else
	    ma << (0xff ^ (0xff >> mi))
	  end
	  mi -= 8
	  mlen -= 8
	end
        @mask = ma.pack("C*")
      end
    end

    # private
    def applymask
      a = @addr.unpack("C*")
      m = @mask.unpack("C*")
      (0...a.size).each { |i| a[i] &= m[i] }
      @addr = a.pack("C*")
    end

    def family
      @family
    end

    def hton
      @addr
    end

    def inspect
      def a2s(a)
        if @family == Socket::AF_INET
	  return a.unpack("C*").map { |b| format("%u", b) }.join('.')
	else
	  bytes = a.unpack("C*").map { |x| format "%02x", x }
	  return (0..7).map { |i| bytes[i*2, 2].join }.join(':')
	end
      end
      format "#<%s: %s:%s/%s>",
        self.class, (ipv4? ? 'IPv4' : 'IPv6'), a2s(@addr), a2s(@mask)
    end

    def ipv4?
      @family == Socket::AF_INET
    end

    def ipv6?
      @family == Socket::AF_INET6
    end

    def mask(prefixlen)
      a = @addr.unpack("C*")
      if prefixlen % 8 == 0
        a[prefixlen.div(8)] = 0
      else
        a[prefixlen.div(8)] &= ~(0xff >> (prefixlen % 8))
      end
      (prefixlen.div(8)+1..a.size-1).each { |i| a[i] = 0 }
      IPAddr.new('::', Socket::AF_INET6).set2(a.pack("C*"), @family)
    end

    # private
    def set2(a, f)
      @addr = a
      @family = f
      if @family == Socket::AF_INET
        @mask = "\xff" * 4
      elsif @family == Socket::AF_INET6
        @mask = "\xff" * 16
      end
      self
    end

    def to_s
      IPSocket.ntop(@family, @addr)
    end

    def <=>(other)
      @addr <=> other.hton
    end

    def ~
      a = @addr.unpack("C*").map { |b| b ^ 0xff }.pack("C*")
      IPAddr.new('::', Socket::AF_INET6).set2(a, @family)
    end

    alias to_string to_s

    # mruby extension
    def is_netaddr?(prefixlen)
      (self <=> self.mask(prefixlen)) == 0
    end

    def is_bcastaddr?(prefixlen)
      i = ~self
      (i <=> i.mask(prefixlen)) == 0
    end
  end
end
