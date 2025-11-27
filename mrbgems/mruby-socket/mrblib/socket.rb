class Addrinfo
  #
  # call-seq:
  #   Addrinfo.new(sockaddr, family=Socket::PF_UNSPEC, socktype=0, protocol=0) -> addrinfo
  #
  # Creates a new Addrinfo object from socket address information.
  # sockaddr can be a packed sockaddr string or an array representation.
  #
  #   Addrinfo.new(Socket.sockaddr_in(80, "127.0.0.1"))
  #   Addrinfo.new(["AF_INET", 80, "localhost", "127.0.0.1"])
  #   Addrinfo.new(["AF_UNIX", "/tmp/socket"])
  #
  def initialize(sockaddr, family=Socket::PF_UNSPEC, socktype=0, protocol=0)
    @hostname = nil
    if sockaddr.is_a? Array
      sary = sockaddr
      if sary[0] == 'AF_INET' || sary[0] == 'AF_INET6'
        @sockaddr = Socket.sockaddr_in(sary[1], sary[3])
        @hostname = sary[2]
      elsif sary[0] == 'AF_UNIX'
        @sockaddr = Socket.sockaddr_un(sary[1])
      end
    else
      @sockaddr = sockaddr.dup
    end
    if family == Socket::PF_UNSPEC or family == nil
      @family = Socket._sockaddr_family(@sockaddr)
    else
      @family = family
    end
    @socktype = socktype
    @protocol = protocol
  end

  #
  # call-seq:
  #   Addrinfo.foreach(nodename, service, family=nil, socktype=nil, protocol=nil, flags=0) { |addrinfo| block } -> array
  #
  # Iterates over all address information for the given nodename and service.
  # Returns an array of Addrinfo objects.
  #
  #   Addrinfo.foreach("www.example.com", "http") { |ai| puts ai.ip_address }
  #   Addrinfo.foreach("localhost", 80) { |ai| puts ai.inspect }
  #
  def self.foreach(nodename, service, family=nil, socktype=nil, protocol=nil, flags=0, &block)
    a = self.getaddrinfo(nodename, service, family, socktype, protocol, flags)
    a.each { |ai| block.call(ai) }
    a
  end

  #
  # call-seq:
  #   Addrinfo.ip(host) -> addrinfo
  #
  # Creates an Addrinfo object for the given host with port 0.
  # Useful for creating address info without specifying a port.
  #
  #   Addrinfo.ip("127.0.0.1")     #=> #<Addrinfo: 127.0.0.1:0>
  #   Addrinfo.ip("::1")           #=> #<Addrinfo: [::1]:0>
  #
  def self.ip(host)
    Addrinfo.new(Socket.sockaddr_in(0, host))
  end

  #
  # call-seq:
  #   Addrinfo.tcp(host, port) -> addrinfo
  #
  # Creates an Addrinfo object for TCP connection to the given host and port.
  #
  #   Addrinfo.tcp("localhost", 80)        #=> #<Addrinfo: 127.0.0.1:80 TCP>
  #   Addrinfo.tcp("www.example.com", 443) #=> #<Addrinfo: 93.184.216.34:443 TCP>
  #
  def self.tcp(host, port)
    Addrinfo.getaddrinfo(host, port, nil, Socket::SOCK_STREAM, Socket::IPPROTO_TCP)[0]
  end

  #
  # call-seq:
  #   Addrinfo.udp(host, port) -> addrinfo
  #
  # Creates an Addrinfo object for UDP connection to the given host and port.
  #
  #   Addrinfo.udp("localhost", 53)        #=> #<Addrinfo: 127.0.0.1:53 UDP>
  #   Addrinfo.udp("8.8.8.8", 53)         #=> #<Addrinfo: 8.8.8.8:53 UDP>
  #
  def self.udp(host, port)
    Addrinfo.getaddrinfo(host, port, nil, Socket::SOCK_DGRAM, Socket::IPPROTO_UDP)[0]
  end

  #
  # call-seq:
  #   Addrinfo.unix(path, socktype=Socket::SOCK_STREAM) -> addrinfo
  #
  # Creates an Addrinfo object for Unix domain socket at the given path.
  #
  #   Addrinfo.unix("/tmp/socket")                    #=> #<Addrinfo: /tmp/socket SOCK_STREAM>
  #   Addrinfo.unix("/var/run/daemon.sock", Socket::SOCK_DGRAM) #=> #<Addrinfo: /var/run/daemon.sock SOCK_DGRAM>
  #
  def self.unix(path, socktype=Socket::SOCK_STREAM)
    Addrinfo.new(Socket.sockaddr_un(path), Socket::AF_UNIX, socktype)
  end

  #
  # call-seq:
  #   addrinfo.afamily -> integer
  #
  # Returns the address family of the socket address.
  #
  #   Addrinfo.tcp("localhost", 80).afamily  #=> 2 (AF_INET)
  #   Addrinfo.unix("/tmp/sock").afamily     #=> 1 (AF_UNIX)
  #
  #
  # call-seq:
  #   addrinfo.afamily -> integer
  #
  # Returns the address family of the socket address.
  #
  #   Addrinfo.tcp("localhost", 80).afamily  #=> 2 (AF_INET)
  #   Addrinfo.unix("/tmp/sock").afamily     #=> 1 (AF_UNIX)
  #
  def afamily
    @family
  end

  #def bind
  #def canonname
  #def connect
  #def connect_from
  #def connect_to

  #def family_addrinfo(host, port=nil)
  #def getnameinfo(flags=0)
  #  Socket.getnameinfo
  #end

  #
  # call-seq:
  #   addrinfo.inspect -> string
  #
  # Returns a string representation of the Addrinfo object.
  #
  #   Addrinfo.tcp("localhost", 80).inspect  #=> "#<Addrinfo: 127.0.0.1:80 TCP>"
  #   Addrinfo.unix("/tmp/sock").inspect     #=> "#<Addrinfo: /tmp/sock SOCK_STREAM>"
  #
  def inspect
    if ipv4? or ipv6?
      if @protocol == Socket::IPPROTO_TCP or (@socktype == Socket::SOCK_STREAM and @protocol == 0)
        proto = 'TCP'
      elsif @protocol == Socket::IPPROTO_UDP or (@socktype == Socket::SOCK_DGRAM and @protocol == 0)
        proto = 'UDP'
      else
        proto = '???'
      end
    else
      proto = "SOCK_STREAM"
    end
    "#<Addrinfo: #{inspect_sockaddr} #{proto}>"
  end

  #
  # call-seq:
  #   addrinfo.inspect_sockaddr -> string
  #
  # Returns a string representation of the socket address portion.
  #
  #   Addrinfo.tcp("localhost", 80).inspect_sockaddr  #=> "127.0.0.1:80"
  #   Addrinfo.unix("/tmp/sock").inspect_sockaddr     #=> "/tmp/sock"
  #
  def inspect_sockaddr
    if ipv4?
      a, p = ip_unpack
      "#{a}:#{p}"
    elsif ipv6?
      a, p = ip_unpack
      "[#{a}]:#{p}"
    elsif unix?
      unix_path
    else
      '???'
    end
  end

  #
  # call-seq:
  #   addrinfo.ip? -> true or false
  #
  # Returns true if the address is an IP address (IPv4 or IPv6).
  #
  #   Addrinfo.tcp("localhost", 80).ip?  #=> true
  #   Addrinfo.unix("/tmp/sock").ip?     #=> false
  #
  def ip?
    ipv4? or ipv6?
  end

  #
  # call-seq:
  #   addrinfo.ip_address -> string
  #
  # Returns the IP address as a string. Raises an exception if not an IP address.
  #
  #   Addrinfo.tcp("localhost", 80).ip_address  #=> "127.0.0.1"
  #   Addrinfo.udp("::1", 53).ip_address       #=> "::1"
  #
  def ip_address
    ip_unpack[0]
  end

  #
  # call-seq:
  #   addrinfo.ip_port -> integer
  #
  # Returns the port number. Raises an exception if not an IP address.
  #
  #   Addrinfo.tcp("localhost", 80).ip_port   #=> 80
  #   Addrinfo.udp("127.0.0.1", 53).ip_port  #=> 53
  #
  def ip_port
    ip_unpack[1]
  end

  #
  # call-seq:
  #   addrinfo.ip_unpack -> [ip_address, port]
  #
  # Returns an array containing the IP address and port number.
  #
  #   Addrinfo.tcp("localhost", 80).ip_unpack   #=> ["127.0.0.1", 80]
  #   Addrinfo.udp("::1", 53).ip_unpack        #=> ["::1", 53]
  #
  def ip_unpack
    h, p = getnameinfo(Socket::NI_NUMERICHOST|Socket::NI_NUMERICSERV)
    [ h, p.to_i ]
  end

  #
  # call-seq:
  #   addrinfo.ipv4? -> true or false
  #
  # Returns true if the address is an IPv4 address.
  #
  #   Addrinfo.tcp("127.0.0.1", 80).ipv4?  #=> true
  #   Addrinfo.tcp("::1", 80).ipv4?        #=> false
  #
  def ipv4?
    @family == Socket::AF_INET
  end

  #def ipv4_loopback?
  #def ipv4_multicast?
  #def ipv4_private?

  #
  # call-seq:
  #   addrinfo.ipv6? -> true or false
  #
  # Returns true if the address is an IPv6 address.
  #
  #   Addrinfo.tcp("::1", 80).ipv6?        #=> true
  #   Addrinfo.tcp("127.0.0.1", 80).ipv6?  #=> false
  #
  def ipv6?
    @family == Socket::AF_INET6
  end

  #def ipv6_loopback?
  #def ipv6_mc_global?
  #def ipv6_mc_linklocal?
  #def ipv6_mc_nodelocal?
  #def ipv6_mc_orilocal?
  #def ipv6_mc_sitelocal?
  #def ipv6_multicast?
  #def ipv6_to_ipv4
  #def ipv6_unspecified
  #def ipv6_v4compat?
  #def ipv6_v4mapped?
  #def listen(backlog=5)

  #
  # call-seq:
  #   addrinfo.pfamily -> integer
  #
  # Returns the protocol family (same as afamily).
  #
  #   Addrinfo.tcp("localhost", 80).pfamily  #=> 2 (PF_INET)
  #   Addrinfo.unix("/tmp/sock").pfamily     #=> 1 (PF_UNIX)
  #
  def pfamily
    @family
  end

  #
  # call-seq:
  #   addrinfo.protocol -> integer
  #
  # Returns the protocol number.
  #
  #   Addrinfo.tcp("localhost", 80).protocol  #=> 6 (IPPROTO_TCP)
  #   Addrinfo.udp("localhost", 53).protocol  #=> 17 (IPPROTO_UDP)
  #
  attr_reader :protocol

  #
  # call-seq:
  #   addrinfo.socktype -> integer
  #
  # Returns the socket type.
  #
  #   Addrinfo.tcp("localhost", 80).socktype  #=> 1 (SOCK_STREAM)
  #   Addrinfo.udp("localhost", 53).socktype  #=> 2 (SOCK_DGRAM)
  #
  attr_reader :socktype

  #
  # call-seq:
  #   addrinfo._to_array -> array
  #
  # Internal method that returns the address information as an array.
  # Used internally by socket operations.
  #
  def _to_array
    case @family
    when Socket::AF_INET
      s = "AF_INET"
    when Socket::AF_INET6
      s = "AF_INET6"
    when Socket::AF_UNIX
      s = "AF_UNIX"
    else
      s = "(unknown AF)"
    end
    addr, port = self.getnameinfo(Socket::NI_NUMERICHOST|Socket::NI_NUMERICSERV)
    [ s, port.to_i, addr, addr ]
  end

  #
  # call-seq:
  #   addrinfo.to_sockaddr -> string
  #
  # Returns the socket address as a packed string.
  #
  #   ai = Addrinfo.tcp("localhost", 80)
  #   ai.to_sockaddr  #=> packed sockaddr string
  #
  def to_sockaddr
    @sockaddr
  end

  alias to_s to_sockaddr

  #
  # call-seq:
  #   addrinfo.unix? -> true or false
  #
  # Returns true if the address is a Unix domain socket address.
  #
  #   Addrinfo.unix("/tmp/sock").unix?      #=> true
  #   Addrinfo.tcp("localhost", 80).unix?  #=> false
  #
  def unix?
    @family == Socket::AF_UNIX
  end
end

class BasicSocket < IO
  @@do_not_reverse_lookup = true

  #
  # call-seq:
  #   BasicSocket.do_not_reverse_lookup -> true or false
  #
  # Returns the current setting for reverse DNS lookups.
  #
  #   BasicSocket.do_not_reverse_lookup  #=> false
  #
  def self.do_not_reverse_lookup
    @@do_not_reverse_lookup
  end

  #
  # call-seq:
  #   BasicSocket.do_not_reverse_lookup = boolean -> boolean
  #
  # Sets whether to perform reverse DNS lookups.
  #
  #   BasicSocket.do_not_reverse_lookup = true
  #
  def self.do_not_reverse_lookup=(val)
    @@do_not_reverse_lookup = val ? true : false
  end

  #
  # call-seq:
  #   BasicSocket.new(*args) -> basicsocket
  #
  # Creates a new BasicSocket object. This is typically called by subclasses.
  #
  def initialize(*args)
    super(*args)
    self._is_socket = true
    @do_not_reverse_lookup = @@do_not_reverse_lookup
  end

  #
  # call-seq:
  #   BasicSocket.for_fd(fd) -> basicsocket
  #
  # Creates a BasicSocket object from an existing file descriptor.
  #
  #   sock = BasicSocket.for_fd(3)
  #
  def self.for_fd(fd)
    super(fd, "r+")
  end

  #def connect_address

  #
  # call-seq:
  #   basicsocket.local_address -> addrinfo
  #
  # Returns an Addrinfo object for the local address of the socket.
  #
  #   sock.local_address  #=> #<Addrinfo: 127.0.0.1:12345 TCP>
  #
  def local_address
    Addrinfo.new self.getsockname
  end

  #
  # call-seq:
  #   basicsocket.recv_nonblock(maxlen, flags=0) -> string
  #
  # Receives data from the socket without blocking. May raise an exception
  # if no data is available.
  #
  #   data = sock.recv_nonblock(1024)
  #
  def recv_nonblock(maxlen, flags=0)
    begin
      _setnonblock(true)
      recv(maxlen, flags)
    ensure
      _setnonblock(false)
    end
  end

  #
  # call-seq:
  #   basicsocket.remote_address -> addrinfo
  #
  # Returns an Addrinfo object for the remote address of the socket.
  #
  #   sock.remote_address  #=> #<Addrinfo: 192.168.1.1:80 TCP>
  #
  def remote_address
    Addrinfo.new self.getpeername
  end

  attr_accessor :do_not_reverse_lookup
end

class IPSocket < BasicSocket
  #
  # call-seq:
  #   IPSocket.getaddress(host) -> string
  #
  # Returns the IP address of the given hostname as a string.
  #
  #   IPSocket.getaddress("localhost")     #=> "127.0.0.1"
  #   IPSocket.getaddress("www.ruby-lang.org")  #=> "150.95.145.38"
  #
  def self.getaddress(host)
    Addrinfo.ip(host).ip_address
  end

  #
  # call-seq:
  #   ipsocket.addr -> [family, port, hostname, ip_address]
  #
  # Returns the local address information as an array.
  #
  #   sock.addr  #=> ["AF_INET", 12345, "localhost", "127.0.0.1"]
  #
  def addr
    Addrinfo.new(self.getsockname)._to_array
  end

  #
  # call-seq:
  #   ipsocket.peeraddr -> [family, port, hostname, ip_address]
  #
  # Returns the remote address information as an array.
  #
  #   sock.peeraddr  #=> ["AF_INET", 80, "example.com", "93.184.216.34"]
  #
  def peeraddr
    Addrinfo.new(self.getpeername)._to_array
  end

  #
  # call-seq:
  #   ipsocket.recvfrom(maxlen, flags=0) -> [data, addrinfo]
  #
  # Receives data and sender information from the IP socket.
  #
  #   data, addr = sock.recvfrom(1024)
  #   data, addr = sock.recvfrom(512, 0)
  #
  def recvfrom(maxlen, flags=0)
    msg, sa = _recvfrom(maxlen, flags)
    [ msg, Addrinfo.new(sa)._to_array ]
  end
end

class TCPSocket < IPSocket
  #
  # call-seq:
  #   TCPSocket.new(host, service, local_host=nil, local_service=nil) -> tcpsocket
  #
  # Creates a new TCP socket connected to the given host and service.
  # Optionally binds to local_host and local_service first.
  #
  #   sock = TCPSocket.new("localhost", 80)
  #   sock = TCPSocket.new("www.example.com", "http")
  #   sock = TCPSocket.new("remote", 80, "127.0.0.1", 12345)
  #
  def initialize(host, service, local_host=nil, local_service=nil)
    if @init_with_fd
      super(host, service)
    else
      s = nil
      e = SocketError
      Addrinfo.foreach(host, service) { |ai|
        begin
          s = Socket._socket(ai.afamily, Socket::SOCK_STREAM, 0)
          if local_host or local_service
            local_host ||= (ai.afamily == Socket::AF_INET) ? "0.0.0.0" : "::"
            local_service ||= "0"
            bi = Addrinfo.getaddrinfo(local_host, local_service, ai.afamily, ai.socktype)[0]
            Socket._bind(s, bi.to_sockaddr)
          end
          Socket._connect(s, ai.to_sockaddr)
          super(s, "r+")
          return
        rescue => e0
          e = e0
        end
      }
      raise e
    end
  end
  #def self.gethostbyname(host)
end

class TCPServer < TCPSocket
  #
  # call-seq:
  #   TCPServer.new(host=nil, service) -> tcpserver
  #
  # Creates a new TCP server socket bound to the given host and service.
  # If host is nil, binds to all available interfaces.
  #
  #   server = TCPServer.new("localhost", 8080)
  #   server = TCPServer.new(nil, 3000)  # binds to all interfaces
  #   server = TCPServer.new("0.0.0.0", "http")
  #
  def initialize(host=nil, service)
    ai = Addrinfo.getaddrinfo(host, service, nil, nil, nil, Socket::AI_PASSIVE)[0]
    @init_with_fd = true
    super(Socket._socket(ai.afamily, Socket::SOCK_STREAM, 0), "r+")
    if Socket.const_defined?(:SO_REUSEADDR)
      self.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, true)
    end
    Socket._bind(self.fileno, ai.to_sockaddr)
    listen(5)
    self
  end

  #
  # call-seq:
  #   tcpserver.accept -> tcpsocket
  #
  # Accepts an incoming connection and returns a new TCPSocket.
  #
  #   server = TCPServer.new(8080)
  #   client = server.accept
  #
  def accept
    fd = self.sysaccept
    begin
      s = TCPSocket._allocate
      s.instance_eval{
        @init_with_fd = true
      }
      s.__send__(:initialize, fd, "r+")
      s
    rescue => e
      IO._sysclose(fd) rescue nil
      raise e
    end
  end

  #
  # call-seq:
  #   tcpserver.accept_nonblock -> unixsocket
  #
  # Accepts an incoming connection without blocking. May raise an exception
  # if no connection is available.
  #
  #   client = server.accept_nonblock
  #
  def accept_nonblock
    begin
      self._setnonblock(true)
      self.accept
    ensure
      self._setnonblock(false)
    end
  end

  #
  # call-seq:
  #   unixserver.listen(backlog) -> 0
  #
  # Sets the socket to listen for incoming connections with the given backlog.
  #
  #   server.listen(5)
  #   server.listen(128)
  #
  def listen(backlog)
    Socket._listen(self.fileno, backlog)
    0
  end

  #
  # call-seq:
  #   tcpserver.sysaccept -> integer
  #
  # Accepts an incoming connection and returns the file descriptor.
  #
  #   fd = server.sysaccept
  #
  def sysaccept
    Socket._accept(self.fileno)
  end
end

class UDPSocket < IPSocket
  #
  # call-seq:
  #   UDPSocket.new(af=Socket::AF_INET) -> udpsocket
  #
  # Creates a new UDP socket for the given address family.
  #
  #   sock = UDPSocket.new
  #   sock = UDPSocket.new(Socket::AF_INET6)
  #
  def initialize(af=Socket::AF_INET)
    super(Socket._socket(af, Socket::SOCK_DGRAM, 0), "r+")
    @af = af
    self
  end

  #
  # call-seq:
  #   ipsocket.bind(host, port) -> 0
  #
  # Binds the socket to the given host and port.
  #
  #   sock.bind("127.0.0.1", 8080)
  #   sock.bind("0.0.0.0", 3000)
  #
  def bind(host, port)
    Socket._bind(self.fileno, _sockaddr_in(port, host))
    0
  end

  #
  # call-seq:
  #   ipsocket.connect(host, port) -> 0
  #
  # Connects the socket to the given host and port.
  #
  #   sock.connect("127.0.0.1", 80)
  #   sock.connect("www.example.com", 443)
  #
  def connect(host, port)
    Socket._connect(self.fileno, _sockaddr_in(port, host))
    0
  end

  #
  # call-seq:
  #   udpsocket.recvfrom_nonblock(maxlen, flags=0) -> [data, addrinfo]
  #
  # Receives data and sender information without blocking.
  # May raise an exception if no data is available.
  #
  #   data, addr = sock.recvfrom_nonblock(1024)
  #
  def recvfrom_nonblock(*args)
    s = self
    begin
      self._setnonblock(true)
      self.recvfrom(*args)
    ensure
      # XXX: self is a SystemcallException here! (should be bug)
      s._setnonblock(false)
    end
  end

  #
  # call-seq:
  #   ipsocket.send(mesg, flags, host=nil, port=nil) -> integer
  #
  # Sends data through the socket. Returns the number of bytes sent.
  #
  #   sock.send("Hello", 0)
  #   sock.send("Data", 0, "127.0.0.1", 8080)
  #
  def send(mesg, flags, host=nil, port=nil)
    if port
      super(mesg, flags, _sockaddr_in(port, host))
    elsif host
      super(mesg, flags, host)
    else
      super(mesg, flags)
    end
  end

  #
  # call-seq:
  #   udpsocket._sockaddr_in(port, host) -> string
  #
  # Internal method to create a sockaddr_in structure for the given port and host.
  # Uses the socket's address family.
  #
  def _sockaddr_in(port, host)
    ai = Addrinfo.getaddrinfo(host, port, @af, Socket::SOCK_DGRAM)[0]
    ai.to_sockaddr
  end
end

class Socket < BasicSocket
  #
  # call-seq:
  #   Socket.new(domain, type, protocol=0) -> socket
  #
  # Creates a new socket with the given domain, type, and protocol.
  #
  #   sock = Socket.new(Socket::AF_INET, Socket::SOCK_STREAM, 0)
  #   sock = Socket.new(Socket::AF_UNIX, Socket::SOCK_DGRAM)
  #
  def initialize(domain, type, protocol=0)
    super(Socket._socket(domain, type, protocol), "r+")
  end

  #def self.accept_loop

  #
  # call-seq:
  #   Socket.getaddrinfo(nodename, servname, family=nil, socktype=nil, protocol=nil, flags=0) -> array
  #
  # Returns an array of Addrinfo objects for the given nodename and servname.
  #
  #   Addrinfo.getaddrinfo("localhost", "http")
  #   Addrinfo.getaddrinfo("www.example.com", 80, Socket::AF_INET)
  #
  def self.getaddrinfo(nodename, servname, family=nil, socktype=nil, protocol=nil, flags=0)
    Addrinfo.getaddrinfo(nodename, servname, family, socktype, protocol, flags).map { |ai|
      ary = ai._to_array
      ary[2] = nodename
      ary[4] = ai.afamily
      ary[5] = ai.socktype
      ary[6] = ai.protocol
      ary
    }
  end

  #def self.getnameinfo
  #def self.ip_address_list

  #
  # call-seq:
  #   Socket.open(domain, type, protocol=0) -> socket
  #
  # Creates a new socket. Alias for Socket.new.
  #
  #   sock = Socket.open(Socket::AF_INET, Socket::SOCK_STREAM)
  #
  def self.open(*args)
    new(args)
  end

  #
  # call-seq:
  #   Socket.sockaddr_in(port, host) -> string
  #
  # Returns a packed sockaddr_in structure for the given port and host.
  #
  #   Socket.sockaddr_in(80, "127.0.0.1")
  #   Socket.sockaddr_in(443, "localhost")
  #
  def self.sockaddr_in(port, host)
    ai = Addrinfo.getaddrinfo(host, port, nil, Socket::SOCK_DGRAM)[0]
    ai.to_sockaddr
  end

  #def self.tcp
  #def self.tcp_server_loop
  #def self.tcp_server_sockets
  #def self.udp_server_loop
  #def self.udp_server_loop_on
  #def self.udp_server_recv
  #def self.udp_server_sockets
  #def self.unix(path)
  #def self.unix_server_loop
  #def self.unix_server_socket

  #
  # call-seq:
  #   Socket.unpack_sockaddr_in(sockaddr) -> [port, ip_address]
  #
  # Unpacks a packed sockaddr_in structure and returns port and IP address.
  #
  #   port, addr = Socket.unpack_sockaddr_in(sockaddr)
  #
  def self.unpack_sockaddr_in(sa)
    Addrinfo.new(sa).ip_unpack.reverse
  end

  #
  # call-seq:
  #   Socket.unpack_sockaddr_un(sockaddr) -> path
  #
  # Unpacks a packed sockaddr_un structure and returns the Unix socket path.
  #
  #   path = Socket.unpack_sockaddr_un(sockaddr)
  #
  def self.unpack_sockaddr_un(sa)
    Addrinfo.new(sa).unix_path
  end

  class << self
    alias pack_sockaddr_in sockaddr_in
    alias pack_sockaddr_un sockaddr_un
    alias pair socketpair
  end

  def accept
    fd, addr = self.sysaccept
    [ Socket.for_fd(fd), addr ]
  end

  def accept_nonblock
    begin
      self._setnonblock(true)
      self.accept
    ensure
      self._setnonblock(false)
    end
  end

  #
  # call-seq:
  #   socket.bind(sockaddr) -> 0
  #
  # Binds the socket to the given socket address.
  #
  #   sock.bind(Socket.sockaddr_in(8080, "127.0.0.1"))
  #   sock.bind(addrinfo)
  #
  def bind(sockaddr)
    sockaddr = sockaddr.to_sockaddr if sockaddr.is_a? Addrinfo
    Socket._bind(self.fileno, sockaddr)
    0
  end

  #
  # call-seq:
  #   socket.connect(sockaddr) -> 0
  #
  # Connects the socket to the given socket address.
  #
  #   sock.connect(Socket.sockaddr_in(80, "127.0.0.1"))
  #   sock.connect(addrinfo)
  #
  def connect(sockaddr)
    sockaddr = sockaddr.to_sockaddr if sockaddr.is_a? Addrinfo
    Socket._connect(self.fileno, sockaddr)
    0
  end

  #
  # call-seq:
  #   socket.connect_nonblock(sockaddr) -> 0
  #
  # Connects the socket to the given address without blocking.
  # May raise an exception if the connection cannot be completed immediately.
  #
  #   sock.connect_nonblock(sockaddr)
  #
  def connect_nonblock(sockaddr)
    begin
      self._setnonblock(true)
      self.connect(sockaddr)
    ensure
      self._setnonblock(false)
    end
  end

  #def ipv6only!

  def listen(backlog)
    Socket._listen(self.fileno, backlog)
    0
  end

  def recvfrom(maxlen, flags=0)
    msg, sa = _recvfrom(maxlen, flags)
    socktype = self.getsockopt(Socket::SOL_SOCKET, Socket::SO_TYPE).int
    [ msg, Addrinfo.new(sa, Socket::PF_UNSPEC, socktype) ]
  end

  def recvfrom_nonblock(*args)
    begin
      self._setnonblock(true)
      self._recvfrom(*args)
    ensure
      self._setnonblock(false)
    end
  end

  def sysaccept
    Socket._accept2(self.fileno)
  end
end

class UNIXSocket < BasicSocket
  #
  # call-seq:
  #   UNIXSocket.new(path) -> unixsocket
  #   UNIXSocket.new(path) { |sock| block } -> obj
  #
  # Creates a new Unix domain socket connected to the given path.
  # If a block is given, yields the socket and closes it when done.
  #
  #   sock = UNIXSocket.new("/tmp/socket")
  #   UNIXSocket.new("/tmp/socket") { |s| s.write("data") }
  #
  def initialize(path, &block)
    if self.is_a? UNIXServer
      super(path, "r")
    else
      super(Socket._socket(Socket::AF_UNIX, Socket::SOCK_STREAM, 0), "r+")
      Socket._connect(self.fileno, Socket.sockaddr_un(path))

      if block_given?
        begin
          yield self
        ensure
          begin
            self.close unless self.closed?
          rescue StandardError
          end
        end
      end
    end
  end

  class << self
    #
    # call-seq:
    #   UNIXSocket.socketpair(type=Socket::SOCK_STREAM, protocol=0) -> [socket1, socket2]
    #
    # Creates a pair of connected Unix domain sockets.
    #
    #   sock1, sock2 = UNIXSocket.socketpair
    #   sock1, sock2 = UNIXSocket.socketpair(Socket::SOCK_DGRAM)
    #
    def socketpair(type=Socket::SOCK_STREAM, protocol=0)
      a = Socket.socketpair(Socket::AF_UNIX, type, protocol)
      [ UNIXSocket.for_fd(a[0]), UNIXSocket.for_fd(a[1]) ]
    end

    alias pair socketpair
  end

  #
  # call-seq:
  #   unixsocket.addr -> [family, path]
  #
  # Returns the local address information as an array.
  #
  #   sock.addr  #=> ["AF_UNIX", "/tmp/socket"]
  #
  def addr
    [ "AF_UNIX", path ]
  end

  #
  # call-seq:
  #   unixsocket.path -> string
  #
  # Returns the path of the Unix domain socket.
  #
  #   sock.path  #=> "/tmp/socket"
  #
  def path
    Addrinfo.new(self.getsockname).unix_path
  end

  #
  # call-seq:
  #   unixsocket.peeraddr -> [family, path]
  #
  # Returns the remote address information as an array.
  #
  #   sock.peeraddr  #=> ["AF_UNIX", "/tmp/peer_socket"]
  #
  def peeraddr
    [ "AF_UNIX", Addrinfo.new(self.getpeername).unix_path ]
  end

  #def recv_io

  def recvfrom(maxlen, flags=0)
    msg, sa = _recvfrom(maxlen, flags)
    path = (sa.size > 0) ? Addrinfo.new(sa).unix_path : ""
    [ msg, [ "AF_UNIX", path ] ]
  end

  #def send_io
end

class UNIXServer < UNIXSocket
  #
  # call-seq:
  #   UNIXServer.new(path) -> unixserver
  #
  # Creates a new Unix domain server socket bound to the given path.
  #
  #   server = UNIXServer.new("/tmp/server_socket")
  #
  def initialize(path)
    fd = Socket._socket(Socket::AF_UNIX, Socket::SOCK_STREAM, 0)
    begin
      super(fd)
      Socket._bind(fd, Socket.pack_sockaddr_un(path))
      self.listen(5)
    rescue => e
      IO._sysclose(fd) rescue nil
      raise e
    end

    if block_given?
      begin
        yield self
      ensure
        self.close rescue nil unless self.closed?
      end
    end
  end

  #
  # call-seq:
  #   unixserver.accept -> unixsocket
  #
  # Accepts an incoming connection and returns a new UNIXSocket.
  #
  #   server = UNIXServer.new("/tmp/server")
  #   client = server.accept
  #
  def accept
    fd = self.sysaccept
    begin
      sock = UNIXSocket.for_fd(fd)
    rescue
      IO._sysclose(fd) rescue nil
    end
    sock
  end

  #
  # call-seq:
  #   unixserver.accept_nonblock -> unixsocket
  #
  # Accepts an incoming connection without blocking. May raise an exception
  # if no connection is available.
  #
  #   client = server.accept_nonblock
  #
  def accept_nonblock
    begin
      self._setnonblock(true)
      self.accept
    ensure
      self._setnonblock(false)
    end
  end

  #
  # call-seq:
  #   unixserver.listen(backlog) -> 0
  #
  # Sets the socket to listen for incoming connections with the given backlog.
  #
  #   server.listen(5)
  #   server.listen(128)
  #
  def listen(backlog)
    Socket._listen(self.fileno, backlog)
    0
  end

  #
  # call-seq:
  #   unixserver.sysaccept -> integer
  #
  # Accepts an incoming connection and returns the file descriptor.
  #
  #   fd = server.sysaccept
  #
  def sysaccept
    Socket._accept(self.fileno)
  end
end

class SocketError < StandardError; end
