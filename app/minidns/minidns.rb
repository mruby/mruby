require "socket"

class DNS
  def initialize(s)
    @pkt = s.unpack("C*")
  end

  def decompress(offs)
    a = []
    while true
      if @pkt[offs] == 0
	offs += 1
        break
      elsif @pkt[offs] > 0xc0
        ptr = get16(offs) - 0xc0
	offs += 3
	a2, n = decompress(ptr)
	a += a2
	break
      else
        n = @pkt[offs]
        a << @pkt[offs+1, n].pack("C*")
	offs += n + 1
      end
    end
    [ a.join('.'), offs ]
  end

  def get8(offs)
    @pkt[offs]
  end

  def get16(offs)
    (@pkt[offs]<<8) + @pkt[offs+1]
  end

  def get32(offs)
    (@pkt[offs]<<24) + (@pkt[offs+1]<<16) + (@pkt[offs+2]<<8) + @pkt[offs+3]
  end

  def parse_header
    @id = get16(0)
    x = get16(2)
    @qr = x >> 15
    @opcode = (x >> 11) & 0xf
    @aa = (x >> 10) & 1
    @tc = (x >> 9) & 1
    @rd = (x >> 8) & 1
    @ra = (x >> 7) & 1
    # check MBZ?
    @rcode = (x >> 10) & 0xf
    @qdcount = get16(4)
    @ancount = get16(6)
    @nscount = get16(8)
    @arcount = get16(10)
  end

  def parse_query_section
    @qname, offs = decompress(12)
    @qtype = get16(offs)
    @qclass = get16(offs+2)
    puts "qname=#{@qname}, qtype=#{@qtype}, qclass=#{@qclass}"
  end

  attr_accessor :id
  attr_accessor :qname

  def show
    @id = get16(0)
    @qdcount = get16(4)
    printf "id=%x, qdcount=%d\n", @id, @qdcount
  end
end

class DNSServer
  def initialize
    @client_sock = nil
    @server_sock = nil
  end

  def bind_socket
    s = UDPSocket.new
    s.bind("127.0.0.1", 5553)
    s.setnonblock(true)
    @client_sock = s
  end

  def connect_to_server
    s = UDPSocket.new
    s.connect("8.8.8.8", 53)
    @server_socket = s
  end

  def mainloop
    bind_socket
    connect_to_server
    while true
      towait = true
      begin
        x, alist = @client_sock.recvfrom(4096)
	p alist
	y = receive_query(x)
	@client_sock.send(y, 0, alist[3], alist[1])
        nowait = false
      rescue Errno::EWOULDBLOCK
      end
      sleep 0.01 if towait
    end
  end

  def make_response(query, addr)
    def make16(x)
      [ (x >> 8) & 0xff, x & 0xff ]
    end
    def make32(x)
      [ (x>>24)&0xff, (x>>16)&0xff, (x>>8)&0xff, x&0xff ]
    end
    def make_label(name)
      a = []
      na = name.split('.')
      na.each { |label|
        a << label.size
        a += label.unpack("C*")
      }
      a << 0
    end
    a = []
    a += make16(query.id)
    a << 0x85  # QR=1,Opcode=0,AA=1,TC=0,RD=1
    a << 0x80  # RA=1,Z=0,RCODE=0
    a += make16(1)
    a += make16(1)
    a += make16(0)
    a += make16(0)
    # question section
    a += make_label(query.qname)
    a += make16(1)
    a += make16(1)
    # answer section
    a += make_label(query.qname)
    a += make16(1)
    a += make16(1)
    a += make32(512)
    a += make16(4)
    a += addr
    a.pack("C*")
  end

  def receive_query(pkt)
    def getifa
      i = IO.popen("ifconfig en0").read
      m = Regexp.new('inet (\S+)').match(i)
      m[1].split('.').map { |x| x.to_i }
    end
    def getdefault
      i = IO.popen("route -n get default").read
      m = Regexp.new('gateway: (\S+)').match(i)
      m[1].split('.').map { |x| x.to_i }
    end

    #send_query(pkt)
    dnsp = DNS.new(pkt)
    dnsp.show
    dnsp.parse_header
    dnsp.parse_query_section
    if dnsp.qname == "en0.example.jp"
      make_response(dnsp, getifa)
    elsif dnsp.qname == "default.example.com"
      make_response(dnsp, getdefault)
    else
      p "send", pkt
      @server_socket.send(pkt, 0)
      @server_socket.recv(4096)
    end
  end
end

DNSServer.new.mainloop
