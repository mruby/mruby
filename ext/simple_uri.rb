##
# SimpleURI

class SimpleURI
  PARAMS_ORDER = [:scheme, :userinfo, :host, :port, :registry, :path, :opaque, :query, :fragment]

  def initialize(*args)
    args = SimpleURI.split(args.first)  if args.length == 1
    @uri = {}
    args.each_index{|idx|
      @uri[PARAMS_ORDER[idx]] = args[idx]
    }
    validate
    self
  end

  def validate
    # TODO
  end

  def self.parse(str)
    args = SimpleURI.split(str)
    SimpleURI.new(*args)
  end

  def self.split(str)
    raise InvalidURIError unless str
    uri = {}

    pattern_scheme_host_path = "\\A(https?|ftp)\://([^\/]+)(\/.*)?\\Z"
    m = Regexp.new(pattern_scheme_host_path).match(str)
    raise InvalidURIError unless m

    uri[:scheme] = m[1]

    if m[2].include?('@')
      uri[:userinfo], uri[:host] = m[2].split('@')
    else
      uri[:host] = m[2]
    end

    if uri[:host].include?(':')
      uri[:host], uri[:port] = uri[:host].split(':')
    end

    if m[3] and m[3].include?('?')
      uri[:path], uri[:query] = m[3].split('?')
    else
      uri[:path] = m[3]
    end

    uri[:path] = "/"  unless uri[:path]

    PARAMS_ORDER.map{|key| uri[key] }
  end

  def [](key); @uri[key.to_sym]; end
  def []=(key, value);  @uri[key.to_sym] = value; end

  def request_uri
    if @uri[:query]
      @uri[:path] + '?' + @uri[:query]
    else
      @uri[:path]
    end
  end

  def to_s
    str = scheme + "://"
    str += userinfo + "@"  if userinfo
    str += host
    str += ":" + port  if port
    str += path if path
    str += "?" + query  if query
    str
  end

  def scheme;   @uri[:scheme];   end
  def userinfo; @uri[:userinfo]; end
  def host;     @uri[:host];     end
  def port;     @uri[:port];     end
  def path;     @uri[:path];     end
  def query;    @uri[:query];    end
  def scheme=(s);   @uri[:scheme]   = s; end
  def userinfo=(s); @uri[:userinfo] = s; end
  def host=(s);     @uri[:host]     = s; end
  def port=(s);     @uri[:port]     = s; end
  def path=(s);     @uri[:path]     = s; end
  def query=(s);    @uri[:query]    = s; end

  class Error < StandardError; end
  class InvalidURIError < SimpleURI::Error; end

  def self.escape(str, unsafe = nil)
    tmp = ''
    str = str.to_s
    str.size.times do |idx|
      chr = str[idx]
      if unsafe.nil? or unsafe.match(chr).nil?
        tmp += chr
      else
        tmp += "%" + chr.unpack("H*").first.upcase
      end
    end
    tmp
  end
end


