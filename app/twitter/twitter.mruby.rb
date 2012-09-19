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


##
# Simple Http

class SimpleHttp
  DEFAULTPORT = 80
  HTTP_VERSION = "HTTP/1.0"
  DEFAULT_ACCEPT = "*/*"
  SEP = "\r\n"

  def initialize(address, port = DEFAULTPORT)
    @socket
    @uri = {}
    @uri[:address] = address
    @uri[:port] = port ? port.to_i : DEFAULTPORT
    self
  end

  def address; @uri[:address]; end
  def port; @uri[:port]; end

  def get(path = "/", request = nil)
    request("GET", path, request)
  end

  def post(path = "/", request = nil)
    request("POST", path, request)
  end

  # private
  def request(method, path, req)
    @uri[:path] = path
    if @uri[:path].nil?
      @uri[:path] = "/"
    elsif @uri[:path][0] != "/"
      @uri[:path] = "/" + @uri[:path]
    end
    request_header = create_request_header(method.upcase.to_s, req)
    response_text = send_request(request_header)
    SimpleHttpResponse.new(response_text)
  end

  def send_request(request_header)
    @socket = TCPSocket.new(@uri[:address], @uri[:port])
    @socket.write(request_header)
    response_text = ""
    while (t = @socket.read(1024))
      response_text += t
    end
    @socket.close
    response_text
  end

  def create_request_header(method, req)
    req = {}  unless req
    str = ""
    body   = ""
    str += sprintf("%s %s %s", method, @uri[:path], HTTP_VERSION) + SEP
    header = {}
    req.each do |key,value|
      header[key.capitalize] = value
    end
    header["Host"] = @uri[:address]  unless header.keys.include?("Host")
    header["Accept"] = DEFAULT_ACCEPT  unless header.keys.include?("Accept")
    header["Connection"] = "close"
    if header["Body"]
      body = header["Body"]
      header.delete("Body")
    end
    if method == "POST" && (not header.keys.include?("content-length".capitalize))
        header["Content-Length"] = (body || '').length
    end
    header.keys.sort.each do |key|
      str += sprintf("%s: %s", key, header[key]) + SEP
    end
    str + SEP + body
  end

  class SimpleHttpResponse
    SEP = SimpleHttp::SEP
    def initialize(response_text)
      @response = {}
      if response_text.include?(SEP + SEP)
        @response["header"], @response["body"] = response_text.split(SEP + SEP)
      else
        @response["header"] = response_text
      end
      parse_header
      self
    end

    def [](key); @response[key]; end
    def []=(key, value);  @response[key] = value; end

    def header; @response['header']; end
    def body; @response['body']; end
    def status; @response['status']; end
    def code; @response['code']; end
    def date; @response['date']; end
    def content_type; @response['content-type']; end
    def content_length; @response['content-length']; end

    def each(&block)
      if block
        @response.each do |k,v| block.call(k,v) end
      end
    end
    def each_name(&block)
      if block
        @response.each do |k,v| block.call(k) end
      end
    end

    # private
    def parse_header
      return unless @response["header"]
      h = @response["header"].split(SEP)
      if h[0].include?("HTTP/1")
        @response["status"] = h[0].split(" ", 2).last
        @response["code"]   = h[0].split(" ", 3)[1].to_i
      end
      h.each do |line|
        if line.include?(": ")
          k,v = line.split(": ")
          @response[k.downcase] = v
        end
      end
    end
  end

end
##
# SimpleOAuth

class SimpleOAuth
  def initialize(consumer_key, consumer_secret, token, token_secret)
    @consumer_key = consumer_key
    @consumer_secret = consumer_secret
    @token = token
    @token_secret = token_secret
    # This class supports only 'HMAC-SHA1' as signature method at present.
    @signature_method = 'HMAC-SHA1'
    self
  end

  def get(url, headers = {})
    request("GET", url, nil, headers)
  end

  def head(url, headers = {})
    request("HEAD", url, nil, headers)
  end

  def post(url, body = nil, headers = {})
    request("POST", url, body, headers)
  end

  def put(url, body = nil, headers = {})
    request("PUT", url, body, headers)
  end

  def delete(url, headers = {})
    request("DELETE", url, nil, headers)
  end

  # private
  def request(method, url, body = nil, headers = {})
    url = SimpleURI.parse(url)
    request = create_http_request(method, body, headers)
    request['Authorization'] = auth_header(method, url, request["body"])
    # XXX: String contains NUL
    host = url.host.to_sym.to_s
    SimpleHttp.new(host, url.port).request(method, url.request_uri, request)
  end

  RESERVED_CHARACTERS = Regexp.new("[^a-zA-Z0-9\\-\\.\\_\\~]")

  def escape(value)
    SimpleURI.escape(value, RESERVED_CHARACTERS)
  end

  def encode_parameters(params, delimiter = '&', quote = nil)
    if params.is_a?(Hash)
      params = params.map do |key, value|
        sprintf("%s=%s%s%s", escape(key), quote, escape(value), quote)
      end
    else
      params = params.map { |value| escape(value) }
    end
    delimiter ? params.join(delimiter) : params
  end

  VERSION = '0.1'
  USER_AGENT = "SimpleOAuth/" + VERSION

  def create_http_request(method, body, headers)
    method = method.upcase.to_s
    request = {}
    request['User-Agent'] = USER_AGENT
    if method == "POST" || method == "PUT"
      request["body"] = body.is_a?(Hash) ? encode_parameters(body) : body.to_s
      request["Content-Type"] = 'application/x-www-form-urlencoded'
      request["Content-Length"] = (request["body"] || '').length
    end
    request
  end

  def auth_header(method, url, body)
    parameters = oauth_parameters
    parameters["oauth_signature"] = signature(method, url, body, parameters)
    'OAuth ' + encode_parameters(parameters, ', ', '"')
  end

  OAUTH_VERSION = '1.0'

  def oauth_parameters
    {
      "oauth_consumer_key" => @consumer_key,
      "oauth_token" => @token,
      "oauth_signature_method" => @signature_method,
      "oauth_timestamp" => timestamp,
      "oauth_nonce" => nonce,
      "oauth_version" => OAUTH_VERSION
    }
  end

  def timestamp
    Time.now.to_i.to_s
  end

  def nonce
    Digest::MD5.hexdigest(timestamp)
  end

  def signature(*args)
    base64(digest_hmac_sha1(signature_base_string(*args)))
  end

  def base64(value)
    # [ value ].pack('m').gsub(/\n/, '')
    r = [ value ].pack('m')
    r.include?("\n") ? r.split("\n").join("") : r
  end

  def digest_hmac_sha1(value)
    # OpenSSL::HMAC.digest(OpenSSL::Digest::SHA1.new, secret, value)
    Digest::HMAC.digest(value, secret, Digest::SHA1)
  end

  def secret
    escape(@consumer_secret) + '&' + escape(@token_secret)
  end

  def signature_base_string(method, url, body, parameters)
    method = method.upcase
    base_url = signature_base_url(url)
    parameters = normalize_parameters(parameters, body, url.query)
    #debug
    # p ['signature_base_string', method, base_url, parameters]
    encode_parameters([ method, base_url, parameters ])
  end

  def signature_base_url(url)
    SimpleURI.new(url.scheme, url.userinfo, url.host, nil, nil, url.path, nil, nil, nil).to_s
  end

  def normalize_parameters(parameters, body, query)
    parameters = encode_parameters(parameters, nil)
    parameters += body.split('&') if body
    parameters += query.split('&') if query
    parameters.sort.join('&')
  end
end
#!/usr/bin/env ruby
# -*- coding: utf-8 -*-


CONSUMER_KEY        = 'YOUR_CONSUMER_KEY'
CONSUMER_SECRET     = 'YOUR_CONSUMER_SECRET'
ACCESS_TOKEN        = 'YOUR_ACCESS_TOKEN'
ACCESS_TOKEN_SECRET = 'YOUR_ACCESS_TOKEN_SECRET'

simple_oauth = SimpleOAuth.new(CONSUMER_KEY, CONSUMER_SECRET, ACCESS_TOKEN, ACCESS_TOKEN_SECRET)

# post tweet
puts "#========== post tweet ============"
msg = sprintf("test %s ", Time.now.to_s)
response = simple_oauth.post('http://twitter.com/statuses/update.json', {
  :status => msg
})
if response.code.to_i == 200
  # puts response.body
  puts sprintf("メッセージ: '%s' をツイートしました！！", msg)
else
  puts sprintf("メッセージ: '%s' のツイートに失敗しました ＞＜", msg)
  puts response.body
  raise "Request failed: " + response.code.to_s
end
puts "#=================================="


# get timeline
puts "#========== get timeline =========="
response = simple_oauth.get('http://twitter.com/statuses/friends_timeline.json?count=1')
puts "#response"
puts "Status: " + response["status"]
puts "Content-Type: " + response["content-type"]
puts "Content-Length: " + response["content-length"]
if response.code.to_i == 200
  puts response.body
  puts sprintf("%d bytes fetched.", response.body.size)
else
  puts response.body
  raise "Request failed: " + response.code.to_s
end

