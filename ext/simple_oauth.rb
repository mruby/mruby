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
