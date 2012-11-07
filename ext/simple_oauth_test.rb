##
# SimpleOAuth Test

begin
  require 'assert'
  require 'simple_http'
  require 'simple_uri'
  require 'simple_oauth'
rescue
  # nothing todo.
end

TESTAPI_URL = "http://term.ie/oauth/example/echo_api.php"
CONSUMER_KEY        = "key"
CONSUMER_SECRET     = "secret"
ACCESS_TOKEN        = "accesskey"
ACCESS_TOKEN_SECRET = "accesssecret"

if Object.const_defined?(:SimpleHttp) and Object.const_defined?(:SimpleURI) and Object.const_defined?(:SimpleOAuth)

  assert('SimpleOAuth') do
    SimpleOAuth.class == Class
  end

  assert('SimpleOAuth superclass') do
    SimpleOAuth.superclass == Object
  end

  assert('SimpleOAuth.new') do
    method = 'GET'
    body = ''
    headers = {}
    url = SimpleURI.parse(TESTAPI_URL + "?method=foo&bar=baz")

    simple_oauth = SimpleOAuth.new(CONSUMER_KEY, CONSUMER_SECRET, ACCESS_TOKEN, ACCESS_TOKEN_SECRET)
    request = simple_oauth.create_http_request(method, body, headers)
    auth = simple_oauth.auth_header(method, url, request['body'])
    response = simple_oauth.get(TESTAPI_URL)

    request['User-Agent'] == 'SimpleOAuth/0.1' and
    Regexp.new("^OAuth\s").match(auth) and
    response.class == SimpleHttp::SimpleHttpResponse and
    response.code == 200
  end

  report

  if $ko_test > 0 or $kill_test > 0
    raise "simple_oauth test failed. (KO:#{$ko_test}, Crash:#{$kill_test})"
  end
end

