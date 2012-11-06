##
# SimpleHttp Test

begin
  require 'assert'
  require 'simple_http'
rescue
  # nothing todo.
end

if Object.const_defined?(:SimpleHttp)
  SEP = SimpleHttp::SEP

  assert('SimpleHttp') do
    SimpleHttp.class == Class
  end

  assert('SimpleHttp superclass') do
    SimpleHttp.superclass == Object
  end

  assert('SimpleHttp#new') do
    h1 = SimpleHttp.new('github.com')
    h2 = SimpleHttp.new('github.com', '8080')

    h1.address == 'github.com' and h1.port == 80 and
    h2.address == 'github.com' and h2.port == 8080
  end

  socket_test_enable = false
  if socket_test_enable
    assert('SimpleHttp#get') do
      h1 = SimpleHttp.new('github.com')
      res = h1.get

      res.class == SimpleHttp::SimpleHttpResponse
    end
  end

  assert('SimpleHttpResponse#new') do
    r  = ""
    r += "HTTP/1.0 200 OK" + SEP
    r += "Server: webserver/0.0.1" + SEP
    r += "Content-Type: text/plain" + SEP
    r += "" + SEP
    r += "Hello, World"
    rep = SimpleHttp::SimpleHttpResponse.new(r)

    rep.body == "Hello, World" and
    rep.code == 200 and
    rep.content_type == "text/plain" and
    rep.content_length == nil and
    rep['content-type'] == 'text/plain'
  end

  report

  if $ko_test > 0 or $kill_test > 0
    raise "simple_http test failed. (KO:#{$ko_test}, Crash:#{$kill_test})"
  end
end
