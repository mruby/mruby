##
# SimpleURI Test

if Object.const_defined?(:SimpleURI)

  assert('SimpleURI') do
    SimpleURI.class == Class
  end

  assert('SimpleURI superclass') do
    SimpleURI.superclass == Object
  end

  assert('SimpleURI.split 1') do
    u = SimpleURI.split('http://example.jp')

    u[0] == 'http' and        # scheme
    u[1] == nil and           # userinfo
    u[2] == 'example.jp' and  # host
    u[3] == nil and           # port
    u[4] == nil and           # registry
    u[5] == '/' and           # path
    u[6] == nil and           # opaque
    u[7] == nil and           # query
    u[8] == nil               # fragment
  end

  assert('SimpleURI.split 2') do
    u1 = SimpleURI.split('http://example.jp')
    u2 = SimpleURI.split('http://example.jp/')

    u1 == u2
  end

  assert('SimpleURI.split 3') do
    u = SimpleURI.split('http://user:pass@example.jp:8080/path/fuga')

    u[0] == 'http' and        # scheme
    u[1] == 'user:pass'  and  # userinfo
    u[2] == 'example.jp' and  # host
    u[3] == '8080' and        # port
    u[4] == nil and           # registry
    u[5] == '/path/fuga' and  # path
    u[6] == nil and           # opaque
    u[7] == nil and           # query
    u[8] == nil               # fragment
  end

  assert('SimpleURI.split 4') do
    u = SimpleURI.split('http://user:pass@example.jp:8080/path?query=value')

    u[0] == 'http' and        # scheme
    u[1] == 'user:pass'  and  # userinfo
    u[2] == 'example.jp' and  # host
    u[3] == '8080' and        # port
    u[4] == nil and           # registry
    u[5] == '/path' and       # path
    u[6] == nil and           # opaque
    u[7] == 'query=value' and # query
    u[8] == nil               # fragment
  end

  assert('SimpleURI.parse') do
    uri = 'http://user:pass@example.jp:8080/path?query=value'
    u = SimpleURI.parse(uri)

    u.scheme   == 'http' and
    u.userinfo == 'user:pass'  and
    u.host     == 'example.jp' and
    u.port     == '8080' and
    u.path     == '/path' and
    u.query    == 'query=value' and
    u.to_s     == uri  and
    u.request_uri == "/path?query=value"
  end

  assert('SimpleURI.new') do
    uri = 'http://user:pass@example.jp:8080/path?query=value'
    u = SimpleURI.new(uri)
    u2 = SimpleURI.parse(uri)

    t = true
    [:scheme, :userinfo, :host, :port, :path, :query].each do |key|
      t &= (u[key] == u2[key])
    end

    t and
    u.to_s     == uri  and
    u.request_uri == "/path?query=value" and
    u.to_s     == u2.to_s
  end

  assert('SimpleURI.split invalid 1') do
    e1 = nil
    begin
      SimpleURI.parse('')
    rescue => e1
    end

    e2 = nil
    begin
      SimpleURI.parse(nil)
    rescue => e2
    end

    e3 = nil
    begin
      SimpleURI.parse('htp://hoge.jp/')
    rescue => e3
    end

    e1.class == SimpleURI::InvalidURIError and
    e2.class == SimpleURI::InvalidURIError and
    e3.class == SimpleURI::InvalidURIError
  end

  assert('SimpleURI.escape basic test 1') do
    SimpleURI.escape("abc") == "abc"
  end

  assert('SimpleURI.escape basic test 2') do
    reg = Regexp.new("[^a-z]")
    SimpleURI.escape("abc", reg) == "abc" and
    SimpleURI.escape("ABCabc", reg) == "%41%42%43abc"
  end

  assert('SimpleURI.escape valid test 1') do
    reg = Regexp.new("[^a-zA-Z0-9\\-\\.\\_\\~]")
    SimpleURI.escape("ABCabc012-._~", reg) == "ABCabc012-._~" and
    SimpleURI.escape("!@#^&*()=", reg) == "%21%40%23%5E%26%2A%28%29%3D" and
    SimpleURI.escape("http://", reg) == "http%3A%2F%2F"
  end
end
