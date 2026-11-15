##
# Random.urandom Test

assert("Random.urandom") do
  a = Random.urandom(16)
  assert_kind_of String, a
  assert_equal 16, a.bytesize
  # binary: each byte is a character, whatever the bytes spell
  assert_equal 16, a.size
  assert_not_equal a, Random.urandom(16)

  assert_equal "", Random.urandom(0)
  assert_equal 1000, Random.urandom(1000).bytesize
end

assert("Random.urandom rejects a bad size") do
  assert_raise(ArgumentError) { Random.urandom(-1) }
  assert_raise(TypeError) { Random.urandom("4") }
  assert_raise(ArgumentError) { Random.urandom }
end
