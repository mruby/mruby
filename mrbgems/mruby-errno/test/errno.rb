assert('Errno') do
  assert_equal(Module, Errno.class)
end

assert('SystemCallError') do
  assert_equal(Class, SystemCallError.class)
end

assert('SystemCallError superclass') do
  assert_equal(StandardError, SystemCallError.superclass)
end

assert('SystemCallError#initialize') do
  assert_equal("unknown error - a", SystemCallError.new("a").message)
  assert_equal("Unknown error: 12345 - a", SystemCallError.new("a", 12345).message)
  assert_equal("Unknown error: 12345", SystemCallError.new(12345).message)
end

assert('SystemCallError#errno') do
  assert_equal 1, SystemCallError.new("a", 1).errno
  assert_equal 1, SystemCallError.new(1).errno
  assert_equal 12345, SystemCallError.new("a", 12345).errno
  assert_equal 23456, SystemCallError.new(23456).errno
end

assert('SystemCallError#initialize with an errno past int') do
  # 2**32 + 2 was cut to 2 and made an Errno::ENOENT. Computed, not written
  # out, so the file still compiles where mrb_int is 32 bits.
  # A big integer is not an mrb_int either, and is taken as no errno at all.
  shift = 32
  big = nil
  wide = begin
    big = (1 << shift) + 2  # RangeError where mrb_int is 32 bits and bigint is absent
    [][big]                 # nil for an mrb_int index, RangeError for a big integer
    true
  rescue RangeError
    false
  end
  skip "needs 64-bit mrb_int" unless wide
  assert_raise(RangeError) { SystemCallError.new("a", big) }
  assert_raise(RangeError) { SystemCallError.new(-big) }
end

assert('SystemCallError#inspect') do
  assert_equal("#<SystemCallError: unknown error - a>", SystemCallError.new("a").inspect)
end

assert('Errno::NOERROR') do
  assert_equal(Class, Errno::NOERROR.class)
end

# Is there any platform does not have EPERM?
assert('Errno::EPERM') do
  assert_equal(Class, Errno::EPERM.class)
end

assert('Errno::EPERM superclass') do
  assert_equal(SystemCallError, Errno::EPERM.superclass)
end

assert('Errno::EPERM::Errno') do
  assert_true(Errno::EPERM::Errno.is_a?(Fixnum))
end

assert('Errno::EPERM#message') do
  msg = Errno::EPERM.new.message
  assert_equal("#{msg} - a", Errno::EPERM.new("a").message)
end

assert('Errno::EPERM#inspect') do
  e = Errno::EPERM.new
  msg = e.message
  assert_equal("#<Errno::EPERM: #{msg}>", e.inspect)
  assert_equal("#<Errno::EPERM: #{msg} - a>", Errno::EPERM.new("a").inspect)
end
