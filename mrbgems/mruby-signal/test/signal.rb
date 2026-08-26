##
# Signal ISO Test

assert('Signal.signame') do
  assert_equal "KILL", Signal.signame(9)
  assert_equal "TERM", Signal.signame(15)
  assert_equal "INT", Signal.signame(2)
end

assert('Signal.signame with a number that names no signal') do
  # Nothing here is an error: Ruby answers nil for a number the platform does
  # not name, however far out it is.
  assert_nil Signal.signame(-1)
  assert_nil Signal.signame(999)
end

assert('Signal.signame(0) is EXIT') do
  # No platform numbers a signal 0, so the name is Ruby's own rather than one
  # a port reports, and every platform answers with it.
  assert_equal "EXIT", Signal.signame(0)
end

assert('Signal.signame refuses what is not a number') do
  assert_raise(TypeError) { Signal.signame("9") }
  assert_raise(TypeError) { Signal.signame(nil) }
  assert_raise(TypeError) { Signal.signame(:KILL) }
end

assert('Signal.signame answers with the name Ruby answers with') do
  # A host that spells one signal two ways gives both names the same number,
  # and the table is ordered so that the reverse lookup finds the name Ruby
  # reports: ABRT rather than IOT, CHLD rather than CLD, IO rather than POLL.
  aliases = %w[IOT CLD POLL]
  0.upto(64) do |signo|
    name = Signal.signame(signo)
    assert_not_include aliases, name if name
  end
end

assert('Signal.list') do
  list = Signal.list
  assert_kind_of Hash, list
  assert_equal 0, list["EXIT"]
  assert_equal 9, list["KILL"]
  assert_equal 15, list["TERM"]
  assert_nil list["SIGKILL"]  # the names are bare, without the prefix
end

assert('Signal.list agrees with Signal.signame') do
  # Every name maps to a number that names a signal, and the name that comes
  # back maps to the same number.  An alias fails the first test and passes
  # the second, which is what makes the two directions consistent rather than
  # merely reversible.
  Signal.list.each do |name, signo|
    assert_kind_of String, name
    assert_kind_of Integer, signo
    back = Signal.signame(signo)
    assert_not_nil back
    assert_equal signo, Signal.list[back]
  end
end

assert('Signal.list is built fresh each call') do
  list = Signal.list
  list["NOSUCHSIGNAL"] = 1234
  assert_nil Signal.list["NOSUCHSIGNAL"]
end
