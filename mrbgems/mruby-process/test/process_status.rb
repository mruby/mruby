##
# Process::Status Test
#
# ProcessTestUtil comes from process.rb, which the test runner loads first:
# a gem's test files are run in name order.

assert('Process::Status.new is undefined') do
  # As in CRuby, which undefines it: a status reports what happened to a
  # process, so one written by hand reports nothing.  A subclass inherits the
  # absence rather than gaining a way round it.
  assert_raise(NoMethodError) { Process::Status.new(1234, 0) }
  assert_raise(NoMethodError) { Class.new(Process::Status).new(1234, 0) }
end

assert('Process::Status') do
  # A raw status of 0 means "exited with 0" on every port, which is what lets
  # this be asserted without knowing the platform's status layout.
  st = ProcessTestUtil.status(1234, 0)
  assert_equal 1234, st.pid
  assert_equal 0, st.to_i
  assert_true st.exited?
  assert_equal 0, st.exitstatus
  assert_true st.success?
  assert_false st.signaled?
  assert_nil st.termsig
  assert_false st.stopped?
  assert_nil st.stopsig
  assert_false st.coredump?
end

assert('Process::Status keeps its pid and raw status out of instance_variables') do
  # The pid and raw status are internal state, not something a caller wrote or
  # is meant to read back by name; CRuby answers the same empty array, since
  # it keeps both in the object's own struct rather than in an ivar table.
  st = ProcessTestUtil.status(1234, 0)
  assert_equal [], st.instance_variables
end

assert('Process::Status with a status too large for the platform') do
  # A port reads a raw status as the `int` the platform reported it with, so a
  # value that does not fit one is refused where RangeError can be said rather
  # than answered about from bits nobody wrote.  Nothing this gem produces can
  # reach it: waitpid carries a platform status, and mruby-io hands `IO#close`
  # the same `int` on both platforms.
  #
  # This reads the same whatever the build's own Integer is.  Where it is
  # wider than an int, this gem does the refusing; where it is not, a value
  # this far out is a big integer that is turned away before the gem sees it.
  # A build that can name neither raises working the value out, and is skipped.
  big = (2**31 rescue nil)
  skip "this build cannot name a number wider than an int" unless big

  assert_raise(RangeError) { ProcessTestUtil.status(1234, big) }
  assert_raise(RangeError) { ProcessTestUtil.status(1234, -big - 1) }

  # The edges themselves are still statuses, being what an int can carry.
  assert_equal big - 1, ProcessTestUtil.status(1234, big - 1).to_i
  assert_equal(-big, ProcessTestUtil.status(1234, -big).to_i)
end

assert('Process::Status is frozen once built') do
  # What a process did is over by the time there is a status for it, and the
  # pid and the raw status set at construction are what every other question
  # is read back from.  Freezing says so, and keeps the two from being
  # rewritten under the answers; CRuby freezes the status it leaves in $?.
  st = ProcessTestUtil.status(1234, 0)
  assert_true st.frozen?
  # Written through the one door there is: #initialize is where the two are
  # set, and a frozen receiver turns a second pass through it away.
  assert_raise(FrozenError) { st.__send__(:initialize, 1234, 1) }
  assert_equal 1234, st.pid
  assert_equal 0, st.to_i
end

assert('Process::Status subclass is left to finish building itself') do
  # A subclass calls super to have the two set and goes on to set whatever
  # else it is made of, so it is still being built when #initialize returns
  # and freezing there would turn the rest of its construction into a
  # FrozenError.  What gets frozen is what is a status and nothing more.
  cls = Class.new(Process::Status) do
    def initialize(pid, raw_status)
      super
      @tag = "reaped"
    end

    attr_reader :tag
  end

  st = ProcessTestUtil.status(1234, 0, cls)
  assert_false st.frozen?
  assert_equal "reaped", st.tag
  # Still a status, and still read as one.
  assert_equal 1234, st.pid
  assert_equal 0, st.to_i
  assert_true st.exited?
  assert_operator st, :==, ProcessTestUtil.status(1234, 0)
end

assert('Process::Status#==') do
  st = ProcessTestUtil.status(1234, 0)
  assert_operator st, :==, ProcessTestUtil.status(1234, 0)
  # The raw status alone decides, so the pid does not have to match.
  assert_operator st, :==, ProcessTestUtil.status(1235, 0)
  assert_not_operator st, :==, ProcessTestUtil.status(1234, 1)
  assert_operator st, :==, 0
  assert_not_operator st, :==, 1
  assert_not_operator st, :==, "0"
end

assert('Process::Status#== reads a subclass as the status it is') do
  # What decides is the raw status, and carrying one is not something a
  # subclass stops doing.  CRuby never asks about the class here: its
  # Integer#== hands a non-numeric right operand the question back, so the
  # object on the right answers through whichever #== it inherits.  mruby's
  # Integer#== does not hand back, so the unwrapping is this method's to do
  # and it has to read a subclass as a status.
  sub = Class.new(Process::Status)
  st = ProcessTestUtil.status(1234, 0)

  assert_operator st, :==, ProcessTestUtil.status(1234, 0, sub)
  assert_operator ProcessTestUtil.status(1234, 0, sub), :==, st
  # The pid takes no part here either.
  assert_operator st, :==, ProcessTestUtil.status(1235, 0, sub)
  assert_not_operator st, :==, ProcessTestUtil.status(1234, 1, sub)
  assert_not_operator ProcessTestUtil.status(1234, 1, sub), :==, st
end

assert('Process::Status does not answer to_int') do
  # mruby has no implicit-conversion protocol, so nothing would ever call it,
  # and CRuby does not have the method either.
  assert_false ProcessTestUtil.status(1234, 0).respond_to?(:to_int)
end

assert('Process::Status#to_s, #inspect') do
  st = ProcessTestUtil.status(1234, 0)
  assert_equal "pid 1234 exit 0", st.to_s
  assert_equal "#<Process::Status: pid 1234 exit 0>", st.inspect
end

assert('Process::Status#to_s spells a signal out') do
  # The seam with mruby-signal: a status carries a number, and the name it is
  # written with is the one Signal.signame answers with.  Each raw value is
  # checked through the decoding predicates first, so a platform that reads
  # one differently skips rather than fails on an encoding assumed here.
  kill = Signal.list["KILL"]
  st = ProcessTestUtil.status(1234, kill)
  skip "a raw status is not a POSIX wait status on this platform" unless st.signaled?
  assert_equal "pid 1234 SIGKILL (signal #{kill})", st.to_s

  st = ProcessTestUtil.status(1234, kill | 0x80)
  assert_equal "pid 1234 SIGKILL (signal #{kill}) (core dumped)", st.to_s if st.coredump?

  # A raw stopped status can only be spelled where the platform has STOP to
  # spell it with.  The skip above already keeps every such platform out, but
  # asking the table rather than leaning on that keeps this case readable on
  # its own.
  stop = Signal.list["STOP"]
  if stop
    st = ProcessTestUtil.status(1234, (stop << 8) | 0x7f)
    assert_equal "pid 1234 stopped SIGSTOP (signal #{stop})", st.to_s if st.stopped?
  end

  # A number this platform gives no name is written as the bare number.
  unnamed = (1..63).find { |signo| Signal.signame(signo).nil? }
  if unnamed
    st = ProcessTestUtil.status(1234, unnamed)
    assert_equal "pid 1234 signal #{unnamed}", st.to_s if st.signaled?
  end
end
