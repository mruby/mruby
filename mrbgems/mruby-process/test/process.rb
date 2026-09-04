##
# Process ISO Test

module ProcessTestUtil
  # mruby-io is a test-only dependency: mruby-process itself never needs it,
  # but a child process is the only honest way to test waiting on one, and
  # IO.popen is how this build makes children.
  def self.popen?
    Object.const_defined?(:IO) && IO.respond_to?(:popen)
  end

  def self.windows?
    Object.const_defined?(:File) && !File::ALT_SEPARATOR.nil?
  end

  # Why the tests that need a child are POSIX-only for now.  On Windows
  # mruby-io hands out a process HANDLE as IO#pid, not a process ID, so
  # there is nothing there to give Process.waitpid or Process.kill, and its
  # IO.popen sets $? through a path that never fires; both are mruby-io's to
  # fix, and neither is what this gem is being tested for.  The Windows port
  # declares no wait in any case: it cannot tell a child from any other
  # process it may open, so it does not wait at all rather than report a
  # stranger's exit code.  See the README.
  def self.child_reason
    return "IO.popen is not available" unless popen?
    return "IO#pid is a process handle, not a pid, on this platform" if windows?
    nil
  end

  # Whether this port declares a wait.  The four spellings and the wait
  # behind them come and go together, which the test below pins, so one of
  # them is asked for all.
  def self.wait?
    Process.respond_to?(:waitpid)
  end

  # Whether Process.kill turned +pid+ down rather than passing it on.  What a
  # non-positive pid selects is the platform's to say, so its answer cannot be
  # asserted here; that it was asked at all can be.
  def self.kill_refused?(pid)
    Process.kill(0, pid)
    false
  rescue ArgumentError
    true
  rescue StandardError
    false
  end

  # Write a status carrying +raw+ for +pid+.  Process::Status.new is
  # undefined, here as in CRuby, so a status is built the way mruby-io builds
  # the one it sets $? to: by allocating an instance and initializing it,
  # which is what the C helper does.  +cls+ names the class to build, since a
  # subclass has no `new` to reach either.
  def self.status(pid, raw, cls = Process::Status)
    ProcessStatusTest.build(pid, raw, cls)
  end

  # The four clocks Process names.  The ids are 0 to 3, so `clocks.size` is
  # the first number that names none of them.
  def self.clocks
    [Process::CLOCK_REALTIME, Process::CLOCK_MONOTONIC,
     Process::CLOCK_PROCESS_CPUTIME_ID, Process::CLOCK_THREAD_CPUTIME_ID]
  end

  # Whether this platform has the clock +id+ names.  Every constant is
  # defined everywhere, and a port without the clock behind one refuses to
  # read it, so a test that means to exercise a reading asks first.  Asked in
  # seconds, which every build's Integer can carry.
  def self.clock?(id)
    Process.clock_gettime(id, :second)
    true
  rescue Errno::EINVAL
    false
  end

  # Whether a reading of +id+ in +unit+ is one this build can answer with.
  # An Integer of 32 bits carries a clock in seconds and little finer, and a
  # build without bigints has nothing wider to put such a reading in, so a
  # test that reads nanoseconds asks rather than pinning a build's width.
  def self.fits?(id, unit)
    Process.clock_gettime(id, unit)
    true
  rescue RangeError
    false
  end

  # The reading of +sec+ seconds and +nsec+ nanoseconds answered in +unit+,
  # as the decimal the Integer of it spells, or nil where this build's
  # Integer is too narrow to hold it and the conversion says so.  Both the
  # seconds handed in and the answer read back are decimal Strings: the
  # values worth asking about here are the ones a build may have no way to
  # write as a literal.
  def self.convert(sec, nsec, unit)
    ProcessClockTest.convert(sec, nsec, unit).to_s
  rescue RangeError
    nil
  end

  # Whether this build has a Float for the float units and Process.times to
  # answer in.
  def self.float?
    Object.const_defined?(:Float)
  end

  # Start a child running +cmd+ through a shell, or return nil where this
  # build has no child to give.
  def self.spawn(cmd)
    return nil if child_reason
    IO.popen(cmd)
  rescue NotImplementedError
    nil
  end
end

assert('Process.pid') do
  pid = Process.pid
  assert_kind_of Integer, pid
  assert_true pid > 0
  assert_equal pid, Process.pid
  assert_equal pid, $$
end

assert('Process.ppid') do
  ppid = Process.ppid
  assert_kind_of Integer, ppid
  assert_true ppid >= 0
end

assert('Process::WNOHANG, Process::WUNTRACED') do
  # Portable bits of mruby's own, not the host's, so they are simply two
  # distinct flags that can be combined.
  assert_kind_of Integer, Process::WNOHANG
  assert_kind_of Integer, Process::WUNTRACED
  assert_not_equal Process::WNOHANG, Process::WUNTRACED
  assert_equal 0, Process::WNOHANG & Process::WUNTRACED
end

assert('the wait is what the port declares') do
  # Whether there is a wait is the port's to say, through its
  # process_hal_features.h, and it says it once for all four spellings: a
  # port that declares none leaves all four marked as not implemented, which
  # `respond_to?` answers false for and a call raises NotImplementedError
  # from.  The two option constants are the shape of the call and stay
  # defined either way.
  spellings = [:wait, :waitpid, :wait2, :waitpid2]
  spellings.each do |name|
    assert_equal ProcessTestUtil.wait?, Process.respond_to?(name), name.to_s
  end
  unless ProcessTestUtil.wait?
    spellings.each do |name|
      assert_raise(NotImplementedError, name.to_s) { Process.__send__(name) }
    end
  end
  assert_true Process.const_defined?(:WNOHANG)
  assert_true Process.const_defined?(:WUNTRACED)
end

assert('Process.waitpid with a flag it does not define') do
  skip "this port declares no wait" unless ProcessTestUtil.wait?
  # A port answers for the bits it is given and says nothing about the rest,
  # so a bit that stands for nothing is refused before it reaches one.  A
  # negative value is refused for the same reason: read as the unsigned value
  # a port takes, it would turn both of the known bits on.
  [4, -1, Process::WNOHANG | Process::WUNTRACED | 4].each do |flags|
    assert_raise(Errno::EINVAL) { Process.waitpid(-1, flags) }
  end
end

assert('Process.waitpid reports the error by itself') do
  skip "this port declares no wait" unless ProcessTestUtil.wait?
  # What a SystemCallError message carries after the error is the object the
  # call was working on, the way `File.open` names the path it could not open.
  # A wait has no such object and CRuby names nothing here.  Compared against
  # the text this platform gives that errno rather than against a literal, so
  # the wording itself is not pinned.
  e = assert_raise(Errno::EINVAL) { Process.waitpid(-1, 4) }
  assert_equal SystemCallError.new(e.errno).message, e.message
end

assert('Process.kill with signal 0') do
  # Signal 0 sends nothing; it only asks whether the process can be signalled.
  assert_equal 1, Process.kill(0, Process.pid)
  assert_equal 2, Process.kill(0, Process.pid, Process.pid)
end

assert('Process.kill does not take "EXIT" for signal 0') do
  # "EXIT" names signal 0 only where a handler is being set, which is not
  # something this gem does; as a signal to send, Ruby refuses the name and
  # leaves the number as the portable way to ask for the null signal.
  assert_raise(ArgumentError) { Process.kill("EXIT", Process.pid) }
  assert_raise(ArgumentError) { Process.kill(:EXIT, Process.pid) }
  assert_raise(ArgumentError) { Process.kill("SIGEXIT", Process.pid) }
end

assert('Process.kill with no process to signal') do
  # A signal on its own names nothing to send it to, which CRuby reports as
  # a missing argument rather than as a call that signalled nobody.
  assert_raise(ArgumentError) { Process.kill(0) }
  assert_raise(ArgumentError) { Process.kill(:TERM) }
end

assert('Process.kill with an unknown signal name') do
  # The name is reported with the "SIG" prefix put back on, whether or not it
  # was written with one, which is how Ruby reports it.
  assert_raise_with_message(ArgumentError, "unsupported signal 'SIGNO_SUCH_SIGNAL'") do
    Process.kill("NO_SUCH_SIGNAL", Process.pid)
  end
  assert_raise_with_message(ArgumentError, "unsupported signal 'SIGNO_SUCH_SIGNAL'") do
    Process.kill(:NO_SUCH_SIGNAL, Process.pid)
  end
  assert_raise_with_message(ArgumentError, "unsupported signal 'SIGNO_SUCH'") do
    Process.kill("SIGNO_SUCH", Process.pid)
  end
end

assert('Process.kill with a signal name too long for any signal') do
  # A name past the lookup buffer's width is still an unsupported signal, not
  # a different kind of error; the name is reported in full rather than
  # replaced by a generic message.
  long_name = "A" * 40
  assert_raise_with_message(ArgumentError, "unsupported signal 'SIG#{long_name}'") do
    Process.kill(long_name, Process.pid)
  end
  assert_raise_with_message(ArgumentError, "unsupported signal 'SIG#{long_name}'") do
    Process.kill(long_name.to_sym, Process.pid)
  end
end

assert('Process.kill with a name that is nothing but the prefix') do
  # "SIG" loses the prefix like any longer name and leaves nothing behind, and
  # a name that was empty to begin with reaches the same place.  Neither is an
  # error of its own; both are reported as the signal that "SIG" alone names,
  # which is none.
  ["SIG", ""].each do |name|
    assert_raise_with_message(ArgumentError, "unsupported signal 'SIG'") do
      Process.kill(name, Process.pid)
    end
    assert_raise_with_message(ArgumentError, "unsupported signal 'SIG'") do
      Process.kill(name.to_sym, Process.pid)
    end
  end
end

assert('Process.kill with a signal of no signal type') do
  # What cannot be a signal at all is refused by its class, which Ruby
  # reports as ArgumentError: the call is not converting the argument, it is
  # naming the kinds it takes.
  # Under MRB_NO_FLOAT the literal below is Integer 0, which is a signal
  # number `kill` takes rather than a class it refuses.
  if Object.const_defined?(:Float)
    assert_raise_with_message(ArgumentError, "bad signal type Float") do
      Process.kill(15.0, Process.pid)
    end
  end
  assert_raise_with_message(ArgumentError, "bad signal type NilClass") do
    Process.kill(nil, Process.pid)
  end
  assert_raise_with_message(ArgumentError, "bad signal type Array") do
    Process.kill([], Process.pid)
  end

  # A big integer is an Integer, but not the Integer the signal branch reads,
  # so it is refused the same way, as CRuby refuses it.  Worked out rather
  # than written down: a literal this wide would drop the whole file from a
  # build that cannot parse it.
  huge = ((2**35) * (2**35) rescue nil)
  if huge
    assert_raise_with_message(ArgumentError, "bad signal type Integer") do
      Process.kill(huge, Process.pid)
    end
  end
end

assert('Process.kill with a signal name holding a NUL') do
  # The name reaches the port as a C string, so a NUL in it would name the
  # part before it.  "TERM\0suffix" must not be a way to spell TERM.
  assert_raise(ArgumentError) { Process.kill("TERM\0suffix", Process.pid) }
  assert_raise(ArgumentError) { Process.kill(:"TERM\0suffix", Process.pid) }
end

assert('Process.kill rejects the process-group signal forms') do
  # Naming a process group through the signal is out of this gem's scope for
  # now, and saying so beats signalling the process instead.
  assert_raise(ArgumentError) { Process.kill(-15, Process.pid) }
  assert_raise(ArgumentError) { Process.kill("-TERM", Process.pid) }
end

assert('Process.kill passes the pid selectors on') do
  # A pid selects processes the way kill(2) reads it, and reading it is the
  # platform's job: POSIX takes 0 for the caller's process group, -1 for every
  # process the caller may signal, and a number below -1 for the group whose
  # ID is -pid, while Windows has no such selectors and answers ESRCH.  Both
  # are answers rather than refusals, and it is the refusal that is pinned
  # here, since what the selectors reach depends on the host: whether the test
  # runner leads its own process group, and whether there is any other process
  # it may signal.  Signal 0 throughout, so nothing is signalled.
  assert_false ProcessTestUtil.kill_refused?(0), "a pid of 0"
  assert_false ProcessTestUtil.kill_refused?(-1), "a pid of -1"
  # Not -Process.pid: where this runs as process 1, that is -1 again and the
  # third selector would never be asked about.
  assert_false ProcessTestUtil.kill_refused?(-(Process.pid + 1)), "a pid below -1"

  # The caller's own process group is one the caller is always in, so where
  # the platform reads the selectors as POSIX does, this one selects.
  assert_equal 1, Process.kill(0, 0) unless ProcessTestUtil.windows?
end

assert('a pid or a signal number too large for the platform') do
  # What is wrong with these is their size, so RangeError is the answer, not
  # the errno a port would have to borrow to report one.
  #
  # A build whose own Integer cannot hold this and has no big integer to
  # promote it to raises while working the value out, so the value is asked
  # for rather than assumed.  `is_a?(Integer)` would not do: a big integer is
  # an Integer, so a build that has them answers yes and says nothing about
  # the width in question.
  big = (2**31 rescue nil)
  skip "this build cannot name a number wider than a pid" unless big

  assert_raise(RangeError) { Process.kill(0, big) }
  assert_raise(RangeError) { Process.waitpid(big) } if ProcessTestUtil.wait?

  # As a signal, its size is only what is wrong with it where the build's own
  # Integer carries it: a build that promoted it to a big integer refuses it
  # as no signal type instead, tested above.  The builds are told apart by
  # identity, which every Integer has and no big integer object does.
  if big.equal?(2**31)
    assert_raise(RangeError) { Process.kill(big, Process.pid) }
  end
end

assert('Process.waitpid') do
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 3")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  assert_equal pid, Process.waitpid(pid)

  # waitpid publishes what it reaped through $?
  assert_kind_of Process::Status, $?
  assert_true $?.frozen?
  assert_equal pid, $?.pid
  assert_true $?.exited?
  assert_equal 3, $?.exitstatus
  assert_false $?.success?
  io.close
end

assert('Process.waitpid through a replaced Process::Status#initialize') do
  # A status is written into the object without calling #initialize where it
  # is the one the gem defines, so that the stretch between reaping a child
  # and recording what it did runs no method a program can replace.  One that
  # was replaced is still called: writing it is asking for it to run, and by
  # then the status it is handed is the one the wait reported.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 6")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  seen = nil
  Process::Status.class_eval do
    alias_method :__test_initialize, :initialize
    define_method(:initialize) do |child, raw_status|
      seen = [child, raw_status]
      __test_initialize(child, raw_status)
    end
  end
  begin
    assert_equal pid, Process.waitpid(pid)
    assert_kind_of Array, seen
    assert_equal pid, seen[0]
    # The status it built is the one published, and reads as any other does.
    assert_equal pid, $?.pid
    assert_equal 6, $?.exitstatus
    assert_equal seen[1], $?.to_i
  ensure
    Process::Status.class_eval do
      alias_method :initialize, :__test_initialize
      remove_method :__test_initialize
    end
    io.close
  end
end

assert('Process.waitpid with Process::WNOHANG') do
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  # `exec` so that the pid this knows is the one that sleeps.  IO.popen runs
  # the command under /bin/sh, and a shell that forks it rather than replacing
  # itself with it leaves the sleep running once the shell has been killed.
  io = ProcessTestUtil.spawn("exec sleep 30")
  skip "IO.popen is not available" unless io

  # Nothing has finished, so the wait returns at once with nothing to report.
  assert_nil Process.waitpid(io.pid, Process::WNOHANG)
  assert_nil $?

  Process.kill(:KILL, io.pid)
  assert_equal io.pid, Process.waitpid(io.pid)
  assert_true $?.signaled?
  assert_false $?.exited?
  assert_nil $?.exitstatus
  assert_nil $?.success?
  assert_equal "KILL", Signal.signame($?.termsig)
  io.close
end

assert('Process.waitpid with no child to wait for') do
  # A pid reaped once is gone; waiting on it again has nothing to find.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 0")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  Process.waitpid(pid)
  assert_raise(Errno::ECHILD) { Process.waitpid(pid) }
  io.close
end

assert('Process.kill reports the error by itself') do
  # Signalling names no object either, so its message is the error alone.  A
  # reaped pid is one nothing answers to any more, which is how the failure is
  # reached without naming a process that might belong to someone else.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 0")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  Process.waitpid(pid)
  io.close

  e = assert_raise(Errno::ESRCH) { Process.kill(0, pid) }
  assert_equal SystemCallError.new(e.errno).message, e.message
end

assert('Process.wait') do
  # The same wait under Ruby's other name for it.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 4")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  assert_equal pid, Process.wait(pid)
  assert_kind_of Process::Status, $?
  assert_equal pid, $?.pid
  assert_equal 4, $?.exitstatus
  io.close
end

assert('Process.waitpid2, Process.wait2') do
  # The pid and the status of one wait, returned together.  $? is set to the
  # same status, so the pair is a second way to reach it and not a second
  # wait: asking twice would find nothing to wait for the second time.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 4")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  result = Process.waitpid2(pid)
  assert_kind_of Array, result
  assert_equal 2, result.size
  assert_equal pid, result[0]
  assert_kind_of Process::Status, result[1]
  assert_equal 4, result[1].exitstatus
  assert_equal pid, result[1].pid
  # The same object, not merely a status that reads the same: one wait
  # happened, and both ways of reaching it reach that one.
  assert_true result[1].equal?($?)
  io.close
end

assert('Process.wait2 with Process::WNOHANG') do
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  # `exec` so that the pid this knows is the one that sleeps; see
  # Process.waitpid with Process::WNOHANG above.
  io = ProcessTestUtil.spawn("exec sleep 30")
  skip "IO.popen is not available" unless io

  # Nothing has finished, so there is no pair to hand back.
  assert_nil Process.wait2(io.pid, Process::WNOHANG)
  assert_nil $?

  Process.kill(:KILL, io.pid)
  pid, status = Process.wait2(io.pid)
  assert_equal io.pid, pid
  assert_true status.signaled?
  assert_nil status.exitstatus
  assert_equal "KILL", Signal.signame(status.termsig)
  io.close
end

assert('$? after IO.popen') do
  # mruby-io builds the status it sets $? to when this gem is present, by
  # allocating one and initializing it with the pid and the raw status.
  # Neither gem depends on the other; this is the seam.
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason
  io = ProcessTestUtil.spawn("exit 0")
  skip "IO.popen is not available" unless io

  io.read
  pid = io.pid
  io.close
  assert_kind_of Process::Status, $?
  assert_equal pid, $?.pid
  assert_true $?.success?
end
