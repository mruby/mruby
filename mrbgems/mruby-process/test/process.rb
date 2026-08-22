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
  # would refuse the wait in any case: it cannot tell a child from any other
  # process it may open, so it reports ECHILD rather than a stranger's exit
  # code.  See the README.
  def self.child_reason
    return "IO.popen is not available" unless popen?
    return "IO#pid is a process handle, not a pid, on this platform" if windows?
    nil
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

assert('Process.waitpid with a flag it does not define') do
  # A port answers for the bits it is given and says nothing about the rest,
  # so a bit that stands for nothing is refused before it reaches one.  A
  # negative value is refused for the same reason: read as the unsigned value
  # a port takes, it would turn both of the known bits on.
  [4, -1, Process::WNOHANG | Process::WUNTRACED | 4].each do |flags|
    assert_raise(Errno::EINVAL) { Process.waitpid(-1, flags) }
  end
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
  assert_raise(ArgumentError) { Process.kill("NO_SUCH_SIGNAL", Process.pid) }
  assert_raise(ArgumentError) { Process.kill(:NO_SUCH_SIGNAL, Process.pid) }
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
  # the errno a port would have to borrow to report one.  Where the build's
  # Integer is no wider than the platform's own, `big` is a Float and there
  # is nothing here to ask.
  big = 2**31
  skip "this build has no Integer wider than a pid" unless big.is_a?(Integer)

  assert_raise(RangeError) { Process.kill(0, big) }
  assert_raise(RangeError) { Process.kill(big, Process.pid) }
  assert_raise(RangeError) { Process.waitpid(big) }
end

assert('Process::Status.new') do
  # A raw status of 0 means "exited with 0" on every port, which is what lets
  # this be asserted without knowing the platform's status layout.
  st = Process::Status.new(1234, 0)
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

assert('Process::Status#==') do
  st = Process::Status.new(1234, 0)
  assert_operator st, :==, Process::Status.new(1234, 0)
  # The raw status alone decides, so the pid does not have to match.
  assert_operator st, :==, Process::Status.new(1235, 0)
  assert_not_operator st, :==, Process::Status.new(1234, 1)
  assert_operator st, :==, 0
  assert_not_operator st, :==, 1
  assert_not_operator st, :==, "0"
end

assert('Process::Status#to_s, #inspect') do
  st = Process::Status.new(1234, 0)
  assert_equal "pid 1234 exit 0", st.to_s
  assert_equal "#<Process::Status: pid 1234 exit 0>", st.inspect
end

assert('Process::Status._signame answers with the name Ruby answers with') do
  # A host that spells one signal two ways gives both names the same number,
  # and the table is ordered so that the reverse lookup finds the name Ruby
  # reports: ABRT rather than IOT, CHLD rather than CLD, IO rather than POLL.
  aliases = %w[IOT CLD POLL]
  0.upto(64) do |signo|
    name = Process::Status._signame(signo)
    assert_not_include aliases, name if name
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
  assert_equal pid, $?.pid
  assert_true $?.exited?
  assert_equal 3, $?.exitstatus
  assert_false $?.success?
  io.close
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
  assert_equal "KILL", Process::Status._signame($?.termsig)
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

assert('$? after IO.popen') do
  # mruby-io sets $? through Process::Status.new(pid, raw_status) when this
  # gem is present.  Neither gem depends on the other; this is the seam.
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
