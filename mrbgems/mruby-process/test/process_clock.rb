##
# Process.clock_gettime, Process.clock_getres and Process.times Test
#
# ProcessTestUtil comes from process.rb, which the test runner loads first:
# a gem's test files are run in name order.

assert('Process.times') do
  skip "this build has no Float" unless ProcessTestUtil.float?

  t = Process.times
  assert_kind_of Process::Tms, t
  [t.utime, t.stime, t.cutime, t.cstime].each do |v|
    assert_kind_of Float, v
    assert_operator v, :>=, 0.0
  end
end

assert('Process.times keeps the class Tms was created as') do
  skip "this build has no Float" unless ProcessTestUtil.float?

  original = Process::Tms
  Process.__send__(:remove_const, :Tms)
  Process::Tms = Struct.new(:other)
  begin
    t = Process.times
    assert_true t.class.equal?(original)
    assert_false t.class.equal?(Process::Tms)
  ensure
    Process.__send__(:remove_const, :Tms)
    Process::Tms = original
  end
end

assert('Process.times counts CPU time this process spends') do
  # utime or stime, at least one of them, is worth more after genuine CPU
  # work than before it; which of the two the work lands in is the
  # scheduler's to say. A busy loop is read as user time on every platform
  # this gem supports, so utime alone would do, but stime is read too in
  # case a host charges some of it to system time.
  #
  # The CLOCK_MONOTONIC test below reads two clocks back to back with no
  # work forced between them, so it can only ask for a reading that is not
  # smaller. This one drives a busy loop between the two readings, so a
  # strict increase is asked for: a HAL bug that pins the reading at zero,
  # or anywhere else it never moves from, has to fail this one where >=
  # would have let it pass.
  #
  # How much work that takes is not a number a test may fix. A CPU time
  # total is accumulated rather than read: Win32 charges a process the clock
  # interrupts it was found running at, 15.625ms of one at a time, so the
  # reading steps rather than flows, and a whole round of this loop can sit
  # inside one step. On a virtualised runner it can sit inside several,
  # which is what made a fixed count of iterations fail here. So the loop is
  # driven again until the reading moves, which is how CRuby's own spec for
  # this property asks it (`1 until Process.times.utime > user`). The bound
  # is what keeps a pinned reading a failure rather than a hung suite; a
  # clock that moves leaves after the first round.
  skip "this build has no Float" unless ProcessTestUtil.float?

  before = Process.times
  after = before
  20.times do
    n = 0
    n += 1 while n < 3_000_000
    after = Process.times
    break if after.utime + after.stime > before.utime + before.stime
  end

  assert_operator after.utime + after.stime, :>, before.utime + before.stime
end

assert('Process.times without a Float') do
  skip "this build has a Float" if ProcessTestUtil.float?

  assert_raise(NotImplementedError) { Process.times }
end

assert('Process.times takes no argument') do
  # The count is checked by the VM before the C function runs, so this holds
  # on every build: without a Float the bare call above raises
  # NotImplementedError, but an argument still raises ArgumentError first.
  assert_raise(ArgumentError) { Process.times(1) }
  assert_raise(ArgumentError) { Process.times(1, 2) }
end

assert('Process.times is laid as a module function') do
  # The method is defined by two raw defines from a proc rather than by
  # mrb_define_module_function_id, so the shape that call would have given,
  # public on the singleton and private as an instance method, is pinned
  # here rather than assumed.
  assert_true Process.respond_to?(:times)
  assert_false Process.public_instance_methods.include?(:times)
  assert_true Process.private_instance_methods.include?(:times)
end

assert('Process.times and reaped children') do
  # cutime and cstime total the CPU time of children this process has
  # reaped, and nothing else: a child still running, or one that has
  # already exited but sits unwaited as a zombie, does not contribute yet.
  # "exit 0" burns too little CPU to tell that apart from a HAL that always
  # answers 0, so this spawns a child that spins for a measurable stretch,
  # confirms it is not yet counted while unreaped, and that reaping it
  # through waitpid makes cutime + cstime increase strictly.
  skip "this build has no Float" unless ProcessTestUtil.float?
  skip ProcessTestUtil.child_reason if ProcessTestUtil.child_reason

  baseline = Process.times
  io = ProcessTestUtil.spawn('i=0; while [ "$i" -lt 200000 ]; do i=$((i+1)); done; exit 0')
  skip "IO.popen is not available" unless io

  io.read
  before = Process.times
  assert_equal baseline.cutime, before.cutime
  assert_equal baseline.cstime, before.cstime

  Process.waitpid(io.pid)
  io.close
  after = Process.times

  assert_kind_of Float, after.cutime
  assert_kind_of Float, after.cstime
  assert_operator after.cutime + after.cstime, :>, before.cutime + before.cstime
end

# Process::Tms is a Struct, so what a Struct answers (#==, #inspect,
# #initialize's arity, and the rest) is mruby-struct's to test, and is not
# repeated here. What is this gem's is that Process::Tms is that Struct, with
# those four members in that order, under that name.
assert('Process::Tms') do
  t = Process::Tms.new(1, 2, 3, 4)
  assert_kind_of Struct, t
  assert_equal [:utime, :stime, :cutime, :cstime], Process::Tms.members
  assert_equal [1, 2, 3, 4], [t.utime, t.stime, t.cutime, t.cstime]
  assert_equal [1, 2, 3, 4], t.to_a
  # Built with plain Integers rather than the Floats Process.times always
  # gives it, on purpose: a Struct stores whatever it is given without asking
  # whether it is a Float, which also makes this read the same under
  # MRB_NO_FLOAT, where a Float literal is not one.
  assert_equal "#<struct Process::Tms utime=1, stime=2, cutime=3, cstime=4>", t.inspect
end

assert('Process::CLOCK_REALTIME and the rest') do
  # Four distinct ids, all defined on every platform, so that a program
  # naming one is naming the same clock wherever it runs.
  clocks = ProcessTestUtil.clocks
  seen = {}
  clocks.each do |id|
    assert_kind_of Integer, id
    seen[id] = true
  end
  assert_equal clocks.size, seen.size
end

assert('Process.clock_gettime') do
  ProcessTestUtil.clocks.each do |id|
    next unless ProcessTestUtil.clock?(id)

    if ProcessTestUtil.float?
      assert_kind_of Float, Process.clock_gettime(id)
    end
    assert_kind_of Integer, Process.clock_gettime(id, :second)
    if ProcessTestUtil.fits?(id, :nanosecond)
      assert_kind_of Integer, Process.clock_gettime(id, :nanosecond)
    end
  end
end

assert('Process.clock_gettime with CLOCK_MONOTONIC') do
  # What a monotonic clock promises is that a later reading is not a smaller
  # one.  Where its origin is, and so what any single reading says, is the
  # platform's to choose and nothing to assert on.
  skip "no monotonic clock on this platform" unless ProcessTestUtil.clock?(Process::CLOCK_MONOTONIC)
  skip "no Integer for a reading in nanoseconds" unless ProcessTestUtil.fits?(Process::CLOCK_MONOTONIC, :nanosecond)

  first = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  second = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  assert_operator second, :>=, first
end

assert('Process.clock_gettime in each unit') do
  # The units are scalings of one reading, so a later reading in a smaller
  # unit is worth at least what an earlier one in a bigger unit was, and the
  # two are no further apart than the moment between them.  Read from the
  # monotonic clock, which cannot step between the two readings.
  skip "no monotonic clock on this platform" unless ProcessTestUtil.clock?(Process::CLOCK_MONOTONIC)
  skip "no Integer for a reading in nanoseconds" unless ProcessTestUtil.fits?(Process::CLOCK_MONOTONIC, :nanosecond)

  sec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :second)
  msec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond)
  usec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :microsecond)
  nsec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

  [sec, msec, usec, nsec].each { |v| assert_kind_of Integer, v }
  # A whole unit is what the reading had reached, never the one it was about
  # to reach, so scaling a bigger unit up never overtakes a later reading.
  assert_operator sec * 1000, :<=, msec
  assert_operator msec * 1000, :<=, usec
  assert_operator usec * 1000, :<=, nsec
  # The four were read moments apart (a scheduler pause on a loaded CI runner
  # costs seconds, not the minute asserted here), while a reading the HAL
  # built wrong, the way a swapped field or the wrong clock would, is off by
  # an amount this catches easily.
  assert_operator nsec - sec * 1_000_000_000, :<, 60 * 1_000_000_000
end

assert('Process.clock_gettime in a float unit') do
  skip "this build has no Float" unless ProcessTestUtil.float?
  skip "no monotonic clock on this platform" unless ProcessTestUtil.clock?(Process::CLOCK_MONOTONIC)

  sec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_second)
  msec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond)
  usec = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_microsecond)

  [sec, msec, usec].each { |v| assert_kind_of Float, v }
  # One reading in three scalings, taken in this order moments apart, so each
  # is worth at least the one before it and the three say the same second.
  # Every unit is scaled and rounded on its own, so comparing two of them
  # rounds a second time, and that alone can read a live pair backwards once
  # the clock's magnitude eats into the mantissa: hence the epsilon.  What a
  # rounding costs is a share of the value rounded, not a fixed amount, so
  # the epsilon is a share of the reading's magnitude too, the origin being
  # the platform's to choose.  An absolute epsilon wide enough for a
  # monotonic clock that has been running a while is either far too wide on
  # a `double` or far too narrow on an `MRB_USE_FLOAT32` build, where the
  # two roundings are worth two milliseconds after a day of uptime.  A
  # millionth is many times the pair of them even there, and far short of
  # what an ordering bug would show.  The minute is the same slack against a
  # scheduler pause on a loaded CI runner that the integer test above uses.
  epsilon = sec.abs * 1.0e-6
  assert_operator msec / 1000, :>=, sec - epsilon
  assert_operator usec / 1000000, :>=, msec / 1000 - epsilon
  assert_operator usec / 1000000 - sec, :<, 60
  # :float_second is what a caller who names no unit gets.
  assert_operator Process.clock_gettime(Process::CLOCK_MONOTONIC), :>=, sec - epsilon
end

assert('Process.clock_gettime with a nil unit') do
  # Naming no unit is what nil says, which is what leaving the argument out
  # says: CRuby cannot tell the two apart at all, an omitted unit arriving
  # there as nil, so neither is answered differently from the other here.
  skip "this build has no Float" unless ProcessTestUtil.float?
  skip "no monotonic clock on this platform" unless ProcessTestUtil.clock?(Process::CLOCK_MONOTONIC)

  assert_kind_of Float, Process.clock_gettime(Process::CLOCK_MONOTONIC, nil)
  assert_kind_of Float, Process.clock_getres(Process::CLOCK_MONOTONIC, nil)
end

assert('Process.clock_gettime in a float unit without a Float') do
  # A build without Float cannot answer in one, and the method is not made
  # to disappear over it: the integer units still answer, and asking for a
  # float one is told so where it is asked.
  skip "this build has a Float" if ProcessTestUtil.float?

  assert_kind_of Integer, Process.clock_gettime(Process::CLOCK_REALTIME, :second)
  [:float_second, :float_millisecond, :float_microsecond].each do |unit|
    assert_raise(NotImplementedError) { Process.clock_gettime(Process::CLOCK_REALTIME, unit) }
  end
  # A resolution in hertz is a Float too, so it goes the same way, and it is
  # still not a unit a reading has, which ArgumentError says first.
  assert_raise(NotImplementedError) { Process.clock_getres(Process::CLOCK_REALTIME, :hertz) }
  assert_raise(ArgumentError) { Process.clock_gettime(Process::CLOCK_REALTIME, :hertz) }
  # Including the one a caller gets by not naming a unit at all, which is the
  # one nil asks for as well.
  assert_raise(NotImplementedError) { Process.clock_gettime(Process::CLOCK_REALTIME) }
  assert_raise(NotImplementedError) { Process.clock_gettime(Process::CLOCK_REALTIME, nil) }
  assert_raise(NotImplementedError) { Process.clock_getres(Process::CLOCK_REALTIME, nil) }
end

assert('Process.clock_gettime with a number naming no clock') do
  # An id outside the list is refused before a port sees it, with the errno
  # a platform's own call gives for a clock it does not have: nothing is
  # wrong with the size of the number, it simply names nothing.
  [-1, ProcessTestUtil.clocks.size, 99].each do |id|
    assert_raise(Errno::EINVAL) { Process.clock_gettime(id) }
    assert_raise(Errno::EINVAL) { Process.clock_getres(id) }
  end
end

assert('Process.clock_gettime with a clock named by a Symbol') do
  # A clock can be named as well as numbered, as it can in CRuby, and the
  # name is the constant's, so a program need not depend on the number.
  {
    CLOCK_REALTIME: Process::CLOCK_REALTIME,
    CLOCK_MONOTONIC: Process::CLOCK_MONOTONIC,
    CLOCK_PROCESS_CPUTIME_ID: Process::CLOCK_PROCESS_CPUTIME_ID,
    CLOCK_THREAD_CPUTIME_ID: Process::CLOCK_THREAD_CPUTIME_ID,
  }.each do |name, id|
    next unless ProcessTestUtil.clock?(id)

    assert_kind_of Integer, Process.clock_gettime(name, :second)
    assert_kind_of Integer, Process.clock_getres(name, :nanosecond)
    assert_equal Process.clock_getres(id, :nanosecond),
                 Process.clock_getres(name, :nanosecond)
  end
end

assert('Process.clock_gettime with a clock_id that names nothing') do
  # CRuby knows further names: the clocks only some platforms have, and the
  # ways it emulates one the host lacks.  A port here either has one of the
  # four or says it has not, so those pick nothing out, and are refused the
  # way a number naming no clock is, which is also what CRuby answers for a
  # name it does not know.
  [:NOPE, :CLOCK_MONOTONIC_RAW, :GETTIMEOFDAY_BASED_CLOCK_REALTIME].each do |name|
    assert_raise(Errno::EINVAL) { Process.clock_gettime(name, :second) }
    assert_raise(Errno::EINVAL) { Process.clock_getres(name, :second) }
  end
  # A String is not a name: it is refused for its type, as CRuby refuses it,
  # rather than read for a clock name it might spell.  nil is refused the
  # same way: naming no clock is not the default a nil unit is.
  assert_raise(TypeError) { Process.clock_gettime("CLOCK_MONOTONIC") }
  assert_raise(TypeError) { Process.clock_gettime(nil) }
end

assert('Process.clock_gettime names the clock it failed on') do
  # The failure says which call was made and which clock it was asked for,
  # the way CRuby says it, so a caller who named a clock is shown the name
  # back rather than a number never written.
  begin
    Process.clock_gettime(:NOPE, :second)
    flunk "no error raised"
  rescue Errno::EINVAL => e
    assert_include e.message, "clock_gettime(:NOPE)"
  end

  begin
    Process.clock_getres(99, :second)
    flunk "no error raised"
  rescue Errno::EINVAL => e
    assert_include e.message, "clock_getres(99)"
  end
end

assert('Process.clock_gettime with a unit it does not know') do
  # A unit is a Symbol, as it is in CRuby, which takes nothing else: a String
  # naming the same thing is not one of the units.
  [:minute, :float_nanosecond, "second", 1].each do |unit|
    assert_raise(ArgumentError) { Process.clock_gettime(Process::CLOCK_REALTIME, unit) }
    assert_raise(ArgumentError) { Process.clock_getres(Process::CLOCK_REALTIME, unit) }
  end
  # :hertz is a resolution's unit alone: there is no rate at which a moment
  # happened.  CRuby refuses it for a reading in the same words.
  assert_raise(ArgumentError) { Process.clock_gettime(Process::CLOCK_REALTIME, :hertz) }
end

assert('Process.clock_gettime with a reading this build cannot carry') do
  # A 32-bit Integer holds a wall clock in seconds and not in nanoseconds.
  # What is wrong with such a reading is its size, so it is refused the way
  # an oversized pid is, unless the build has bigints, which are what CRuby
  # answers with here and are wide enough for any of these clocks.
  skip "this build carries a wall clock in nanoseconds" if ProcessTestUtil.fits?(Process::CLOCK_REALTIME, :nanosecond)

  assert_raise(RangeError) { Process.clock_gettime(Process::CLOCK_REALTIME, :nanosecond) }
  # The same reading in seconds is untouched by it.
  assert_kind_of Integer, Process.clock_gettime(Process::CLOCK_REALTIME, :second)
end

assert('Process.clock_gettime at the ends of what a reading fits in') do
  # A reading becomes an Integer without a clock being read, so where that
  # arithmetic ends can be asked about directly.  It has to be: the first of
  # these is a wall clock in nanoseconds in 2262, and the ones below zero are
  # centuries the other way.  The reading is handed over as a port hands one
  # over, in whole seconds and nanoseconds within one, and the answer is read
  # back as a decimal, these being numbers a build's own Integer may have no
  # way to write.
  #
  # Where the build's Integer holds the answer it is that Integer, whether it
  # took a bigint to hold it or not; where it does not, the reading is
  # refused for its size, as an oversized pid is.
  [
    # int64_t's last value, and the nanosecond after it
    ["9223372036", 854775807, :nanosecond, "9223372036854775807"],
    ["9223372036", 854775808, :nanosecond, "9223372036854775808"],
    # and its first, which falls in a second no whole product of seconds
    # lands on, and the nanosecond before it
    ["-9223372037", 145224192, :nanosecond, "-9223372036854775808"],
    ["-9223372037", 145224191, :nanosecond, "-9223372036854775809"],
    # a second int64_t holds, in a unit whose answer it does not: how far a
    # reading reaches is the platform's business and how far an Integer
    # reaches is mruby's, and the two are not the same question
    ["9223372036854775807", 0, :second, "9223372036854775807"],
    ["9223372036854775807", 0, :millisecond, "9223372036854775807000"],
    ["10000000000", 123456789, :nanosecond, "10000000000123456789"],
    # the second int64_t's own first value falls in, asked for in a unit
    # whose whole seconds land either side of it: the product of that second
    # is itself past int64_t, so a reading there is counted up from INT64_MIN
    # rather than multiplied, and the nanoseconds decide whether it lands
    # back inside.  Without that counting the two below would be refused for
    # a size they have.
    ["-9223372036854776", 200000000, :millisecond, "-9223372036854775800"],
    ["-9223372036854776", 999000000, :millisecond, "-9223372036854775001"],
    # and one in the same second that really is past the end
    ["-9223372036854776", 100000000, :millisecond, "-9223372036854775900"],
    # a reading before the epoch, whose nanoseconds count upwards from the
    # second below it, as a port reports every reading
    ["-2", 500000000, :second, "-2"],
    ["-2", 500000000, :millisecond, "-1500"],
    ["-2", 500000000, :nanosecond, "-1500000000"],
  ].each do |sec, nsec, unit, expected|
    if ProcessClockTest.fits?(expected)
      assert_equal expected, ProcessTestUtil.convert(sec, nsec, unit)
      assert_kind_of Integer, ProcessClockTest.convert(sec, nsec, unit)
    else
      assert_nil ProcessTestUtil.convert(sec, nsec, unit)
      assert_raise(RangeError) { ProcessClockTest.convert(sec, nsec, unit) }
    end
  end
end

assert('Process.clock_getres in hertz') do
  # How many times a second the clock can tell apart, which is one over what
  # :float_second says.  Read back against the same resolution in
  # nanoseconds: a hertz for every nanosecond of it is a second's worth,
  # whatever the clock, and the two are computed apart from each other.
  skip "this build has no Float" unless ProcessTestUtil.float?

  ProcessTestUtil.clocks.each do |id|
    next unless ProcessTestUtil.clock?(id)

    hz = Process.clock_getres(id, :hertz)
    res = Process.clock_getres(id, :nanosecond)
    assert_kind_of Float, hz
    assert_operator hz, :>, 0
    assert_operator (hz * res - 1000000000).abs, :<, 1
  end
end

assert('Process.clock_getres') do
  ProcessTestUtil.clocks.each do |id|
    next unless ProcessTestUtil.clock?(id)

    # A clock a port can read is one it answers a granularity for, so
    # nothing is skipped here for a port declining to say.
    res = Process.clock_getres(id, :nanosecond)
    assert_kind_of Integer, res
    # A resolution is never zero, and no clock here is coarser than a whole
    # second, so it is worth at least a nanosecond and at most one second.
    assert_operator res, :>, 0
    assert_operator res, :<=, 1000000000
    # An integer unit truncates, so a resolution finer than one whole unit
    # of it reads as 0; a clock coarser than a second is the only one that
    # reads above it here.
    assert_operator Process.clock_getres(id, :second), :>=, 0
  end
end

assert('Process.clock_getres of a clock read as a FILETIME') do
  # Windows accounts both CPU clocks in FILETIMEs, and a FILETIME is written
  # in 100ns ticks, so that is how finely two of those readings can differ.
  # The wall clock is left out, being read two different ways depending on
  # the Windows; ports/win/process_hal.c pairs each way with its own
  # granularity.
  skip "not on Windows" unless ProcessTestUtil.windows?

  # A tick is 100ns, so the resolution is asked for in nanoseconds; the
  # reading beside it is asked in an integer unit too, the unit being
  # resolved before the HAL is, so that a build without Float does not raise
  # NotImplementedError before the port is reached at all.
  [Process::CLOCK_PROCESS_CPUTIME_ID, Process::CLOCK_THREAD_CPUTIME_ID].each do |id|
    assert_kind_of Integer, Process.clock_gettime(id, :nanosecond)
    assert_equal 100, Process.clock_getres(id, :nanosecond)
  end
end
