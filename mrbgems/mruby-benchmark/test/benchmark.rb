##
# Benchmark Test

assert('Benchmark') do
  assert_equal(Module, Benchmark.class)
end

assert('Benchmark::Tms') do
  assert_equal(Class, Benchmark::Tms.class)
end

assert('Benchmark::Report') do
  assert_equal(Class, Benchmark::Report.class)
end

assert('Benchmark.measure') do
  result = Benchmark.measure do
    sum = 0
    100.times { |i| sum += i }
  end

  assert_kind_of(Benchmark::Tms, result)
  assert_kind_of(Float, result.utime)
  assert_kind_of(Float, result.stime)
  assert_kind_of(Float, result.cutime)
  assert_kind_of(Float, result.cstime)
  assert_kind_of(Float, result.real)

  # For mruby, CPU times are typically 0
  assert_equal(0.0, result.utime)
  assert_equal(0.0, result.stime)
  assert_equal(0.0, result.cutime)
  assert_equal(0.0, result.cstime)

  # Real time should be positive
  assert_true(result.real >= 0)
end

assert('Benchmark.measure with actual delay') do
  result = Benchmark.measure do
    # Create some objects to ensure measurable time
    arr = []
    1000.times { |i| arr << i }
  end

  # Real time should be measurable (greater than 0)
  assert_true(result.real > 0)
end

assert('Benchmark.realtime') do
  time = Benchmark.realtime do
    sum = 0
    100.times { |i| sum += i }
  end

  assert_kind_of(Float, time)
  assert_true(time >= 0)
end

assert('Benchmark.realtime with actual delay') do
  time = Benchmark.realtime do
    arr = []
    1000.times { |i| arr << i }
  end

  assert_true(time > 0)
end

assert('Benchmark::Tms#total') do
  tms = Benchmark::Tms.new(1.0, 2.0, 3.0, 4.0, 5.0)
  assert_equal(10.0, tms.total)
end

assert('Benchmark::Tms#to_s') do
  tms = Benchmark::Tms.new(1.234567, 2.345678, 3.456789, 4.567890, 5.678901)
  str = tms.to_s

  assert_kind_of(String, str)
  # Should contain formatted numbers
  assert_true(str.include?('1.234567'))
  assert_true(str.include?('2.345678'))
end

assert('Benchmark::Tms#format') do
  tms = Benchmark::Tms.new(1.5, 2.5, 0.0, 0.0, 10.0)

  result = tms.format("user: %u, system: %s, total: %t, real: %r")
  assert_equal("user: 1.5, system: 2.5, total: 4.0, real: 10.0", result)
end

assert('Benchmark::Tms attributes') do
  tms = Benchmark::Tms.new(1.0, 2.0, 3.0, 4.0, 5.0)

  assert_equal(1.0, tms.utime)
  assert_equal(2.0, tms.stime)
  assert_equal(3.0, tms.cutime)
  assert_equal(4.0, tms.cstime)
  assert_equal(5.0, tms.real)
end

assert('Benchmark.bm') do
  # Suppress output during test
  old_stdout = $stdout
  $stdout = nil

  results = []

  report = Benchmark.bm(10) do |x|
    results << x.report("test1") { 100.times { 1 + 1 } }
    results << x.report("test2") { 100.times { 2 * 2 } }
  end

  $stdout = old_stdout

  assert_kind_of(Benchmark::Report, report)
  assert_equal(2, results.length)
  assert_kind_of(Benchmark::Tms, results[0])
  assert_kind_of(Benchmark::Tms, results[1])

  # Verify results were stored in report
  assert_equal(2, report.results.length)
end

assert('Benchmark.bm without label width') do
  # Suppress output during test
  old_stdout = $stdout
  $stdout = nil

  report = Benchmark.bm do |x|
    x.report { 50.times { 1 + 1 } }
  end

  $stdout = old_stdout

  assert_kind_of(Benchmark::Report, report)
  assert_equal(1, report.results.length)
end

assert('Benchmark::Report#report') do
  # Suppress output during test
  old_stdout = $stdout
  $stdout = nil

  report = Benchmark::Report.new(5)
  result = report.report("test") { 100.times { 1 + 1 } }

  $stdout = old_stdout

  assert_kind_of(Benchmark::Tms, result)
  assert_equal(1, report.results.length)
end

assert('Benchmark.measure with memory tracking') do
  skip unless Object.const_defined?(:ObjectSpace)

  result = Benchmark.measure(memory: true) do
    arr = []
    # Create actual objects (strings) that will be tracked
    100.times { |i| arr << "string_#{i}" }
  end

  assert_kind_of(Benchmark::Tms, result)
  assert_kind_of(Integer, result.objects)
  assert_kind_of(Integer, result.memory)

  # Memory tracking is best-effort, just verify the attributes exist
  # and have reasonable values (non-negative)
  assert_true(result.objects >= 0)
  assert_true(result.memory >= 0)
end

assert('Benchmark.measure without memory tracking') do
  result = Benchmark.measure(memory: false) do
    arr = []
    100.times { |i| arr << i }
  end

  assert_kind_of(Benchmark::Tms, result)
  assert_nil(result.objects)
  assert_nil(result.memory)
end

assert('Benchmark::Tms with memory attributes') do
  tms = Benchmark::Tms.new(1.0, 2.0, 3.0, 4.0, 5.0, "label", 1000, 40000)

  assert_equal(1000, tms.objects)
  assert_equal(40000, tms.memory)
end

assert('Benchmark::Tms#format with memory') do
  tms = Benchmark::Tms.new(1.0, 2.0, 0.0, 0.0, 5.0, "test", 100, 4000)

  result = tms.format("Objects: %o, Memory: %m bytes, Label: %n")
  assert_equal("Objects: 100, Memory: 4000 bytes, Label: test", result)
end

assert('Benchmark comparison example') do
  # Suppress output during test
  old_stdout = $stdout
  $stdout = nil

  results = []

  Benchmark.bm(15) do |x|
    results << x.report("array creation") do
      100.times { [1, 2, 3, 4, 5] }
    end

    results << x.report("hash creation") do
      100.times { {a: 1, b: 2, c: 3} }
    end
  end

  $stdout = old_stdout

  assert_equal(2, results.length)
  # Both should have measurable real time
  assert_true(results[0].real >= 0)
  assert_true(results[1].real >= 0)
end

assert('Benchmark.measure consistency') do
  # Measure the same operation twice
  result1 = Benchmark.measure { 500.times { 1 + 1 } }
  result2 = Benchmark.measure { 500.times { 1 + 1 } }

  # Both should return valid Tms objects
  assert_kind_of(Benchmark::Tms, result1)
  assert_kind_of(Benchmark::Tms, result2)

  # Real times should be positive
  assert_true(result1.real >= 0)
  assert_true(result2.real >= 0)
end

assert('Benchmark.realtime consistency') do
  time1 = Benchmark.realtime { 500.times { 1 + 1 } }
  time2 = Benchmark.realtime { 500.times { 1 + 1 } }

  assert_kind_of(Float, time1)
  assert_kind_of(Float, time2)
  assert_true(time1 >= 0)
  assert_true(time2 >= 0)
end
