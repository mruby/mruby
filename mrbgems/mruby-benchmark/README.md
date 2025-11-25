# mruby-benchmark

Benchmarking and profiling tools for mruby.

## Overview

The `mruby-benchmark` gem provides simple and lightweight benchmarking capabilities for measuring execution time and memory usage in mruby applications. It is designed for embedded systems and resource-constrained environments.

## Installation

Add the following line to your `build_config.rb`:

```ruby
conf.gem :core => 'mruby-benchmark'
```

## API

### Benchmark Module

The main interface for benchmarking operations.

#### `Benchmark.measure { block }` → Benchmark::Tms

Measures the execution time of the given block and returns a `Benchmark::Tms` object containing timing information.

```ruby
result = Benchmark.measure do
  # code to benchmark
  1000.times { "string interpolation: #{42}" }
end

puts result  # Prints formatted timing information
```

#### `Benchmark.realtime { block }` → Float

Returns only the real (wall-clock) time in seconds as a floating-point number.

```ruby
time = Benchmark.realtime do
  sleep(0.1)
end

puts "Took #{time} seconds"  # => "Took 0.100... seconds"
```

#### `Benchmark.bm(label_width = 0) { |x| ... }`

Performs formatted benchmark comparisons with aligned output.

```ruby
Benchmark.bm(10) do |x|
  x.report("array:") { 1000.times { [1, 2, 3, 4, 5] } }
  x.report("hash:") { 1000.times { {a: 1, b: 2, c: 3} } }
  x.report("string:") { 1000.times { "hello" * 100 } }
end
```

Output example:

```
                user     system      total        real
array:      0.010000   0.000000   0.010000 (  0.012345)
hash:       0.015000   0.000000   0.015000 (  0.016789)
string:     0.008000   0.000000   0.008000 (  0.009012)
```

#### `Benchmark.measure(memory: true) { block }` → Benchmark::Tms

Measures both execution time and memory allocation when `memory: true` is specified.

```ruby
result = Benchmark.measure(memory: true) do
  array = []
  1000.times { |i| array << i }
end

puts "Objects allocated: #{result.objects}"
puts "Memory used: #{result.memory} bytes"
```

### Benchmark::Tms Class

Holds timing measurement results. Provides methods to access individual timing components.

#### Attributes

- `utime` - User CPU time in seconds (Float)
- `stime` - System CPU time in seconds (Float)
- `cutime` - User CPU time of child processes (Float, usually 0 in mruby)
- `cstime` - System CPU time of child processes (Float, usually 0 in mruby)
- `real` - Real (wall-clock) time in seconds (Float)
- `objects` - Number of objects allocated (Integer, when memory tracking enabled)
- `memory` - Memory allocated in bytes (Integer, when memory tracking enabled)

#### Methods

##### `total` → Float

Returns the total CPU time (user + system).

```ruby
result = Benchmark.measure { heavy_computation }
puts "Total CPU time: #{result.total} seconds"
```

##### `to_s` → String

Returns formatted string representation of timing results.

```ruby
result = Benchmark.measure { sleep(0.1) }
puts result.to_s
# => "  0.000000   0.000000   0.000000 (  0.100123)"
```

##### `format(format_str)` → String

Returns timing results formatted according to the format string.

Format specifiers:

- `%u` - User CPU time
- `%s` - System CPU time
- `%t` - Total CPU time
- `%r` - Real time
- `%o` - Objects allocated (if memory tracking enabled)
- `%m` - Memory allocated (if memory tracking enabled)
- `%n` - Label name

```ruby
result = Benchmark.measure { computation }
puts result.format("Real: %rs, CPU: %ts")
# => "Real: 0.123s, CPU: 0.100s"
```

### Benchmark::Report Class

Used within `Benchmark.bm` for formatted reporting.

#### `report(label = "") { block }`

Executes and reports on a single benchmark within a `bm` block.

```ruby
Benchmark.bm do |x|
  x.report("first test") { code1 }
  x.report("second test") { code2 }
end
```

## Usage Examples

### Basic Timing

```ruby
require 'benchmark'

# Simple timing
time = Benchmark.realtime do
  sum = 0
  1000000.times { |i| sum += i }
end
puts "Calculation took #{time} seconds"

# Detailed timing
result = Benchmark.measure do
  arr = (1..10000).to_a
  arr.sort!
end
puts result
```

### Comparing Implementations

```ruby
require 'benchmark'

Benchmark.bm(15) do |x|
  x.report("Array#each:") do
    arr = (1..1000).to_a
    sum = 0
    arr.each { |n| sum += n }
  end

  x.report("Array#inject:") do
    arr = (1..1000).to_a
    arr.inject(0) { |sum, n| sum + n }
  end

  x.report("Numeric#times:") do
    sum = 0
    1000.times { |n| sum += n }
  end
end
```

### Memory Profiling

```ruby
require 'benchmark'

# Track memory allocation
result = Benchmark.measure(memory: true) do
  strings = []
  1000.times { |i| strings << "string_#{i}" }
end

puts "Execution time: #{result.real}s"
puts "Objects created: #{result.objects}"
puts "Memory allocated: #{result.memory} bytes"
```

### Performance Testing in Tests

```ruby
# In test files
assert('String concatenation performance') do
  time = Benchmark.realtime do
    1000.times { "hello" + "world" }
  end

  # Assert it completes within reasonable time
  assert_true time < 0.1, "String concat should be fast"
end
```

## Implementation Notes

### Time Measurement

mruby-benchmark uses `Process.clock_gettime` (via mruby-time) for high-resolution timing when available. User and system CPU times are measured using platform-specific APIs where available, otherwise both are set to 0.

### Memory Tracking

Memory profiling uses `ObjectSpace.count_objects` (via mruby-objectspace) to track object allocation. Memory size estimation is based on typical object overhead and may not be exact for all platforms.

### Limitations

- Child process timing (`cutime`, `cstime`) is not supported in most mruby environments and always returns 0
- System CPU time may not be available on all platforms
- Memory measurements are estimates and may not reflect actual heap usage
- GC activity during benchmarking may affect timing results

## Dependencies

- **mruby-time** - Required for timing measurements
- **mruby-objectspace** - Required for memory profiling

## License

MIT License

## Authors

mruby developers
