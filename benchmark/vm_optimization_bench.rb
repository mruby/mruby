# VM Optimization Benchmarks for mruby
# Usage: ./bin/mruby benchmark/vm_optimization_bench.rb
#
# Each benchmark is designed to isolate specific VM behaviors:
# - Dispatch overhead
# - Arithmetic operations
# - Method calls
# - Array/Hash access
# - Loop performance

# Benchmark infrastructure
def measure(name, iterations = 1)
  # Warm up
  3.times { yield }

  # Force GC before measurement
  GC.start

  t0 = Time.now
  iterations.times { yield }
  elapsed = Time.now - t0

  puts "#{name}: #{elapsed * 1000 / iterations} ms"
  elapsed
end

N = 1_000_000
M = 100_000

puts "=" * 60
puts "mruby VM Optimization Benchmarks"
puts "=" * 60
puts

#=============================================================================
# 1. DISPATCH OVERHEAD BENCHMARKS
# Target: Tail-call threading, computed goto efficiency
#=============================================================================
puts "--- Dispatch Overhead ---"

# 1a. Empty loop (pure dispatch cost)
measure("empty_loop", 10) do
  i = 0
  while i < N
    i += 1
  end
end

# 1b. NOP-heavy (many instructions, minimal work)
measure("nop_sequence", 10) do
  i = 0
  while i < M
    a = 1; b = 2; c = 3; d = 4; e = 5
    a = 1; b = 2; c = 3; d = 4; e = 5
    a = 1; b = 2; c = 3; d = 4; e = 5
    a = 1; b = 2; c = 3; d = 4; e = 5
    i += 1
  end
end

#=============================================================================
# 2. ARITHMETIC BENCHMARKS
# Target: Type specialization, register variables, ADDI fusion
#=============================================================================
puts
puts "--- Arithmetic Operations ---"

# 2a. Integer addition (tests OP_ADD fast path)
measure("int_add", 10) do
  x = 0
  i = 0
  while i < N
    x = x + 1
    i += 1
  end
  x
end

# 2b. Integer increment (tests potential OP_INCI fusion)
measure("int_increment", 10) do
  x = 0
  i = 0
  while i < N
    x += 1
    i += 1
  end
  x
end

# 2c. Mixed arithmetic (tests type checking overhead)
measure("mixed_arith", 10) do
  x = 0
  y = 1.5
  i = 0
  while i < M
    x = x + 1
    y = y + 0.5
    i += 1
  end
  x
end

# 2d. Comparison in loop (tests OP_LT + JMPNOT fusion potential)
measure("comparison_loop", 10) do
  x = 0
  while x < N
    x += 1
  end
  x
end

# 2e. Multiple comparisons (branch prediction)
measure("multi_compare", 10) do
  i = 0
  count = 0
  while i < M
    count += 1 if i > 100
    count += 1 if i < 50000
    count += 1 if i == 25000
    i += 1
  end
  count
end

#=============================================================================
# 3. METHOD CALL BENCHMARKS
# Target: Inline caching, method dispatch optimization
#=============================================================================
puts
puts "--- Method Calls ---"

class BenchClass
  def empty_method
  end

  def simple_add(a, b)
    a + b
  end

  def self.class_method
  end
end

$obj = BenchClass.new

# 3a. Empty method call (pure dispatch overhead)
measure("empty_method_call", 10) do
  obj = $obj
  i = 0
  while i < M
    obj.empty_method
    i += 1
  end
end

# 3b. Method with arguments
measure("method_with_args", 10) do
  obj = $obj
  i = 0
  while i < M
    obj.simple_add(1, 2)
    i += 1
  end
end

# 3c. Self method call (tests OP_SENDSELF potential)
class SelfCallBench
  def run
    i = 0
    while i < M
      helper
      i += 1
    end
  end

  def helper
  end
end

measure("self_method_call", 10) do
  SelfCallBench.new.run
end

# 3d. Polymorphic call site (tests inline cache invalidation)
class Duck1
  def quack; 1; end
end
class Duck2
  def quack; 2; end
end

$duck1 = Duck1.new
$duck2 = Duck2.new

measure("polymorphic_call", 10) do
  d1, d2 = $duck1, $duck2
  i = 0
  sum = 0
  while i < M
    sum += d1.quack
    sum += d2.quack
    i += 1
  end
  sum
end

#=============================================================================
# 4. ARRAY/HASH BENCHMARKS
# Target: GETIDX/SETIDX fast path, bounds checking
#=============================================================================
puts
puts "--- Array/Hash Access ---"

$ary = Array.new(1000) { |i| i }
$hash = {}
1000.times { |i| $hash[i] = i }

# 4a. Array read (sequential)
measure("array_read_seq", 10) do
  ary = $ary
  i = 0
  sum = 0
  while i < M
    sum += ary[i % 1000]
    i += 1
  end
  sum
end

# 4b. Array read (constant index - tests constant propagation)
measure("array_read_const", 10) do
  ary = $ary
  i = 0
  sum = 0
  while i < M
    sum += ary[500]
    i += 1
  end
  sum
end

# 4c. Array write
measure("array_write", 10) do
  ary = Array.new(1000, 0)
  i = 0
  while i < M
    ary[i % 1000] = i
    i += 1
  end
end

# 4d. Hash read
measure("hash_read", 10) do
  h = $hash
  i = 0
  sum = 0
  while i < M
    sum += h[i % 1000]
    i += 1
  end
  sum
end

#=============================================================================
# 5. LOOP PATTERN BENCHMARKS
# Target: Loop optimization, branch prediction
#=============================================================================
puts
puts "--- Loop Patterns ---"

# 5a. Simple while loop
measure("while_loop", 10) do
  i = 0
  while i < N
    i += 1
  end
end

# 5b. times iterator (block overhead)
measure("times_iterator", 10) do
  sum = 0
  M.times do |i|
    sum += i
  end
  sum
end

# 5c. each iterator on array
$small_ary = (0...1000).to_a
measure("each_iterator", 10) do
  ary = $small_ary
  total = 0
  1000.times do
    ary.each { |x| total += x }
  end
  total
end

# 5d. Nested loops
measure("nested_loop", 10) do
  sum = 0
  i = 0
  while i < 1000
    j = 0
    while j < 1000
      sum += 1
      j += 1
    end
    i += 1
  end
  sum
end

#=============================================================================
# 6. CONSTANT LOADING BENCHMARKS
# Target: Constant pre-computation, pool access
#=============================================================================
puts
puts "--- Constant Loading ---"

# 6a. Integer literals (tests LOADI optimization)
measure("int_literals", 10) do
  i = 0
  sum = 0
  while i < M
    sum += 1
    sum += 2
    sum += 3
    sum += 42
    sum += 100
    i += 1
  end
  sum
end

# 6b. Large integer literals (tests LOADL from pool)
measure("large_int_literals", 10) do
  i = 0
  sum = 0
  while i < M
    sum += 1000000
    sum += 2000000
    sum += 3000000
    i += 1
  end
  sum
end

# 6c. Float literals
measure("float_literals", 10) do
  i = 0
  sum = 0.0
  while i < M
    sum += 1.5
    sum += 2.5
    sum += 3.5
    i += 1
  end
  sum
end

# 6d. String literals (allocation vs interning)
measure("string_literals", 5) do
  i = 0
  while i < 100000
    s = "hello"
    s = "world"
    s = "test"
    i += 1
  end
end

#=============================================================================
# 7. BRANCH PREDICTION BENCHMARKS
# Target: mrb_likely/mrb_unlikely effectiveness
#=============================================================================
puts
puts "--- Branch Prediction ---"

# 7a. Predictable branch (always true)
measure("predictable_true", 10) do
  i = 0
  count = 0
  while i < N
    count += 1 if true
    i += 1
  end
  count
end

# 7b. Predictable branch (always false)
measure("predictable_false", 10) do
  i = 0
  count = 0
  while i < N
    count += 1 if false
    i += 1
  end
  count
end

# 7c. Unpredictable branch (50/50)
measure("unpredictable_50", 10) do
  i = 0
  count = 0
  while i < M
    count += 1 if i & 1 == 0
    i += 1
  end
  count
end

# 7d. Rare branch (error path simulation)
measure("rare_branch", 10) do
  i = 0
  count = 0
  while i < N
    count += 1 if i == -1  # Never true
    i += 1
  end
  count
end

#=============================================================================
# 8. REGISTER PRESSURE BENCHMARKS
# Target: Register variable optimization
#=============================================================================
puts
puts "--- Register Pressure ---"

# 8a. Few local variables (should fit in registers)
measure("few_locals", 10) do
  i = 0
  a = 0
  while i < N
    a += 1
    i += 1
  end
  a
end

# 8b. Many local variables (register spilling)
measure("many_locals", 10) do
  i = 0
  a = 0; b = 0; c = 0; d = 0; e = 0
  f = 0; g = 0; h = 0; j = 0; k = 0
  l = 0; m = 0; n = 0; o = 0; p = 0
  while i < M
    a += 1; b += 1; c += 1; d += 1; e += 1
    f += 1; g += 1; h += 1; j += 1; k += 1
    l += 1; m += 1; n += 1; o += 1; p += 1
    i += 1
  end
  a + b + c + d + e + f + g + h + j + k + l + m + n + o + p
end

#=============================================================================
# 9. COMPOSITE BENCHMARKS (Real-world-ish)
#=============================================================================
puts
puts "--- Composite Benchmarks ---"

# 9a. Fibonacci (recursion + arithmetic)
def fib(n)
  return n if n < 2
  fib(n - 1) + fib(n - 2)
end

measure("fibonacci_30", 3) do
  fib(30)
end

# 9b. Tak function (heavy recursion)
def tak(x, y, z)
  if y < x
    tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
  else
    z
  end
end

measure("tak_18_12_6", 3) do
  tak(18, 12, 6)
end

# 9c. Array manipulation
measure("array_manipulation", 5) do
  ary = []
  10000.times { |i| ary << i }
  ary.map! { |x| x * 2 }
  ary.select { |x| x % 3 == 0 }.size
end

# 9d. String operations
measure("string_ops", 5) do
  s = ""
  10000.times { |i| s = s + i.to_s }
  s.size
end

# 9e. Hash operations
measure("hash_ops", 5) do
  h = {}
  50000.times { |i| h[i.to_s] = i }
  sum = 0
  h.each { |k, v| sum += v }
  sum
end

puts
puts "=" * 60
puts "Benchmark complete"
puts "=" * 60
