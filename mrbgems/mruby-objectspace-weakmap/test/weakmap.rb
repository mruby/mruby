##
# ObjectSpace::WeakMap and ObjectSpace::WeakKeyMap

# Objects made in a method of their own, so that no register of the test
# holds them once it returns
def weakmap_end_with(s, t)
  s[s.size - t.size, t.size] == t
end

def weakmap_fill(m, n)
  n.times { |i| m["k#{i}"] = "v#{i}" }
  nil
end

def weakmap_fill_values(m, keys)
  keys.each { |k| m[k] = "v#{k}" }
  nil
end

def weakkeymap_fill(m, n)
  n.times { |i| m["k#{i}"] = i }
  nil
end

assert("ObjectSpace::WeakMap") do
  m = ObjectSpace::WeakMap.new
  assert_true m.is_a?(Enumerable)
  k = "a"
  v = "b"
  assert_same v, (m[k] = v)
  assert_same v, m[k]
  # keys are compared by identity
  assert_nil m["a"]
  assert_true m.key?(k)
  assert_true m.include?(k)
  assert_true m.member?(k)
  assert_false m.key?("a")
  assert_equal 1, m.size
  assert_equal 1, m.length

  # an immediate is held as any other object
  m[1] = 2
  m[:s] = nil
  assert_equal 2, m[1]
  assert_true m.key?(:s)
  assert_nil m[:s]
  assert_equal 3, m.size

  m[k] = "c"
  assert_equal "c", m[k]
  assert_equal 3, m.size
end

assert("ObjectSpace::WeakMap#delete") do
  m = ObjectSpace::WeakMap.new
  k = "a"
  m[k] = 1
  assert_equal 1, m.delete(k)
  assert_nil m.delete(k)
  assert_equal [:blk, k], m.delete(k) { |x| [:blk, x] }
  assert_equal 0, m.size
  assert_false m.key?(k)
end

assert("ObjectSpace::WeakMap iteration") do
  m = ObjectSpace::WeakMap.new
  a, b, c = "a", "b", "c"
  m[a] = 1
  m[b] = 2
  m[c] = 3
  m.delete(b)
  m[b] = 4
  # in the order the entries were added
  assert_equal [a, c, b], m.keys
  assert_equal [1, 3, 4], m.values
  pairs = []
  assert_same m, m.each { |k, v| pairs << [k, v] }
  assert_equal [[a, 1], [c, 3], [b, 4]], pairs
  pairs = []
  m.each_pair { |k, v| pairs << [k, v] }
  assert_equal [[a, 1], [c, 3], [b, 4]], pairs
  keys = []
  assert_same m, m.each_key { |k| keys << k }
  assert_equal [a, c, b], keys
  values = []
  assert_same m, m.each_value { |v| values << v }
  assert_equal [1, 3, 4], values
  assert_equal [2, 4, 5], m.map { |k, v| v + 1 }
  assert_raise(LocalJumpError) { m.each }
end

assert("ObjectSpace::WeakMap#inspect") do
  m = ObjectSpace::WeakMap.new
  assert_true m.inspect[0, 25] == "#<ObjectSpace::WeakMap:0x"
  assert_true weakmap_end_with(m.inspect, ">")
  m[1] = :a
  assert_true weakmap_end_with(m.inspect, ": 1 => :a>")
  k = Object.new
  m[k] = 2
  assert_true weakmap_end_with(m.inspect, ": 1 => :a, #{k.to_s} => 2>")
end

assert("ObjectSpace::WeakMap#dup is empty") do
  m = ObjectSpace::WeakMap.new
  k = "a"
  m[k] = 1
  d = m.dup
  assert_equal 0, d.size
  assert_nil d[k]
  d[k] = 2
  assert_equal 1, m[k]
end

assert("ObjectSpace::WeakMap drops what is collected") do
  m = ObjectSpace::WeakMap.new
  k = "keep"
  v = "value"
  m[k] = v
  weakmap_fill(m, 100)
  # a build that collects on each allocation may have collected some already
  GC.start
  assert_true m.size < 101
  assert_same v, m[k]

  # an entry goes when its value is collected too
  m = ObjectSpace::WeakMap.new
  keys = (1..100).map { |i| "k#{i}" }
  weakmap_fill_values(m, keys)
  GC.start
  assert_true m.size < 100
end

assert("ObjectSpace::WeakMap grows and keeps what it holds") do
  m = ObjectSpace::WeakMap.new
  keys = (1..300).map { |i| "k#{i}" }
  keys.each_with_index { |k, i| m[k] = i }
  keys.each_with_index { |k, i| m.delete(k) if i % 2 == 1 }
  GC.start
  keys.each_with_index { |k, i| m[k] = i * 2 if i % 2 == 1 }
  assert_equal 300, m.size
  keys.each_with_index do |k, i|
    assert_equal (i % 2 == 1 ? i * 2 : i), m[k]
  end
end

assert("ObjectSpace::WeakKeyMap") do
  m = ObjectSpace::WeakKeyMap.new
  assert_false m.is_a?(Enumerable)
  k = "a"
  assert_equal 1, (m[k] = 1)
  # keys are compared by eql? and hash
  assert_equal 1, m["a"]
  assert_true m.key?("a")
  assert_same k, m.getkey("a")
  assert_nil m.getkey("b")
  assert_nil m["b"]
  m["a"] = 2
  assert_equal 2, m[k]
  assert_same k, m.getkey("a")

  e = assert_raise(ArgumentError) { m[1] = 2 }
  assert_equal "WeakKeyMap keys must be garbage collectable", e.message
  assert_raise(ArgumentError) { m[:s] = 2 }
  assert_raise(ArgumentError) { m[nil] = 2 }

  assert_equal 2, m.delete("a")
  assert_nil m.delete("a")
  assert_equal [:blk, "a"], m.delete("a") { |x| [:blk, x] }
  m[k] = 1
  assert_same m, m.clear
  assert_false m.key?(k)
end

assert("ObjectSpace::WeakKeyMap#inspect") do
  m = ObjectSpace::WeakKeyMap.new
  k = "a"
  m[k] = 1
  assert_true m.inspect[0, 28] == "#<ObjectSpace::WeakKeyMap:0x"
  assert_true weakmap_end_with(m.inspect, " size=1>")
  assert_true weakmap_end_with(m.dup.inspect, " size=0>")
end

assert("ObjectSpace::WeakKeyMap drops what is collected, and holds values") do
  m = ObjectSpace::WeakKeyMap.new
  k = "keep"
  m[k] = (1..3).map { |i| "v#{i}" }
  weakkeymap_fill(m, 100)
  GC.start
  assert_false weakmap_end_with(m.inspect, " size=101>")
  # the value is held by the map alone, and kept
  assert_equal ["v1", "v2", "v3"], m[k]
end

assert("ObjectSpace::WeakKeyMap keeps values with their keys across a rebuild") do
  m = ObjectSpace::WeakKeyMap.new
  keys = (0...40).map { |i| "k#{i}" }
  keys.each_with_index { |k, i| m[k] = "v#{i}" }
  keys.each_with_index { |k, i| m.delete(k) if i % 2 == 1 }
  extra = (0...40).map { |i| "x#{i}" }
  extra.each { |k| m[k] = "e#{k}" }
  keys.each_with_index do |k, i|
    assert_equal(i % 2 == 0 ? "v#{i}" : nil, m[k])
  end
  extra.each { |k| assert_equal("e#{k}", m[k]) }
end

assert("ObjectSpace.each_object does not reach WeakKeyMap's values") do
  # the map indexes the Array of its values by entry position; one Ruby
  # could shrink would have #[], #delete and the GC go past its end
  m = ObjectSpace::WeakKeyMap.new
  marker = Object.new
  keys = (0...20).map { |i| "k#{i}" }
  keys.each { |k| m[k] = marker }
  found = 0
  ObjectSpace.each_object(Array) do |a|
    next unless a.size >= keys.size && a[0].equal?(marker)
    found += 1
    a.clear
  end
  assert_equal 0, found
  keys.each { |k| assert_same marker, m[k] }
  assert_same marker, m.delete(keys[19])
  GC.start
end

assert("ObjectSpace::WeakMap#each yields each entry when the block grows the map") do
  m = ObjectSpace::WeakMap.new
  keys = (0...8).map { |i| "k#{i}" }
  keys.each { |k| m[k] = k }
  m.delete(keys[1])
  m.delete(keys[3])
  added = (0...12).map { |i| "x#{i}" }
  seen = []
  m.each_key do |k|
    seen << k
    added.each { |x| m[x] = x } if seen.size == 3
  end
  assert_equal([0, 2, 4, 5, 6, 7].map { |i| keys[i] } + added, seen)

  # a block left by break or raise leaves the map to work as before
  m.each { break }
  begin
    m.each { raise "x" }
  rescue
  end
  m.delete(keys[0])
  more = (0...40).map { |i| "y#{i}" }
  more.each { |y| m[y] = y }
  assert_equal([2, 4, 5, 6, 7].map { |i| keys[i] } + added + more, m.keys)
end

assert("ObjectSpace::WeakMap and WeakKeyMap take entries when frozen") do
  [ObjectSpace::WeakMap, ObjectSpace::WeakKeyMap].each do |c|
    m = c.new.freeze
    keys = (0...20).map { |i| "k#{i}" }
    keys.each { |k| m[k] = k }
    keys.each { |k| assert_equal(k, m[k]) }
  end
end

assert("symbol GC keeps the Symbols a map holds") do
  stress
  # a Symbol is never dropped from a map, which may be its only holder
  wm = ObjectSpace::WeakMap.new
  wm["gc-weakmap-key-#{1}".to_sym] = 1
  wm[2] = "gc-weakmap-value-#{1}".to_sym
  wk = ObjectSpace::WeakKeyMap.new
  key = "k"
  wk[key] = "gc-weakkeymap-value-#{1}".to_sym

  6000.times { |i| "gc-filler-weakmap-#{i}".to_sym }
  GC.start

  assert_equal ["gc-weakmap-key-1"], wm.keys.select { |k| k.is_a?(Symbol) }.map(&:to_s)
  assert_equal "gc-weakmap-value-1", wm[2].to_s
  assert_equal "gc-weakkeymap-value-1", wk[key].to_s
end
