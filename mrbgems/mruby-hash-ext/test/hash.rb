##
# Hash(Ext) Test

assert('Hash.[] Hash') do
  a = Hash['a_key' => 'a_value']

  assert_equal({'a_key' => 'a_value'}, a)
end

assert('Hash.[] [ [ ["b_key", "b_value" ] ] ]') do
  a = Hash[ [ ['b_key', 'b_value'] ] ]

  assert_equal({'b_key' => 'b_value'}, a)

  a = Hash[ [ ] ]

  assert_equal({}, a)

  assert_raise(ArgumentError) do
    Hash[ [ ['b_key', 'b_value', 'b_over'] ] ]
  end

  assert_raise(ArgumentError) do
    Hash[ [ [] ] ]
  end
end

assert('Hash.[] "c_key", "c_value"') do
  a = Hash['c_key', 'c_value', 'd_key', 1]

  assert_equal({'c_key' => 'c_value', 'd_key' => 1}, a)

  a = Hash[]

  assert_equal({}, a)

  assert_raise(ArgumentError) do
    Hash['d_key']
  end
end

assert('Hash.[] for sub class') do
  sub_hash_class = Class.new(Hash)
  sub_hash = sub_hash_class[]
  assert_equal(sub_hash_class, sub_hash.class)
end

assert('Hash#merge!') do
  # Single hash merge
  a = { 'abc_key' => 'abc_value', 'cba_key' => 'cba_value' }
  b = { 'cba_key' => 'XXX', 'xyz_key' => 'xyz_value' }
  result_1 = a.merge! b
  assert_equal({'abc_key' => 'abc_value', 'cba_key' => 'XXX',
               'xyz_key' => 'xyz_value' }, result_1)

  # Block handling
  a = { 'abc_key' => 'abc_value', 'cba_key' => 'cba_value' }
  result_2 = a.merge!(b) do |key, original, new|
    original
  end
  assert_equal({'abc_key' => 'abc_value', 'cba_key' => 'cba_value',
               'xyz_key' => 'xyz_value' }, result_2)

  # Multiple arguments
  assert_equal({a:1,b:2,c:3}, {a:1}.merge!({b:2},{c:3}))

  # Error cases
  assert_raise(ArgumentError) { {}.merge!() }
  assert_raise(TypeError) { {}.merge!("not a hash") }
end

assert('Hash#values_at') do
  h = { "cat" => "feline", "dog" => "canine", "cow" => "bovine" }
  assert_equal ["bovine", "feline"], h.values_at("cow", "cat")

  keys = []
  (0...1000).each { |v| keys.push "#{v}" }
  h = Hash.new { |hash,k| hash[k] = k }
  assert_equal keys, h.values_at(*keys)
end

assert('Hash#compact') do
  h = { "cat" => "feline", "dog" => nil, "cow" => false }

  assert_equal({ "cat" => "feline", "cow" => false }, h.compact)
  assert_equal({ "cat" => "feline", "dog" => nil, "cow" => false }, h)
end

assert('Hash#compact!') do
  h = { "cat" => "feline", "dog" => nil, "cow" => false }

  assert_equal({ "cat" => "feline", "cow" => false }, h.compact!)
  assert_nil(h.compact!)
end

assert('Hash#fetch') do
  h = { "cat" => "feline", "dog" => "canine", "cow" => "bovine" }
  assert_equal "feline", h.fetch("cat")
  assert_equal "mickey", h.fetch("mouse", "mickey")
  assert_equal "minny", h.fetch("mouse"){"minny"}
  assert_equal "mouse", h.fetch("mouse"){|k| k}
  assert_raise(KeyError) do
    h.fetch("gnu")
  end
end

assert("Hash - a frozen receiver of a call that writes nothing") do
  # Each of these leaves the hash as it was and used to return before the
  # write that carries the frozen check.
  assert_raise(FrozenError) { {a: 1}.freeze.merge!({}) { |*x| x } }
  assert_raise(FrozenError) { {}.freeze.delete_if { true } }
  assert_raise(FrozenError) { {}.freeze.keep_if { true } }
  assert_raise(FrozenError) { {a: 1}.freeze.delete_if { false } }
  assert_raise(FrozenError) { {a: 1}.freeze.keep_if { true } }
  assert_raise(FrozenError) { {}.freeze.transform_values! { |v| v } }
end

assert("Hash#delete_if") do
  base = { 1 => 'one', 2 => false, true => 'true', 'cat' => 99 }
  h1   = { 1 => 'one', 2 => false, true => 'true' }
  h2   = { 2 => false, 'cat' => 99 }
  h3   = { 2 => false }

  h = base.dup
  assert_equal(h, h.delete_if { false })
  assert_equal({}, h.delete_if { true })

  h = base.dup
  assert_equal(h1, h.delete_if {|k,v| k.instance_of?(String) })
  assert_equal(h1, h)

  h = base.dup
  assert_equal(h2, h.delete_if {|k,v| v.instance_of?(String) })
  assert_equal(h2, h)

  h = base.dup
  assert_equal(h3, h.delete_if {|k,v| v })
  assert_equal(h3, h)

  h = base.dup
  n = 0
  h.delete_if {|*a|
    n += 1
    assert_equal(2, a.size)
    assert_equal(base[a[0]], a[1])
    h.shift
    true
  }
  assert_equal(base.size, n)
end

assert("Hash#flatten") do
  a =  {1=> "one", 2 => [2,"two"], 3 => [3, ["three"]]}
  assert_equal [1, "one", 2, [2, "two"], 3, [3, ["three"]]], a.flatten
  assert_equal [[1, "one"], [2, [2, "two"]], [3, [3, ["three"]]]], a.flatten(0)
  assert_equal [1, "one", 2, [2, "two"], 3, [3, ["three"]]], a.flatten(1)
  assert_equal [1, "one", 2, 2, "two", 3, 3, ["three"]], a.flatten(2)
  assert_equal [1, "one", 2, 2, "two", 3, 3, "three"], a.flatten(3)
end

assert("Hash#invert") do
  h = { 1 => 'one', 2 => 'two', 3 => 'three',
        true => 'true', nil => 'nil' }.invert
  assert_equal 1, h['one']
  assert_equal true, h['true']
  assert_equal nil, h['nil']

  h = { 'a' => 1, 'b' => 2, 'c' => 1 }.invert
  assert_equal(2, h.length)
  assert_include(%w[a c], h[1])
  assert_equal('b', h[2])
end

assert("Hash#invert with sub class") do
  sub_hash_class = Class.new(Hash)
  sub_hash = sub_hash_class.new
  assert_equal(sub_hash_class, sub_hash.invert.class)
end

assert("Hash#keep_if") do
  h = { 1 => 2, 3 => 4, 5 => 6 }
  assert_equal({3=>4,5=>6}, h.keep_if {|k, v| k + v >= 7 })
  h = { 1 => 2, 3 => 4, 5 => 6 }
  assert_equal({ 1 => 2, 3=> 4, 5 =>6} , h.keep_if { true })
end

assert("Hash#key") do
  h = { "a" => 100, "b" => 200, "c" => 300, "d" => 300, nil => 'nil', 'nil' => nil }
  assert_equal "b", h.key(200)
  assert_equal "c", h.key(300)
  assert_nil h.key(999)
  assert_nil h.key('nil')
  assert_equal 'nil', h.key(nil)
end

assert("Hash#key keeps the key it answers with across the comparison") do
  # The scan hands the value to `==`, and the Ruby there can delete the pair it
  # is standing on. The key the scan would answer with is then held by the scan
  # in C, which the collector does not scan, so the collection the same Ruby
  # sets off takes it.
  thief = Class.new do
    def initialize(h, k) @h, @k = h, k end
    def ==(other)
      if @h
        h, k, @h, @k = @h, @k, nil, nil
        h.delete(k)
        h[:added] = :added_value
        GC.start
      end
      true
    end
    # Built here and not in the assertion so that returning drops the stored
    # key from the arena: what holds it from then on is the entry alone. An
    # unfrozen String key is stored as a frozen copy, which is the object the
    # entry holds and the delete below takes away.
    def self.armed_hash
      h = {}
      20.times { |i| h[i] = i }
      k = "the key only that entry holds"
      h[k] = new(h, k)
      h
    end
  end

  assert_equal("the key only that entry holds", thief.armed_hash.key(:target))
end

assert("Hash#slice keeps the value it carries across the set") do
  # Reading the pair out asks the key for its hash code, and storing it in the
  # result asks a second time. The Ruby answering that second ask can delete
  # the pair out of the receiver, and what holds the value then is the C local
  # carrying it, which the collector does not scan.
  thief = Class.new do
    attr_reader :fired
    def initialize(h) @h, @asks = h, 0 end
    def arm(n) @fire_at, @asks, @fired = n, 0, false end
    def hash
      @asks += 1
      if @asks == @fire_at
        @fired = true
        h, @h = @h, nil
        h.delete(self)
        h[:added] = :added_value
        GC.start
      end
      42
    end
    def eql?(other) equal?(other) end
    def self.armed_hash
      h = {}
      20.times { |i| h[i] = i }
      k = new(h)
      h[k] = "the value only that entry holds"
      [h, k]
    end
  end

  h, k = thief.armed_hash
  # A result asks for a hash code only once it has an index of its own, which
  # is what the keys ahead of this one are for. The second ask is the set, so
  # having fired says the value below is answered from the path being covered.
  keys = (0...17).to_a
  keys.push(k)
  k.arm(2)
  sliced = h.slice(*keys)
  assert_true(k.fired)
  assert_equal("the value only that entry holds", sliced[k])
end

assert("Hash#slice! keeps the value it removed across the set") do
  # The delete takes the pair out of the receiver, and the set that files it
  # under the same key asks that key for its hash code. Anything the Ruby
  # answering that allocates can collect the value on the way, since what
  # holds it between the two calls is the C local carrying it.
  collector = Class.new do
    attr_reader :asks
    def initialize; @asks = 0 end
    def arm; @asks = 0 end
    def hash
      @asks += 1
      GC.start
      7
    end
    def eql?(other) equal?(other) end
    def self.armed_hash
      h = {}
      20.times { |i| h[i] = i }
      k = new
      h[k] = "the value only that entry holds"
      k.arm
      [h, k]
    end
  end

  h, k = collector.armed_hash
  removed = h.slice!(0)
  # Two asks and no more: the delete and the set into the result. The scan for
  # the keys to keep asks nothing, since one key to keep is a hash in list
  # shape, which compares with `eql?` alone. A result too small to be indexed
  # would not ask either, and the value below would be answered from a walk
  # that never ran Ruby.
  assert_equal(2, k.asks)
  assert_equal("the value only that entry holds", removed[k])
end

assert("Hash#to_h") do
  h = { "a" => 100, "b" => 200 }
  assert_equal Hash, h.to_h.class
  assert_equal h, h.to_h
end

assert("Hash#to_h with a block") do
  h = { "a" => 100, "b" => 200 }
  assert_equal({ 100 => "a", 200 => "b" }, h.to_h { |k, v| [v, k] })
  assert_equal({ "a" => 1, "b" => 1 }, h.to_h(&->(k, v) { [k, 1] }))
  assert_equal :stopped, h.to_h { |k, v| break :stopped }
  assert_raise(TypeError)     { h.to_h { |k, v| k } }
  assert_raise(ArgumentError) { h.to_h { |k, v| [k, v, 1] } }
end

assert('Hash#<') do
  h1 = {a:1, b:2}
  h2 = {a:1, b:2, c:3}

  assert_false(h1 < h1)
  assert_true(h1 < h2)
  assert_false(h2 < h1)
  assert_false(h2 < h2)

  h1 = {a:1}
  h2 = {a:2}

  assert_false(h1 < h1)
  assert_false(h1 < h2)
  assert_false(h2 < h1)
  assert_false(h2 < h2)
end

assert('Hash#< and its siblings take a value for equal to itself') do
  # CRuby compares the two values with `rb_equal()`, the subset's value as the
  # receiver, which answers for an object and itself before it asks `==`. The
  # four operators compare each pair through `Hash#__value_eq`, which answers
  # what `mrb_equal()` answers in C and hands a `==` written in Ruby back.
  never = Class.new { def ==(other); false; end }.new
  always = Class.new { def ==(other); true; end }.new
  h1 = {a: never}
  h2 = {a: never, b: always}

  assert_true(h1 < h2)
  assert_true(h1 <= h2)
  assert_true(h2 > h1)
  assert_true(h2 >= h1)
  assert_true(h1 <= h1)
  assert_true(h1 >= h1)
  assert_false(h1 <= {a: Object.new})
  assert_true({a: always} <= {a: Object.new})
  assert_false({a: Object.new} <= {a: always})
  assert_false({a: never} <= {b: never})
end

assert('Hash#<= sends a `==` written in Ruby in the VM it runs in') do
  # `Hash#__value_eq` answers `:send` for such a `==` instead of calling it
  # from C, so a `Fiber.yield` inside it has no C frame to cross, as in CRuby.
  yielder = Class.new { def ==(other); Fiber.yield(:asked); true; end }.new
  f = Fiber.new { {a: yielder} <= {a: Object.new} }
  assert_equal :asked, f.resume
  assert_true f.resume
end

assert('Hash#<=') do
  h1 = {a:1, b:2}
  h2 = {a:1, b:2, c:3}

  assert_true(h1 <= h1)
  assert_true(h1 <= h2)
  assert_false(h2 <= h1)
  assert_true(h2 <= h2)

  h1 = {a:1}
  h2 = {a:2}

  assert_true(h1 <= h1)
  assert_false(h1 <= h2)
  assert_false(h2 <= h1)
  assert_true(h2 <= h2)
end

assert('Hash#>=') do
  h1 = {a:1, b:2}
  h2 = {a:1, b:2, c:3}

  assert_true(h1 >= h1)
  assert_false(h1 >= h2)
  assert_true(h2 >= h1)
  assert_true(h2 >= h2)

  h1 = {a:1}
  h2 = {a:2}

  assert_true(h1 >= h1)
  assert_false(h1 >= h2)
  assert_false(h2 >= h1)
  assert_true(h2 >= h2)
end

assert('Hash#>') do
  h1 = {a:1, b:2}
  h2 = {a:1, b:2, c:3}

  assert_false(h1 > h1)
  assert_false(h1 > h2)
  assert_true(h2 > h1)
  assert_false(h2 > h2)

  h1 = {a:1}
  h2 = {a:2}

  assert_false(h1 > h1)
  assert_false(h1 > h2)
  assert_false(h2 > h1)
  assert_false(h2 > h2)
end

assert("Hash#dig") do
  h = {a:{b:{c:1}}}
  assert_equal(1, h.dig(:a, :b, :c))
  assert_nil(h.dig(:d))
end

assert("Hash#transform_keys") do
  h = {"1" => 100, "2" => 200}
  assert_equal({"1!" => 100, "2!" => 200},
               h.transform_keys{|k| k+"!"})
  assert_equal({1 => 100, 2 => 200},
               h.transform_keys{|k|k.to_i})
  assert_same(h, h.transform_keys!{|k|k.to_i})
  assert_equal({1 => 100, 2 => 200}, h)
end

assert("Hash#transform_values") do
  h = {a: 1, b: 2, c: 3}
  assert_equal({a: 2, b: 5, c: 10},
               h.transform_values{|v| v * v + 1})
  assert_equal({a: "1", b: "2", c: "3"},
               h.transform_values{|v|v.to_s})
  assert_same(h, h.transform_values!{|v|v.to_s})
  assert_equal({a: "1", b: "2", c: "3"}, h)
end

assert("Hash#slice") do
  h = { a: 100, b: 200, c: 300 }
  assert_equal({:a=>100}, h.slice(:a))
  assert_equal({:b=>200, :c=>300}, h.slice(:b, :c, :d))
end

assert("Hash#slice!") do
  h = { a: 1, b: 2, c: 3, d: 4 }
  removed = h.slice!(:a, :c)
  assert_equal({ a: 1, c: 3 }, h)
  assert_equal({ b: 2, d: 4 }, removed)

  h = { a: 1, b: 2 }
  removed = h.slice!()
  assert_equal({}, h)
  assert_equal({ a: 1, b: 2 }, removed)

  h = { a: 1, b: 2 }
  removed = h.slice!(:a, :b, :c)
  assert_equal({ a: 1, b: 2 }, h)
  assert_equal({}, removed)
end

assert("Hash#except") do
  h = { a: 100, b: 200, c: 300 }
  assert_equal({:b=>200, :c=>300}, h.except(:a))
  assert_equal({:a=>100}, h.except(:b, :c, :d))
  assert_equal(h, h.except)
  assert_not_same(h, h.except)

  # as in CRuby, the result is a plain Hash without the receiver's default
  h2 = Class.new(Hash).new
  h2[:a] = 1
  assert_equal(Hash, h2.except.class)
  h3 = { a: 1 }
  h3.default = 42
  assert_nil(h3.except(:a).default)
end
