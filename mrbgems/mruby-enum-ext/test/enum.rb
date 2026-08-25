##
# Enumerable(Ext) Test

assert("Enumerable#drop") do
  a = [1, 2, 3, 4, 5, 0]

  assert_equal [4, 5, 0], a.drop(3)
  assert_equal [], a.drop(6)
end

assert("Enumerable#drop_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal [3, 4, 5, 0], a.drop_while {|i| i < 3 }
end

assert("Enumerable#take") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal [1, 2, 3], a.take(3)
end

assert("Enumerable#take_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal [1, 2], a.take_while {|i| i < 3}
end

assert("Enumerable#each_cons") do
  a = []
  b = (1..5).each_cons(3){|e| a << e}
  assert_equal [[1, 2, 3], [2, 3, 4], [3, 4, 5]], a
  assert_equal nil, b
end

assert("Enumerable#each_slice") do
  a = []
  b = (1..10).each_slice(3){|e| a << e}
  assert_equal [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]], a
  assert_equal nil, b
end

assert("Enumerable#group_by") do
  r = (1..6).group_by {|i| i % 3 }
  assert_equal [3, 6], r[0]
  assert_equal [1, 4], r[1]
  assert_equal [2, 5], r[2]
end

assert("Enumerable#sort_by") do
  assert_equal ["car", "train", "bicycle"], %w{car bicycle train}.sort_by {|e| e.length}
end

assert("Enumerable#first") do
  a = Object.new
  a.extend Enumerable
  def a.each
    yield 1
    yield 2
    yield 3
  end
  assert_equal 1, a.first
  assert_equal [1, 2], a.first(2)
  assert_equal [1, 2, 3], a.first(10)
  a = Object.new
  a.extend Enumerable
  def a.each
  end
  assert_nil a.first
end

assert("Enumerable#count") do
  a = [1, 2, 4, 2]
  assert_equal 4, a.count
  assert_equal 2, a.count(2)
  assert_equal 3, a.count{|x| x % 2 == 0}
end

assert("Enumerable#flat_map") do
  assert_equal [1, 2, 3, 4], [1, 2, 3, 4].flat_map { |e| e }
  assert_equal [1, -1, 2, -2, 3, -3, 4, -4], [1, 2, 3, 4].flat_map { |e| [e, -e] }
  assert_equal [1, 2, 100, 3, 4, 100], [[1, 2], [3, 4]].flat_map { |e| e + [100] }
end

assert("Enumerable#max_by") do
  assert_equal "albatross", %w[albatross dog horse].max_by { |x| x.length }
end

assert("Enumerable#min_by") do
  assert_equal "dog", %w[albatross dog horse].min_by { |x| x.length }
end

assert("Enumerable#minmax") do
  a = %w(albatross dog horse)
  assert_equal ["albatross", "horse"], a.minmax
  assert_equal ["dog", "albatross"], a.minmax { |a, b| a.length <=> b.length }
end

assert("Enumerable#minmax_by") do
  assert_equal ["dog", "albatross"], %w(albatross dog horse).minmax_by { |x| x.length }
end

assert("Enumerable#none?") do
  assert_true %w(ant bear cat).none? { |word| word.length == 5 }
  assert_false %w(ant bear cat).none? { |word| word.length >= 4 }
  assert_true [].none?
  assert_true [nil, false].none?
  assert_false [nil, true].none?
end

assert("Enumerable#one?") do
  assert_true %w(ant bear cat).one? { |word| word.length == 4 }
  assert_false %w(ant bear cat).one? { |word| word.length > 4 }
  assert_false %w(ant bear cat).one? { |word| word.length < 4 }
  assert_false [nil, true, 99].one?
  assert_true [nil, true, false].one?
  assert_true [ nil, true, 99 ].one?(Integer)
  assert_false [].one?
  assert_true [nil, true, false].one?(NilClass)
end

assert("Enumerable#all? (enhancement)") do
  assert_false [1, 2, nil].all?(Integer)
  assert_true [1, 2, 3].all?(Numeric)
end

assert("Enumerable#any? (enhancement)") do
  assert_true [nil, true, 99].any?(Integer)
  assert_false [1, 2, 3].any?(Array)
end

assert("Enumerable#each_with_object") do
  assert_equal [2, 4, 6, 8, 10, 12, 14, 16, 18, 20], (1..10).each_with_object([]) { |i, a| a << i*2 }
  assert_raise(ArgumentError) { (1..10).each_with_object() { |i, a| a << i*2 } }
end

assert("Enumerable#reverse_each") do
  r = (1..3)
  a = []
  assert_same r, r.reverse_each { |v| a << v }
  assert_equal [3, 2, 1], a
end

assert("Enumerable#cycle") do
  a = []
  ["a", "b", "c"].cycle(2) { |v| a << v }
  assert_equal ["a", "b", "c", "a", "b", "c"], a
  assert_raise(TypeError) { ["a", "b", "c"].cycle("a") { |v| a << v } }

  empty = Class.new do
    include Enumerable
    def each
    end
  end
  assert_nil empty.new.cycle { break :nope }
end

assert("Enumerable#find_index") do
  assert_nil (1..10).find_index { |i| i % 5 == 0 and i % 7 == 0 }
  assert_equal 34, (1..100).find_index { |i| i % 5 == 0 and i % 7 == 0 }
  assert_equal 49 ,(1..100).find_index(50)
end

assert("Enumerable#zip") do
  a = [ 4, 5, 6 ]
  b = [ 7, 8, 9 ]
  assert_equal [[4, 7], [5, 8], [6, 9]], a.zip(b)
  assert_equal [[1, 4, 7], [2, 5, 8], [3, 6, 9]], [1, 2, 3].zip(a, b)
  assert_equal [[1, 4, 7], [2, 5, 8]], [1, 2].zip(a, b)
  assert_equal [[4, 1, 8], [5, 2, nil], [6, nil, nil]], a.zip([1, 2], [8])

  ret = []
  assert_equal nil, a.zip([1, 2], [8]) { |i| ret << i }
  assert_equal [[4, 1, 8], [5, 2, nil], [6, nil, nil]], ret

  assert_raise(TypeError) { [1].zip(1) }
end

assert("Enumerable#to_h") do
  c = Class.new {
    include Enumerable
    def each
      yield [1,2]
      yield [3,4]
    end
  }
  h0 = {1=>2, 3=>4}
  h = c.new.to_h
  assert_equal Hash, h.class
  assert_equal h0, h
  assert_equal({1=>4,3=>8}, c.new.to_h{|k,v|[k,v*2]})
end

assert("Enumerable#filter_map") do
  assert_equal [4, 8, 12, 16, 20], (1..10).filter_map{|i| i * 2 if i%2==0}
end

assert("Enumerable#tally") do
  assert_equal({"a"=>1, "b"=>2, "c"=>1}, ["a", "b", "c", "b"].tally)
end

assert("Enumerable#grep_v") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal [1, 5, 0], a.grep_v(2..4)
  assert_equal [1, 2, 3, 4, 5, 0], a.grep_v(6..8)
  assert_equal [2, 4, 6, 8, 10], a.grep_v(0) {|v| v * 2}
end

assert("Enumerable#each_entry") do
  each_entry_test = Class.new {
    include Enumerable
    def each
      yield 1
      yield 1, 2
      yield
    end
  }
  e = each_entry_test.new
  a = []
  e.each_entry {|v|
    a.push(v)
  }
  assert_equal 1, a[0]
  assert_equal [1,2], a[1]
  assert_equal nil, a[2]
end

assert('Enumerable size arguments reject a redefined converter') do
  # `n.__to_int` was a dispatch the argument could redefine, and `take` then
  # called `to_i` on whatever came back, so neither call was a type check.
  evil = Class.new { def __to_int; 2; end }.new
  sneaky = Class.new { def __to_int; self; end; def to_i; 3; end }.new
  assert_raise(TypeError) { (1..9).take(evil) }
  assert_raise(TypeError) { (1..9).drop(evil) }
  assert_raise(TypeError) { (1..9).take(sneaky) }
  assert_equal [1, 2], (1..9).take(2)
end

assert('Enumerable#minmax - a comparison with no answer') do
  # The same rule as Enumerable#max and #min, which this walks in one pass.
  assert_raise(ArgumentError) { [1, 2].minmax { |a, b| nil } }

  if Object.const_defined?(:Float)
    assert_raise(ArgumentError) { [1.0, Float::NAN, 2.0].minmax }
  end

  # A pair of different kinds stands in no order either.
  assert_raise(ArgumentError) { [1, 'a'].minmax }

  assert_equal [1, 3], [3, 1, 2].minmax
  assert_equal [1, 3], [3, 1, 2].minmax { |a, b| a <=> b }

  # The first element is never compared, and an empty collection has none.
  assert_equal ['a', 'a'], ['a'].minmax
  assert_equal [nil, nil], [].minmax
end

assert('Array#minmax - the walk made in C') do
  # The same move as Array#max and #min: without a block the walk is in C.
  assert_equal [nil, nil], [].minmax
  assert_equal [5, 5], [5].minmax
  assert_equal [1, 3], [3, 1, 2].minmax
  assert_equal ["a", "b"], ["b", "a"].minmax
  assert_equal [3, 1], [3, 1, 2].minmax {|x, y| y <=> x }
  assert_raise(ArgumentError) { [1, "a"].minmax }
  assert_raise(ArgumentError) { [1, "a"].minmax {|x, y| x <=> y } }

  cls = Class.new do
    include Comparable
    def initialize(v, a); @v = v; @a = a; end
    attr_reader :v
    def <=>(o); @a.clear; @v <=> o.v; end
  end
  a = []
  3.times {|i| a << cls.new(i, a) }
  assert_equal 2, a.minmax.size
end

assert('Array#count - the walk made in C') do
  # `Enumerable#count` reaches an element through a call to `each`, a block
  # call and a `__svalue` send, then compares it with `==`. An Array reads its
  # length off itself, and an argument is counted by a walk made in C where
  # the pair is compared with `mrb_equal()`, as `Array#index` compares one.
  a = [1, 2, 4, 2]
  assert_equal 4, a.count
  assert_equal 0, [].count
  assert_equal 2, a.count(2)
  assert_equal 0, a.count(3)
  assert_equal 3, a.count {|x| x % 2 == 0}     # the block form, through super

  # an argument decides even where a block came with it, as in CRuby, where
  # `Enumerable#count` took the block
  assert_equal 2, a.count(2) {|x| true}

  # an element is taken for equal to itself before `==` is asked anything
  never = Class.new { def ==(other); false; end }.new
  assert_equal 1, [never].count(never)
end

assert('Array#count - a comparison that runs Ruby') do
  # `==` can run Ruby, which can empty the array being walked. The C walk
  # reads the length and the pointer afresh each time round.
  cls = Class.new do
    def initialize(a); @a = a; end
    def ==(o); @a.clear; false; end
  end
  a = [1, 2]
  a << cls.new(a)
  assert_equal 0, a.count(:missing)
  assert_equal 0, a.size
end

assert("Array#count - a comparison that answers with a fresh object") do
  # What a call leaves behind in the GC arena is its return value, and a count
  # walks every element rather than returning at the first true one, so an `==`
  # answering with a fresh object each time would pile them up. The walk drops
  # what a turn left before the next one adds to it, which is what keeps a
  # fixed arena from filling with the length of the array.
  cls = Class.new { def ==(o); "truthy"; end }
  a = Array.new(300) { cls.new }
  assert_equal 300, a.count(:x)
end

assert('Enumerable#count - an argument decides over a block') do
  # CRuby counts by the argument and warns that the block is unused. The block
  # was tested first here, so a block passed alongside an argument took over
  # the count. Array#count answers by the argument, and so does every other
  # Enumerable now.
  cls = Class.new do
    include Enumerable
    def each; yield 1; yield 2; yield 2; end
  end
  e = cls.new
  assert_equal 2, e.count(2) {|x| true }
  assert_equal 2, e.count(2)
  assert_equal 3, e.count
  assert_equal 3, e.count {|x| true }
  assert_equal 1, (1..4).count(2) {|x| true }
  assert_equal 1, ({a: 1, b: 2}).count([:a, 1]) {|x| true }

  # an element is taken for equal to itself, `==` reaching it through OP_EQ
  never = Class.new { def ==(other); false; end }.new
  one = Class.new do
    include Enumerable
    define_method(:each) {|&b| b.call(never) }
  end.new
  assert_equal 1, one.count(never)
end

assert("Array#count with a NaN") do
  # A NaN is equal to no value, its own included, so `count` cannot find one by
  # what it is equal to; it searches for the object, and every NaN made is one
  # of its own, so that two made apart are two objects.
  skip unless Object.const_defined?(:Float)
  z = [0.0][0]
  a = z / z
  b = z / z

  assert_equal 1, [a].count(a)
  assert_equal 0, [a].count(b)
  assert_equal 2, [a, a].count(a)
end
