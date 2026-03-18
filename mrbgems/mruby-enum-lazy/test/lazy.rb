assert("Enumerator::Lazy") do
  a = [1, 2]
  assert_equal Enumerator::Lazy, a.lazy.class
end

assert("Enumerator::Lazy laziness") do
  a = Object.new
  def a.each
    return to_enum :each unless block_given?
    self.b << 10
    yield 1
    self.b << 20
    yield 2
    self.b << 30
    yield 3
    self.b << 40
    yield 4
    self.b << 50
    yield 5
  end
  def a.b(b=nil)
    @b = b if b
    @b
  end

  a.b([])
  assert_equal [1,2], a.each.lazy.take(2).force
  assert_equal [10,20], a.b

  a.b([])
  assert_equal [2,4], a.each.lazy.select{|x|x%2==0}.take(2).force
  assert_equal [10,20,30,40], a.b

  a.b([])
  assert_equal [1], a.each.lazy.take_while{|x|x<2}.take(1).force
  assert_equal [10], a.b

  a.b([])
  assert_equal [1], a.each.lazy.take_while{|x|x<2}.take(4).force
  assert_equal [10,20], a.b
end

assert("Enumerator::Lazy#to_enum") do
  lazy_enum = (0..).lazy.to_enum(:each_slice, 2)
  assert_kind_of Enumerator::Lazy, lazy_enum
  assert_equal [0*1, 2*3, 4*5, 6*7], lazy_enum.map { |a| a.first * a.last }.first(4)
end

assert("Enumerator::Lazy#flat_map with array from block") do
  assert_equal [1, 10, 2, 20, 3, 30], [1, 2, 3].lazy.flat_map {|x| [x, x*10]}.force
end

assert("Enumerator::Lazy#flat_map with non-enumerable from block") do
  assert_equal [1, 2, 3], [1, 2, 3].lazy.flat_map {|x| x}.force
end

assert("Enumerator::Lazy#flat_map with nested array from block") do
  assert_equal [[1, 2], [3, 4]], [1, 3].lazy.flat_map {|x| [[x, x+1]]}.force
end

assert("Enumerator::Lazy#grep_v") do
  lazy_grep_v = (0..).lazy.grep_v(2..4)
  assert_kind_of Enumerator::Lazy, lazy_grep_v
  assert_equal [0, 1, 5, 6], lazy_grep_v.first(4)
end

assert("Enumerator::Lazy#tap_each") do
  seen = []
  result = [1, 2, 3, 4, 5].lazy.tap_each{|x| seen << x }.select{|x| x % 2 == 0 }.force
  assert_equal [2, 4], result
  assert_equal [1, 2, 3, 4, 5], seen
end

assert("Enumerator::Lazy#tap_each laziness") do
  seen = []
  [1, 2, 3, 4, 5].lazy.tap_each{|x| seen << x }.first(3)
  assert_equal [1, 2, 3], seen
end

assert("Enumerator::Lazy#zip with cycle") do
  e1 = [1, 2, 3].cycle
  e2 = [:a, :b].cycle
  assert_equal [[1,:a],[2,:b],[3,:a]], e1.lazy.zip(e2).first(3)
end
