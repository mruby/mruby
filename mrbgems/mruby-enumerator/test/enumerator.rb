@obj = Object.new
class << @obj
  include Enumerable
  def foo *a
    a.each { |x| yield x }
  end
end

assert 'Enumerator' do
  assert_equal Class, Enumerator.class
end

assert 'Enumerator' do
  assert_equal Object, Enumerator.superclass
end

assert 'Enumerator.new' do
  assert_equal [0,1,2], 3.times.map{|i| i}.sort
  assert_equal [:x,:y,:z], [:x,:y,:z].each.map{|i| i}.sort
  assert_equal [[:x,1],[:y,2]], {x:1, y:2}.each.map{|i| i}.sort
  assert_equal [1,2,3], @obj.to_enum(:foo, 1,2,3).to_a
  assert_equal [1,2,3], Enumerator.new(@obj, :foo, 1,2,3).to_a
  assert_equal [1,2,3], Enumerator.new { |y| i = 0; loop { y << (i += 1) } }.take(3)
  assert_raise(ArgumentError) { Enumerator.new }
  enum = @obj.to_enum
  assert_raise(NoMethodError) { enum.each {} }

  # examples
  fib = Enumerator.new do |y|
    a = b = 1
    loop do
      y << a
      a, b = b, a + b
    end
  end
  assert_equal fib.take(10), [1,1,2,3,5,8,13,21,34,55]
end

assert 'Enumerator#initialize_copy' do
  assert_equal [1, 2, 3], @obj.to_enum(:foo, 1, 2, 3).dup.to_a
  e = @obj.to_enum :foo, 1, 2, 3
  assert_nothing_raised { assert_equal(1, e.next) }
  assert_raise(TypeError) { e.dup }

  e = Enumerator.new { |y| i = 0; loop { y << (i += 1) } }.dup
  assert_nothing_raised { assert_equal(1, e.next) }
  assert_raise(TypeError) { e.dup }
end

assert 'Enumerator#with_index' do
  assert_equal([[1,0],[2,1],[3,2]], @obj.to_enum(:foo, 1, 2, 3).with_index.to_a)
  assert_equal([[1,5],[2,6],[3,7]], @obj.to_enum(:foo, 1, 2, 3).with_index(5).to_a)
end

assert 'Enumerator#with_index nonnum offset' do
  s = Object.new
  def s.to_int; 1 end
  assert_equal([[1,1],[2,2],[3,3]], @obj.to_enum(:foo, 1, 2, 3).with_index(s).to_a)
end

assert 'Enumerator#with_index string offset' do
  assert_raise(TypeError){ @obj.to_enum(:foo, 1, 2, 3).with_index('1').to_a }
end

assert 'Enumerator#with_object' do
  obj = [0, 1]
  ret = (1..10).each.with_object(obj) {|i, memo|
    memo[0] += i
    memo[1] *= i
  }
  assert_true(obj.equal?(ret))
  assert_equal([55, 3628800], ret)
end

assert 'Enumerator#inspect' do
  e = (0..10).each
  assert_equal("#<Enumerator: 0..10:each>", e.inspect)
end

assert 'Enumerator#each' do
  o = Object.new
  def o.each(ary)
    ary << 1
    yield
  end
  ary = []
  e = o.to_enum.each(ary)
  e.next
  assert_equal([1], ary)
end

assert 'Enumerator#next' do
  e = 3.times
  3.times { |i|
    assert_equal i, e.next
  }
  assert_raise(StopIteration) { e.next }
end

assert 'Enumerator#next_values' do
  o = Object.new
  def o.each
    yield
    yield 1
    yield 1, 2
  end
  e = o.to_enum
  assert_equal nil, e.next
  assert_equal 1, e.next
  assert_equal [1,2], e.next
  e = o.to_enum
  assert_equal [], e.next_values
  assert_equal [1], e.next_values
  assert_equal [1,2], e.next_values
end

assert 'Enumerator#peek' do
  a = [1]
  e = a.each
  assert_equal 1, e.peek
  assert_equal 1, e.peek
  assert_equal 1, e.next
  assert_raise(StopIteration) { e.peek }
  assert_raise(StopIteration) { e.peek }
end

assert 'Enumerator#peek modify' do
  o = Object.new
  def o.each
    yield 1,2
  end
  e = o.to_enum
  a = e.peek
  a << 3
  assert_equal([1,2], e.peek)
end

assert 'Enumerator#peek_values' do
  o = Object.new
  def o.each
    yield
    yield 1
    yield 1, 2
  end
  e = o.to_enum
  assert_equal nil, e.peek
  assert_equal nil, e.next
  assert_equal 1, e.peek
  assert_equal 1, e.next
  assert_equal [1,2], e.peek
  assert_equal [1,2], e.next
  e = o.to_enum
  assert_equal [], e.peek_values
  assert_equal [], e.next_values
  assert_equal [1], e.peek_values
  assert_equal [1], e.next_values
  assert_equal [1,2], e.peek_values
  assert_equal [1,2], e.next_values
  e = o.to_enum
  assert_equal [], e.peek_values
  assert_equal nil, e.next
  assert_equal [1], e.peek_values
  assert_equal 1, e.next
  assert_equal [1,2], e.peek_values
  assert_equal [1,2], e.next
  e = o.to_enum
  assert_equal nil, e.peek
  assert_equal [], e.next_values
  assert_equal 1, e.peek
  assert_equal [1], e.next_values
  assert_equal [1,2], e.peek
  assert_equal [1,2], e.next_values
end

assert 'Enumerator#peek_values modify' do
  o = Object.new
  def o.each
    yield 1,2
  end
  e = o.to_enum
  a = e.peek_values
  a << 3
  assert_equal [1,2], e.peek
end

assert 'Enumerator#feed' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum :each, ary
  e.next
  e.feed 1
  e.next
  e.feed 2
  e.next
  e.feed 3
  assert_raise(StopIteration) { e.next }
  assert_equal [1,2,3], ary
end

assert 'Enumerator#feed mixed' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum :each, ary
  e.next
  e.feed 1
  e.next
  e.next
  e.feed 3
  assert_raise(StopIteration) { e.next }
  assert_equal [1,nil,3], ary
end

assert 'Enumerator#feed twice' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum :each, ary
  e.feed 1
  assert_raise(TypeError) { e.feed 2 }
end

assert 'Enumerator#feed before first next' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum :each, ary
  e.feed 1
  e.next
  e.next
  assert_equal [1], ary
end

assert 'Enumerator#feed yielder' do
  x = nil
  e = Enumerator.new {|y| x = y.yield; 10 }
  e.next
  e.feed 100
  assert_raise(StopIteration) { e.next }
  assert_equal 100, x
end

assert 'Enumerator#rewind' do
  e = @obj.to_enum(:foo, 1, 2, 3)
  assert_equal 1, e.next
  assert_equal 2, e.next
  e.rewind
  assert_equal 1, e.next
  assert_equal 2, e.next
  assert_equal 3, e.next
  assert_raise(StopIteration) { e.next }
end

assert 'Enumerator#rewind clear feed' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum(:each, ary)
  e.next
  e.feed 1
  e.next
  e.feed 2
  e.rewind
  e.next
  e.next
  assert_equal([1,nil], ary)
end

assert 'Enumerator#rewind clear' do
  o = Object.new
  def o.each(ary)
    ary << yield
    ary << yield
    ary << yield
  end
  ary = []
  e = o.to_enum :each, ary
  e.next
  e.feed 1
  e.next
  e.feed 2
  e.rewind
  e.next
  e.next
  assert_equal [1,nil], ary
end

assert 'Enumerator::Generator' do
  # note: Enumerator::Generator is a class just for internal
  g = Enumerator::Generator.new {|y| y << 1 << 2 << 3; :foo }
  g2 = g.dup
  a = []
  assert_equal(:foo, g.each {|x| a << x })
  assert_equal([1, 2, 3], a)
  a = []
  assert_equal(:foo, g2.each {|x| a << x })
  assert_equal([1, 2, 3], a)
end

assert 'Enumerator::Generator args' do
  g = Enumerator::Generator.new {|y, x| y << 1 << 2 << 3; x }
  a = []
  assert_equal(:bar, g.each(:bar) {|x| a << x })
  assert_equal([1, 2, 3], a)
end

assert 'Enumerator::Yielder' do
  # note: Enumerator::Yielder is a class just for internal
  a = []
  y = Enumerator::Yielder.new {|x| a << x }
  assert_equal(y, y << 1 << 2 << 3)
  assert_equal([1, 2, 3], a)

  a = []
  y = Enumerator::Yielder.new {|x| a << x }
  assert_equal([1], y.yield(1))
  assert_equal([1, 2], y.yield(2))
  assert_equal([1, 2, 3], y.yield(3))

  assert_raise(LocalJumpError) { Enumerator::Yielder.new }
end

assert 'next after StopIteration' do
  a = [1]
  e = a.each
  assert_equal(1, e.next)
  assert_raise(StopIteration) { e.next }
  assert_raise(StopIteration) { e.next }
  e.rewind
  assert_equal(1, e.next)
  assert_raise(StopIteration) { e.next }
  assert_raise(StopIteration) { e.next }
end

assert 'gc' do
  assert_nothing_raised do
    1.times do
      foo = [1,2,3].to_enum
      GC.start
    end
    GC.start
  end
end

assert 'nested iteration' do
  def (o = Object.new).each
    yield :ok1
    yield [:ok2, :x].each.next
  end
  e = o.to_enum
  assert_equal :ok1, e.next
  assert_equal :ok2, e.next
  assert_raise(StopIteration) { e.next }
end

assert 'Kernel#to_enum' do
  assert_equal Enumerator, [].to_enum.class
  assert_raise(ArgumentError){ nil.to_enum }
end


assert 'modifying existing methods' do
  e = 3.times
  i = 0
  loop_ret = loop {
    assert_equal i, e.next
    i += 1
  }
  assert_nil loop_ret

  assert_equal Enumerator, loop.class
  assert_equal Enumerator, 3.times.class
  assert_equal Enumerator, [].each.class
  assert_equal Enumerator, [].map.class
  assert_equal Enumerator, {a:1}.each.class
  assert_equal Enumerator, (1..5).each.class
end
