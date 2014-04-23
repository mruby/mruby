##
# Array(Ext) Test

assert("Array::try_convert") do
  assert_equal [1], Array.try_convert([1])
  assert_nil Array.try_convert("1")
end

assert("Array#assoc") do
  s1 = [ "colors", "red", "blue", "green" ]
  s2 = [ "letters", "a", "b", "c" ]
  s3 = "foo"
  a  = [ s1, s2, s3 ]

  assert_equal [ "letters", "a", "b", "c" ], a.assoc("letters")
  assert_nil a.assoc("foo")
end

assert("Array#at") do
  a = [ "a", "b", "c", "d", "e" ]
  assert_equal "a", a.at(0)
  assert_equal "e", a.at(-1) 
end

assert("Array#rassoc") do
  a = [ [ 1, "one"], [2, "two"], [3, "three"], ["ii", "two"] ]

  assert_equal [2, "two"], a.rassoc("two")
  assert_nil a.rassoc("four")
end

assert("Array#uniq!") do
  a = [1, 2, 3, 1]
  a.uniq!
  assert_equal [1, 2, 3], a

  b = [ "a", "b", "c" ]
  assert_nil b.uniq!

  c = [["student","sam"], ["student","george"], ["teacher","matz"]]
  assert_equal [["student", "sam"], ["teacher", "matz"]], c.uniq! { |s| s.first }

  d = [["student","sam"], ["teacher","matz"]]
  assert_nil d.uniq! { |s| s.first }
end

assert("Array#uniq") do
  a = [1, 2, 3, 1]
  assert_equal [1, 2, 3], a.uniq
  assert_equal [1, 2, 3, 1], a

  b = [["student","sam"], ["student","george"], ["teacher","matz"]]
  assert_equal [["student", "sam"], ["teacher", "matz"]], b.uniq { |s| s.first } 
end

assert("Array#-") do
  a = [1, 2, 3, 1]
  b = [1]
  c = 1

  assert_raise(TypeError) { a - c }
  assert_equal [2, 3], (a - b)
  assert_equal [1, 2, 3, 1], a 
end

assert("Array#|") do
  a = [1, 2, 3, 1]
  b = [1, 4]
  c = 1

  assert_raise(TypeError) { a | c }
  assert_equal [1, 2, 3, 4], (a | b)
  assert_equal [1, 2, 3, 1], a 
end

assert("Array#&") do
  a = [1, 2, 3, 1]
  b = [1, 4]
  c = 1

  assert_raise(TypeError) { a & c }
  assert_equal [1], (a & b) 
  assert_equal [1, 2, 3, 1], a 
end

assert("Array#flatten") do
  assert_equal [1, 2, "3", {4=>5}, :'6'],    [1, 2, "3", {4=>5}, :'6'].flatten
  assert_equal [1, 2, 3, 4, 5, 6], [1, 2,    [3, 4, 5], 6].flatten
  assert_equal [1, 2, 3, 4, 5, 6], [1, 2,    [3, [4, 5], 6]].flatten
  assert_equal [1, [2, [3, [4, [5, [6]]]]]], [1, [2, [3, [4, [5, [6]]]]]].flatten(0)
  assert_equal [1, 2, [3, [4, [5, [6]]]]],   [1, [2, [3, [4, [5, [6]]]]]].flatten(1)
  assert_equal [1, 2, 3, [4, [5, [6]]]],     [1, [2, [3, [4, [5, [6]]]]]].flatten(2)
  assert_equal [1, 2, 3, 4, [5, [6]]],       [1, [2, [3, [4, [5, [6]]]]]].flatten(3)
  assert_equal [1, 2, 3, 4, 5, [6]],         [1, [2, [3, [4, [5, [6]]]]]].flatten(4)
  assert_equal [1, 2, 3, 4, 5, 6],           [1, [2, [3, [4, [5, [6]]]]]].flatten(5)
end

assert("Array#flatten!") do
  assert_equal [1, 2, 3, 4, 5, 6], [1, 2, [3, [4, 5], 6]].flatten!
end

assert("Array#compact") do
  a = [1, nil, "2", nil, :t, false, nil]
  assert_equal [1, "2", :t, false], a.compact
  assert_equal [1, nil, "2", nil, :t, false, nil], a
end

assert("Array#compact!") do
  a = [1, nil, "2", nil, :t, false, nil]
  a.compact!
  assert_equal [1, "2", :t, false], a
end

assert("Array#fetch") do
  a = [ 11, 22, 33, 44 ]
  assert_equal 22, a.fetch(1)
  assert_equal 44, a.fetch(-1)
  assert_equal 'cat', a.fetch(4, 'cat')
  ret = 0
  a.fetch(100) { |i| ret = i }
  assert_equal 100, ret
  assert_raise(IndexError) { a.fetch(100) }
end

assert("Array#fill") do
  a = [ "a", "b", "c", "d" ]
  assert_equal ["x", "x", "x", "x"], a.fill("x")
  assert_equal ["x", "x", "x", "w"], a.fill("w", -1) 
  assert_equal ["x", "x", "z", "z"], a.fill("z", 2, 2)
  assert_equal ["y", "y", "z", "z"], a.fill("y", 0..1)
  assert_equal [0, 1, 4, 9], a.fill { |i| i*i } 
  assert_equal [0, 1, 8, 27], a.fill(-2) { |i| i*i*i }
  assert_equal [0, 2, 3, 27], a.fill(1, 2) { |i| i+1 }
  assert_equal [1, 2, 3, 27], a.fill(0..1) { |i| i+1 }
  assert_raise(ArgumentError) { a.fill }
end

assert("Array#reverse_each") do
  a = [ "a", "b", "c", "d" ]
  b = []
  a.reverse_each do |i|
    b << i
  end
  assert_equal [ "d", "c", "b", "a" ], b

  if Object.const_defined?(:Enumerator)
    assert_equal [ "d", "c", "b", "a" ], a.reverse_each.to_a
  else
    true
  end
end

assert("Array#rotate") do
  a = ["a", "b", "c", "d"]
  assert_equal ["b", "c", "d", "a"], a.rotate
  assert_equal ["a", "b", "c", "d"], a
  assert_equal ["c", "d", "a", "b"], a.rotate(2)
  assert_equal ["b", "c", "d", "a"], a.rotate(-3)
  assert_equal ["c", "d", "a", "b"], a.rotate(10)
  assert_equal [], [].rotate
end

assert("Array#rotate!") do
  a = ["a", "b", "c", "d"]
  assert_equal ["b", "c", "d", "a"], a.rotate!
  assert_equal ["b", "c", "d", "a"], a
  assert_equal ["d", "a", "b", "c"], a.rotate!(2)
  assert_equal ["a", "b", "c", "d"], a.rotate!(-3)
  assert_equal ["c", "d", "a", "b"], a.rotate(10)
  assert_equal [], [].rotate!
end

assert("Array#delete_if") do
  a = [1, 2, 3, 4, 5]
  assert_equal [1, 2, 3, 4, 5], a.delete_if { false }
  assert_equal [1, 2, 3, 4, 5], a

  a = [1, 2, 3, 4, 5]
  assert_equal [], a.delete_if { true }
  assert_equal [], a

  a = [1, 2, 3, 4, 5]
  assert_equal [1, 2, 3], a.delete_if { |i| i > 3 }
  assert_equal [1, 2, 3], a
end

assert("Array#reject!") do
  a = [1, 2, 3, 4, 5]
  assert_nil a.reject! { false }
  assert_equal [1, 2, 3, 4, 5], a

  a = [1, 2, 3, 4, 5]
  assert_equal [], a.reject! { true }
  assert_equal [], a

  a = [1, 2, 3, 4, 5]
  assert_equal [1, 2, 3], a.reject! { |val| val > 3 }
  assert_equal [1, 2, 3], a
end

assert("Array#insert") do
  a = ["a", "b", "c", "d"]
  assert_equal ["a", "b", 99, "c", "d"], a.insert(2, 99)
  assert_equal ["a", "b", 99, "c", 1, 2, 3, "d"], a.insert(-2, 1, 2, 3)

  b = ["a", "b", "c", "d"]
  assert_equal ["a", "b", "c", "d", nil, nil, 99], b.insert(6, 99)
end
