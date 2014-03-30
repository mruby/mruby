##
# Array(Ext) Test

assert("Array::try_convert") do
  Array.try_convert([1]) == [1] and
  Array.try_convert("1").nil?
end

assert("Array#assoc") do
  s1 = [ "colors", "red", "blue", "green" ]
  s2 = [ "letters", "a", "b", "c" ]
  s3 = "foo"
  a  = [ s1, s2, s3 ]

  a.assoc("letters") == [ "letters", "a", "b", "c" ] and
  a.assoc("foo").nil?
end

assert("Array#at") do
  a = [ "a", "b", "c", "d", "e" ]
  a.at(0)  == "a" and a.at(-1) == "e"
end

assert("Array#rassoc") do
  a = [ [ 1, "one"], [2, "two"], [3, "three"], ["ii", "two"] ]

  a.rassoc("two") == [2, "two"] and
  a.rassoc("four").nil?
end

assert("Array#uniq!") do
  a = [1, 2, 3, 1]
  a.uniq!
  a == [1, 2, 3]
end

assert("Array#uniq") do
  a = [1, 2, 3, 1]
  a.uniq == [1, 2, 3] && a == [1, 2, 3, 1]
end

assert("Array#-") do
  a = [1, 2, 3, 1]
  b = [1]
  c = 1
  e1 = nil

  begin
    a - c
  rescue => e1
  end

  (a - b) == [2, 3] and e1.class == TypeError and a == [1, 2, 3, 1]
end

assert("Array#|") do
  a = [1, 2, 3, 1]
  b = [1, 4]
  c = 1
  e1 = nil

  begin
    a | c
  rescue => e1
  end

  (a | b) == [1, 2, 3, 4] and e1.class == TypeError and a == [1, 2, 3, 1]
end

assert("Array#&") do
  a = [1, 2, 3, 1]
  b = [1, 4]
  c = 1
  e1 = nil

  begin
    a & c
  rescue => e1
  end

  (a & b) == [1] and e1.class == TypeError and a == [1, 2, 3, 1]
end

assert("Array#flatten") do
  [1, 2, "3", {4=>5}, :'6'] == [1, 2, "3", {4=>5}, :'6'].flatten and
  [1, 2, 3, 4, 5, 6] == [1, 2, [3, 4, 5], 6].flatten and
  [1, 2, 3, 4, 5, 6] == [1, 2, [3, [4, 5], 6]].flatten and
  [1, [2, [3, [4, [5, [6]]]]]] == [1, [2, [3, [4, [5, [6]]]]]].flatten(0) and
  [1, 2, [3, [4, [5, [6]]]]] == [1, [2, [3, [4, [5, [6]]]]]].flatten(1) and
  [1, 2, 3, [4, [5, [6]]]] == [1, [2, [3, [4, [5, [6]]]]]].flatten(2) and
  [1, 2, 3, 4, [5, [6]]] == [1, [2, [3, [4, [5, [6]]]]]].flatten(3) and
  [1, 2, 3, 4, 5, [6]] == [1, [2, [3, [4, [5, [6]]]]]].flatten(4) and
  [1, 2, 3, 4, 5, 6] == [1, [2, [3, [4, [5, [6]]]]]].flatten(5)
end

assert("Array#flatten!") do
  [1, 2, 3, 4, 5, 6] == [1, 2, [3, [4, 5], 6]].flatten!
end

assert("Array#compact") do
  a = [1, nil, "2", nil, :t, false, nil]
  a.compact == [1, "2", :t, false] && a == [1, nil, "2", nil, :t, false, nil]
end

assert("Array#compact!") do
  a = [1, nil, "2", nil, :t, false, nil]
  a.compact!
  a == [1, "2", :t, false]
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
  assert_equal [ "d", "c", "b", "a" ], a.reverse_each.to_a
end
