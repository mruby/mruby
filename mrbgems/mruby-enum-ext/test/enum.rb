##
# Enumerable(Ext) Test

assert("Enumerable#drop") do
  a = [1, 2, 3, 4, 5, 0]

  assert_equal a.drop(3), [4, 5, 0]
  assert_equal a.drop(6), []
end

assert("Enumerable#drop_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.drop_while {|i| i < 3 }, [3, 4, 5, 0]
end

assert("Enumerable#take") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.take(3), [1, 2, 3]
end

assert("Enumerable#take_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.take_while {|i| i < 3 }, [1, 2]
end

assert("Enumerable#each_cons") do
  a = []
  (1..5).each_cons(3){|e| a << e}
  assert_equal a, [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
end

assert("Enumerable#each_slice") do
  a = []
  (1..10).each_slice(3){|e| a << e}
  assert_equal  a, [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
end

