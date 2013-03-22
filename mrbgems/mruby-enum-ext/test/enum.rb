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

