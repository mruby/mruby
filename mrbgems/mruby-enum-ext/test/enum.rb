##
# Enumerable(Ext) Test

assert("Enumrable#drop") do
  a = [1, 2, 3, 4, 5, 0]

  assert_equal a.drop(3), [4, 5, 0]
  assert_equal a.drop(6), []
end

assert("Enumrable#drop_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.drop_while {|i| i < 3 }, [3, 4, 5, 0]
end

assert("Enumrable#take") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.take(3), [1, 2, 3]
end

assert("Enumrable#take_while") do
  a = [1, 2, 3, 4, 5, 0]
  assert_equal a.take_while {|i| i < 3 }, [1, 2]
end

