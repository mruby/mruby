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

