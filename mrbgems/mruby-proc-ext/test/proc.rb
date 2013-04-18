##
# Proc(Ext) Test

assert('Proc#lambda?') do
  assert_true lambda{}.lambda?
  assert_true !Proc.new{}.lambda?
end

assert('Proc#===') do
  proc = Proc.new {|a| a * 2}
  assert_equal (proc === 10), 20
end

assert('Proc#yield') do
  proc = Proc.new {|a| a * 2}
  assert_equal proc.yield(10), 20   
end

assert('Proc#curry') do
  b = proc {|x, y, z| (x||0) + (y||0) + (z||0) }
  assert_equal b.curry[1][2][3], 6
  assert_equal b.curry[1, 2][3, 4], 6
  assert_equal b.curry(5)[1][2][3][4][5], 6
  assert_equal b.curry(5)[1, 2][3, 4][5], 6
  assert_equal b.curry(1)[1], 1

  b = lambda {|x, y, z| (x||0) + (y||0) + (z||0) }
  assert_equal b.curry[1][2][3], 6
  assert_raise(ArgumentError) { b.curry[1, 2][3, 4] }
  assert_raise(ArgumentError) { b.curry(5) }
  assert_raise(ArgumentError) { b.curry(1) }
end

assert('Proc#to_proc') do
  proc = Proc.new {}
  assert_equal proc, proc.to_proc
end

assert('Kernel#proc') do
  assert_true !proc{|a|}.lambda?
end
