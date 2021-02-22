assert("Binding#eval") do
  b = nil
  1.times { x, y, z = 1, 2, 3; [x,y,z]; b = binding }
  assert_equal([1, 2, 3], b.eval("[x, y, z]"))
end
