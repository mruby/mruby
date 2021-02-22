assert("Kernel.#binding") do
  assert_kind_of Binding, binding
end

assert("Binding#local_variables") do
  block = Proc.new do |a|
    b = 1
    binding
  end
  assert_equal [:a, :b, :block], block.call(0).local_variables
end

assert("Binding#eval") do
  b = nil
  1.times { x, y, z = 1, 2, 3; [x,y,z]; b = binding }
  assert_equal([1, 2, 3], b.eval("[x, y, z]"))
end
