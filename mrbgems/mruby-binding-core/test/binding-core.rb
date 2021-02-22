assert("Kernel.#binding") do
  assert_kind_of Binding, binding
end

assert("Binding#local_variables") do
  block = Proc.new do |a|
    b = 1
    binding
  end
  assert_equal [:a, :b, :block], block.call(0).local_variables.sort
end
