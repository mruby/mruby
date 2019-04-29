assert 'MRubyVM' do
  assert_true Object.const_defined? :RubyVM
  assert_true Object.const_defined? :MRubyVM
end

assert 'MRubyVM.stat' do
  skip unless MRubyVM.respond_to? :stat

  assert_raise(ArgumentError) { MRubyVM.stat(:__test) }
  assert_kind_of Hash, MRubyVM.stat
  assert_true 1 <= MRubyVM.stat(:global_method_state)
  assert_equal 1, MRubyVM.stat(:global_constant_state)
  assert_equal 1, MRubyVM.stat(:class_serial)
end
