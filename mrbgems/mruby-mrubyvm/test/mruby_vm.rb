assert 'MRubyVM.stat' do
  assert_raise(ArgumentError) { MRubyVM.stat(:__test) }
  assert_kind_of Hash, MRubyVM.stat
  assert_true 1 <= MRubyVM.stat(:global_method_state)
  assert_equal 1, MRubyVM.stat(:global_constant_state)
  assert_equal 1, MRubyVM.stat(:class_serial)
end
