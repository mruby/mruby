def run_numeric_override_test(text, expected_result_override, expected_result_regular)
  result = NumericOverrideTest::run(text)

  assert_equal result, NumericOverrideTest::MRB_ENABLE_NUMERIC_OVERRIDE ? expected_result_override : expected_result_regular
end

assert 'Float/Float override' do
  run_numeric_override_test <<-'END', "100", "6"

class Fixnum
  def +(other)
    100
  end
end

2 + 4

END
end

assert 'Float/Float override' do
  run_numeric_override_test <<-'END', "100.5", "7"

class Float
  def +(other)
    100.5
  end
end

2.5 + 4.5

END
end
