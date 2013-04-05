##
# Hash(Ext) Test

assert('Hash#merge!') do
  a = { 'abc_key' => 'abc_value', 'cba_key' => 'cba_value' }
  b = { 'cba_key' => 'XXX',  'xyz_key' => 'xyz_value' }

  result_1 = a.merge! b
  
  a = { 'abc_key' => 'abc_value', 'cba_key' => 'cba_value' }
  result_2 = a.merge!(b) do |key, original, new|
    original
  end

  result_1 == {'abc_key' => 'abc_value', 'cba_key' => 'XXX',
               'xyz_key' => 'xyz_value' } and
  result_2 == {'abc_key' => 'abc_value', 'cba_key' => 'cba_value',
               'xyz_key' => 'xyz_value' }
end

