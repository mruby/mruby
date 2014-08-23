assert('operator override', 'issue #2557') do
  class Optest
    def +(x)
      "add #{x}"
    end
    def -(x)
      "sub #{x}"
    end
  end

  q = Optest.new
  assert_equal('add 5',  q + +5)
  assert_equal('add -5', q + -5)
  assert_equal('sub 5',  q - +5)
  assert_equal('sub -5', q - -5)
  assert_equal('add 500',  q + +500)
  assert_equal('add -500', q + -500)
  assert_equal('sub 500',  q - +500)
  assert_equal('sub -500', q - -500)
end
