assert('Proc#codedump') do
  assert_raise(TypeError) { CFuncProc.codedump }

  p = Proc.new {
    class A
      def func
      end
    end
  }
  p.codedump

  true
end

assert('String#parser_dump') do
  wrong_script = <<EOS
class A
  def func
  end
EOS
  assert_raise(SyntaxError) { wrong_script.parser_dump }

  begin
    wrong_script.parser_dump
  rescue SyntaxError => e
    assert_equal 'line 4: ', e.message[0, 8]
  end

  str = <<EOS
class A
  def func
  end
end
EOS
  str.parser_dump

  true
end
