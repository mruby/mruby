# assert_equal '[[1], 2, 3]', '*v1, (a, b) = [1,[2, 3]]; [v1, a, b]'
# assert_equal '[[1], 2, 3]', '*v1,(*), (a, b) = [1,:x,[2, 3]]; [v1, a, b]'

# assert_equal '[]',          '*a = *nil; a'
# assert_equal '[nil]',       '*a = nil; a'
assert_equal '2',           'a, a = 1, 2; a', "[ruby-dev:31522]"
assert_equal '[1, 2]',      'a, b = 1, 2'
# assert_equal '[1, 2]', %q{
#   ans = []
#   trace_var(:$a){|v| ans << v}
#   trace_var(:$b){|v| ans << v}
#   $a, $b = 1, 2
#   ans
# }

# assert_equal 'ok', %q{
#   r = :ok
#   :ng.tap {|(r)|}
#   r
# }, '[ruby-dev:31507]'

=begin
# generated by this script:

3.times{|i|
  8.times{|e|
    ary = (0...e).to_a
    a,b,c,d,e,f = nil
    vals = %w(a b c d e f)
    vals[i] = '*' + vals[i]
    program = "#{vals.join(", ")} = *ary"
    eval(program)
    ans = [a,b,c,d,e,f]
    puts %Q{
      assert_equal "#{ans.inspect}", %q{
        ary = #{ary.inspect}
        #{program}; [a, b, c, d, e, f]
      }}
  }
}
=end

      assert_equal "[[], nil, nil, nil, nil, nil]", %q{
        ary = []
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[], 0, nil, nil, nil, nil]", %q{
        ary = [0]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[], 0, 1, nil, nil, nil]", %q{
        ary = [0, 1]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[], 0, 1, 2, nil, nil]", %q{
        ary = [0, 1, 2]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[], 0, 1, 2, 3, nil]", %q{
        ary = [0, 1, 2, 3]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[], 0, 1, 2, 3, 4]", %q{
        ary = [0, 1, 2, 3, 4]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[0], 1, 2, 3, 4, 5]", %q{
        ary = [0, 1, 2, 3, 4, 5]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[[0, 1], 2, 3, 4, 5, 6]", %q{
        ary = [0, 1, 2, 3, 4, 5, 6]
        *a, b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[nil, [], nil, nil, nil, nil]", %q{
        ary = []
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [], nil, nil, nil, nil]", %q{
        ary = [0]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [], 1, nil, nil, nil]", %q{
        ary = [0, 1]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [], 1, 2, nil, nil]", %q{
        ary = [0, 1, 2]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [], 1, 2, 3, nil]", %q{
        ary = [0, 1, 2, 3]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [], 1, 2, 3, 4]", %q{
        ary = [0, 1, 2, 3, 4]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [1], 2, 3, 4, 5]", %q{
        ary = [0, 1, 2, 3, 4, 5]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, [1, 2], 3, 4, 5, 6]", %q{
        ary = [0, 1, 2, 3, 4, 5, 6]
        a, *b, c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[nil, nil, [], nil, nil, nil]", %q{
        ary = []
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, nil, [], nil, nil, nil]", %q{
        ary = [0]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [], nil, nil, nil]", %q{
        ary = [0, 1]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [], 2, nil, nil]", %q{
        ary = [0, 1, 2]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [], 2, 3, nil]", %q{
        ary = [0, 1, 2, 3]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [], 2, 3, 4]", %q{
        ary = [0, 1, 2, 3, 4]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [2], 3, 4, 5]", %q{
        ary = [0, 1, 2, 3, 4, 5]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }

      assert_equal "[0, 1, [2, 3], 4, 5, 6]", %q{
        ary = [0, 1, 2, 3, 4, 5, 6]
        a, b, *c, d, e, f = *ary; [a, b, c, d, e, f]
      }


#
# assert_equal 'ok', %q{
#   a,s=[],"aaa"
#   300.times { a<<s; s=s.succ }
#   eval <<-END__
#   GC.stress=true
#   Fiber.new do
#     #{ a.join(",") },*zzz=1
#   end.resume
#   END__
#   :ok
# }, '[ruby-dev:32581]'

# assert_equal 'ok', %q{
#   while true
#     *, z = 1
#     break
#   end
#   :ok
# }, '[ruby-dev:32892]'
