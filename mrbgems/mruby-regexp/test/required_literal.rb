# A run of literal bytes every match holds -- in every branch, wherever it
# stands in each -- lets a search answer "no match" by not finding the run
# at all. Where the run is there, the search is the one it always was.

assert('Regexp required literal: a backreference pattern') do
  need_backtracking_stack
  scope = /(?:(?<name1>.*)::)#<Class:\k<name1>>|#<Class:(?<name2>.*)>(?:::)?/
  assert_equal "Alpha::Beta::Foo::Bar", "Alpha::Beta::Foo::Bar".sub(scope, '\k<name1>\k<name2>.')
  assert_equal "Alpha::Beta::Foo.", "Alpha::Beta::Foo::#<Class:Alpha::Beta::Foo>".sub(scope, '\k<name1>\k<name2>.')
  assert_equal "Foo.bar", "#<Class:Foo>::bar".sub(scope, '\k<name1>\k<name2>.')
  assert_equal nil, "Alpha::Beta::Foo" =~ scope
  assert_equal nil, $~
  # the search that used to stop at the step limit answers no match
  assert_equal false, !!(("a" * 40 + "!") =~ /(a*)*b\1/)
end

assert('Regexp required literal: at a different distance in each branch') do
  assert_equal 5, "a1b2 xx:=" =~ /\d+:=|x+:=/
  assert_equal nil, "a1b2 xx=" =~ /\d+:=|x+:=/
  assert_equal ["b-"], "ab-cd".scan(/\w-|-\w/)
  assert_equal [], "abcd".scan(/\w-|-\w/)
  assert_equal ["key", "value", nil, nil], "key = value".match(/(\w+) = (\w+)|(\w+)=(\w+)/).captures
end

assert('Regexp required literal: a branch without the run') do
  assert_equal 1, "xyz" =~ /a.*=|y/
  assert_equal 1, "x=z" =~ /a=|=z/
  assert_equal 0, "abe" =~ /ab(?:c)?e/
  assert_equal 0, "ad" =~ /a(?:bc)*d/
end

assert('Regexp required literal: a run only a lookaround reads') do
  need_backtracking_stack
  assert_equal 0, "ad" =~ /a(?=d)d/
  assert_equal 2, "xay" =~ /(?<=a)y/
  assert_equal 0, "y" =~ /(?<!a)y/
  assert_equal 0, "ab" =~ /a(?!bc)b/
  assert_equal 0, "xab" =~ /(x)?(?(1)ab|ab)/
  assert_equal 0, "cd" =~ /(x)?(?(1)ab|cd)/
end

assert('Regexp required literal: the run stands at or after the start') do
  assert_equal 4, "ab=c=d".index(/=\w/, 3)
  assert_equal nil, "ab=c".index(/=\w/, 3)
  assert_equal false, "a=b".match?(/=b/, 2)
  assert_equal 4, "x=y x=z".rindex(/\w=/)
  assert_equal 0, "x=y x=z".rindex(/\w=/, 3)
  assert_equal nil, "x=y".rindex(/=y/, 0)
  assert_equal "aaa:b", "aaa:b".match(/a+:b/)[0]
  assert_equal "aa_b", "aaa:b".gsub(/a:|:b/, "_")
end

assert('Regexp required literal: characters wider than a byte') do
  skip unless __ENCODING__ == "UTF-8"
  assert_equal 1, "日本:語" =~ /.:語|語:./
  assert_equal nil, "日本語" =~ /.:語|語:./
  assert_equal ["é::"], "é::é".scan(/é::|::é/)
end

assert('Regexp required literal: repeated searches over one subject') do
  s = "a=1, b=2, c=3"
  assert_equal [["a", "1"], ["b", "2"], ["c", "3"]], s.scan(/(\w)=(\d)/)
  assert_equal "11, 22, 33", s.gsub(/\w=(\d)/) { $1 * 2 }
  assert_equal ["a=1", "b=2", "c=3"], s.split(/,\s*|;/)
  assert_equal [], "no pairs here".scan(/(\w)=(\d)/)
  assert_equal "no pairs here", "no pairs here".gsub(/\w=\d/, "")
end
