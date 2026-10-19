# The groups a match answers with, over subjects long enough that the
# searches hold many threads, or many backtracking entries, for many
# positions: what each thread captured stays its own however the state that
# holds it is laid out.

assert('Regexp capture slots: leftmost-first among threads') do
  assert_equal ["abcd", "a", "bcd", ""], "abcd".match(/(a|ab)(c|bcd)(d*)/).to_a
  assert_equal ["abcd", "ab", "c", "d"], "abcd".match(/(ab|a)(c|bcd)(d*)/).to_a
  assert_equal ["xyz", "x", "y", nil, "z"], "xyz".match(/(x)(y)?(q)?(z)/).to_a
end

assert('Regexp capture slots: a long run of iterations') do
  s = ("ab" * 150) + "c"
  m = s.match(/((a)|(b))+c/)
  assert_equal 0, m.begin(0)
  assert_equal "b", m[1]
  assert_equal "a", m[2]
  assert_equal "b", m[3]
  m = s.match(/(?:(a)(b))*(c)/)
  assert_equal 0, m.begin(0)
  assert_equal ["a", "b", "c"], m.captures
  assert_equal ["a", "bc"], s.match(/(a|ab|b)*?(bc)/).captures
end

assert('Regexp capture slots: many threads alive at once') do
  t = ("a" * 120) + "b"
  assert_equal ["a", "b"], t.match(/(a|aa|aaa)*(b)/).captures
  assert_equal [117, 3], t.match(/(a+)(a{3})b/).captures.map(&:size)
  assert_equal [[50, 70, 1]], t.scan(/(a{50})(a*)(b)/).map { |g| g.map(&:size) }
end

assert('Regexp capture slots: more groups than the inline pool') do
  g17 = /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)/
  u = ("z" * 100) + "abcdefghijklmnopq" + ("z" * 100)
  assert_equal "abcdefghijklmnopq", u.match(g17).captures.join
  assert_equal 100, u.match(g17).begin(0)
  assert_equal 120, u.sub(/(z+)(a)(b)/) { "#{$1.size}#{$2}#{$3}" }.size
  v = (1..200).map { |i| "k#{i}=v#{i}" }.join(";")
  w = v.gsub(/k(\d+)=v(\d+)/) { $1 == $2 ? "ok" : "ng" }
  assert_equal [], w.split(";").reject { |e| e == "ok" }
  assert_equal 599, w.size
end

assert('Regexp capture slots: the backtracker holding many entries') do
  need_backtracking_stack
  x = ("ab" * 100) + "ab"
  assert_equal [1, 199], x.match(/(\w)(.*)\1/).captures.map(&:size)
  assert_equal 202, x.match(/(ab)(?:\1)+$/)[0].size
  y = ("x" * 80) + "q" + ("x" * 80)
  assert_equal 80, y.match(/(x+)q\1/)[1].size
  assert_equal [79, 1], y.match(/(x*)(x+)q\2/).captures.map(&:size)
end
