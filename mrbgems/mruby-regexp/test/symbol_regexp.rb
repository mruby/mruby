assert("Symbol#match") do
  md = :"hello world".match(Regexp.new("(\\w+)\\s(\\w+)"))
  assert_equal "hello", md[1]
  assert_equal "world", md[2]
  assert_equal "hello world", md.string

  # a String pattern is compiled, as it is for String#match
  assert_equal "ll", :hello.match("l+")[0]

  # pos is honoured, and counted in the symbol's name
  assert_equal 3, :hello.match(Regexp.new("l"), 3).begin(0)
  assert_nil :hello.match(Regexp.new("l"), 4)

  # a negative pos counts back from the end of the name
  assert_equal 3, :hello.match(Regexp.new("l"), -2).begin(0)
  assert_nil :hello.match(Regexp.new("l"), -100)

  assert_nil :hello.match(Regexp.new("z"))
end

assert("Symbol#match - empty symbol") do
  assert_equal 0, :"".match(Regexp.new("")).begin(0)
  assert_nil :"".match(Regexp.new("a"))
  assert_true :"".match?(Regexp.new(""))
  assert_false :"".match?(Regexp.new("a"))
end

assert("Symbol#match - block") do
  assert_equal "LL", :hello.match("l+") { |md| md[0].upcase }
  assert_equal :broke, :hello.match("l+") { break :broke }

  called = false
  assert_nil(:hello.match(Regexp.new("z")) { called = true })
  assert_false called
end

assert("Symbol#match sets the match globals") do
  assert_equal "ll", :hello.match("l+") { $~[0] }
  :hello.match("l+")
  assert_equal "ll", $~[0]
  assert_equal "ll", Regexp.last_match(0)
end

assert("Symbol#match?") do
  assert_true :hello.match?(Regexp.new("l+"))
  assert_true :hello.match?("l+")
  assert_false :hello.match?(Regexp.new("z"))

  # pos is honoured, and counted in the symbol's name
  assert_true :hello.match?(Regexp.new("l"), 3)
  assert_false :hello.match?(Regexp.new("l"), 4)
  assert_true :hello.match?(Regexp.new("l"), -2)
  assert_false :hello.match?(Regexp.new("l"), -100)
end

assert("Symbol#match? - does not update last match") do
  $~ = :matched.match("matched")
  assert_true :hello.match?("l+")
  assert_equal "matched", $~[0]
  assert_false :hello.match?(Regexp.new("z"))
  assert_equal "matched", $~[0]
end

assert("Symbol#=~") do
  assert_equal 2, :hello =~ Regexp.new("l")
  assert_nil :hello =~ Regexp.new("z")

  # inherited from String#=~: a String argument is a type mismatch, and any
  # other argument is answered by its own `=~`
  assert_raise(TypeError) { :hello =~ "l" }
  assert_nil :hello =~ nil
end

assert("Symbol#=~ sets the match globals") do
  :hello =~ Regexp.new("l(l)")
  assert_equal "ll", $~[0]
  assert_equal "l", $1
  assert_equal "l", Regexp.last_match(1)

  assert_nil :hello =~ Regexp.new("z")
  assert_nil $~
end

assert("Symbol#!~") do
  assert_false :hello !~ Regexp.new("l")
  assert_true :hello !~ Regexp.new("z")
  assert_raise(TypeError) { :hello !~ "l" }
end

assert("Symbol - multibyte (UTF-8) name") do
  # pos counts characters of the symbol's name, and begin/end report
  # character offsets, just as they do for the equivalent String.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "い", :"あいう".match(Regexp.new("い"))[0]
  assert_equal 2, :"あいあ".match(Regexp.new("あ"), 1).begin(0)
  assert_equal 2, :"あいあ".match(Regexp.new("あ"), -1).begin(0)
  assert_nil :"あいあ".match(Regexp.new("い"), 2)
  assert_true :"あいあ".match?(Regexp.new("あ"), 2)
  assert_false :"あいあ".match?(Regexp.new("い"), 2)
  assert_equal 1, :"あい" =~ Regexp.new("い")
end
