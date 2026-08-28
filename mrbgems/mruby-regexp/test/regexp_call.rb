# Subexpression calls: \g<name>, \g'name', \g<n>, \g<-n>, \g<+n>, \g<0>.
#
# A call runs a group's sub-pattern again where the call stands, recursively
# when the call stands inside the group it names. Every expectation here is
# CRuby's answer (ruby 4.0.6) for the same pattern and subject; where a
# message deliberately differs, the assertion says so.

assert("Regexp - a call runs the group's sub-pattern again") do
  need_backtracking_stack
  assert_equal "cc", "cc".match(Regexp.new("(?<a>c)\\g<a>"))[0]
  assert_equal "cc", "cc".match(Regexp.new("(?<a>c)\\g'a'"))[0]
  assert_equal "cc", "cc".match(Regexp.new("(c)\\g<1>"))[0]
  assert_equal "cc", "cc".match(Regexp.new("(c)\\g'1'"))[0]

  # the relative forms count over the groups: -n back over those already
  # opened, +n forward over those still to come
  assert_equal "cc", "cc".match(Regexp.new("(c)\\g<-1>"))[0]
  assert_equal "cc", "cc".match(Regexp.new("\\g<+1>(c)"))[0]
  assert_equal "abb", "abb".match(Regexp.new("(a)\\g<+1>(b)"))[0]

  # a call may name a group written later, as \1(a) may
  assert_equal "cc", "cc".match(Regexp.new("\\g<1>(c)"))[0]
  assert_equal "cc", "cc".match(Regexp.new("\\g<a>(?<a>c)"))[0]

  # \g<0> is the whole pattern
  assert_equal "aaa", "aaa".match(Regexp.new("a\\g<0>?"))[0]
  assert_equal "aa", "aa".match(Regexp.new("a\\g'0'?"))[0]

  # the body runs as compiled: an option scoped to it travels with it, and
  # one in force at the call site does not reach into it
  assert_equal "abAB", "abAB".match(Regexp.new("(?i:(?<a>ab))\\g<a>"))[0]
  assert_nil Regexp.new("(?<a>ab)\\g<a>").match("abAB")
  assert_equal "abAB", "abAB".match(Regexp.new("(?<a>ab)\\g<a>", "i"))[0]
end

assert("Regexp - recursion matches balanced text") do
  need_backtracking_stack
  re = Regexp.new("(?<p>\\((?:[^()]|\\g<p>)*\\))")
  md = re.match("x(a(b))y")
  assert_equal "(a(b))", md[0]
  assert_equal "(a(b))", md[:p]
  assert_equal "(a)", re.match("((a)")[0]
  assert_nil re.match("((((")
  assert_equal "(a)", re.match("))(a)((")[0]

  # a multibyte body recurses like any other
  assert_equal "ああ", "ああ".match(Regexp.new("(?<a>あ)\\g<a>"))[0]
end

assert("Regexp - the invocation that completes last names the capture") do
  need_backtracking_stack
  # a call's invocation completes after the inline one, so the capture is the
  # call's text
  md = Regexp.new("(?<a>x)\\g<a>").match("xx")
  assert_equal "x", md[:a]
  assert_equal 1, md.begin(:a)

  # a recursion's outermost invocation completes last of all
  md = Regexp.new("(?<p>\\((?:[^()]|\\g<p>)*\\))").match("(a(b))")
  assert_equal "(a(b))", md[:p]
  assert_equal 0, md.begin(:p)

  # two sibling recursions: the second completes later
  md = Regexp.new("(?<p>\\(\\g<p>?\\))\\g<p>").match("(())()")
  assert_equal "()", md[:p]
  assert_equal 4, md.begin(:p)

  # a call that does not run leaves the capture the inline occurrence wrote
  md = Regexp.new("(?<a>x)\\g<a>?y").match("xy")
  assert_equal "x", md[:a]
  assert_equal 0, md.begin(:a)

  # a plain group inside the body captures per invocation, last write winning
  md = Regexp.new("(?<p>\\((?<q>[a-z])?\\g<p>?\\))").match("((a))")
  assert_equal "a", md[:q]
  assert_equal 2, md.begin(:q)

  # \g<0>'s outermost invocation is the whole match
  md = Regexp.new("a\\g<0>?").match("aaa")
  assert_equal "aaa", md[0]
  assert_equal 0, md.begin(0)
end

assert("Regexp - what a backreference reads while an invocation is open") do
  need_backtracking_stack
  # entering an invocation invalidates the group, so \k inside the second
  # invocation does not read what the first captured: the first branch fails
  # and the second matches, as in CRuby
  md = Regexp.new("(?<a>\\k<a>y|ab)z\\g<a>").match("abzaby")
  assert_equal "abzab", md[0]
  assert_equal 3, md.begin(:a)

  # a completed inner invocation is readable inside the still-open outer one
  md = Regexp.new("(?<p>\\(\\g<p>?\\k<p>?\\))").match("(()())")
  assert_equal "(()())", md[0]

  # and after the invocation closes, \k reads what it captured
  assert_equal "xyy", "xyy".match(Regexp.new("(?<a>x|y)\\g<a>\\k<a>"))[0]
end

assert("Regexp - calls under quantifiers and copies of the body") do
  need_backtracking_stack
  # a quantified call re-enters the body per iteration
  assert_equal "xxxy", "xxxy".match(Regexp.new("(?<a>x)(?:\\g<a>)*y"))[0]
  assert_equal "xxx", "xxx".match(Regexp.new("(?<a>x)\\g<a>+"))[0]
  assert_equal "xxxx", "xxxx".match(Regexp.new("(?<a>x)\\g<a>{2,3}"))[0]

  # a body that can match empty ends the repetition rather than spinning
  assert_equal "y", "y".match(Regexp.new("(?<a>x?)(?:\\g<a>)*y"))[0]

  # a greedy repeat over the called group itself stops the same way: its
  # back edge lands on the body's rerouted entry, and the trampoline's jump
  # back must not be read as a loop of its own, which once re-entered the
  # repetition on every empty iteration until the stack limit refused it
  assert_equal "b", "b".match(Regexp.new("(?<a>|)+b\\g<a>"))[0]
  assert_equal "b", "b".match(Regexp.new("(?<a>x?)+b\\g<a>"))[0]

  # an interval copies the body; the copies stay one callable group
  assert_equal "xxx", "xxx".match(Regexp.new("(?:(?<a>x)){2}\\g<a>"))[0]
  assert_equal "(())", "(())".match(Regexp.new("(?<p>\\(\\g<p>{0,2}\\))"))[0]

  # a possessive call cuts like any possessive atom
  assert_nil Regexp.new("(?<a>x)\\g<a>*+x").match("xxx")
  assert_nil Regexp.new("(?<a>x+)(?>\\g<a>)x").match("xxx")
end

assert("Regexp - {0} erases the inline occurrence and keeps the group") do
  need_backtracking_stack
  # CRuby keeps the body callable when {0} drops its inline occurrence
  assert_equal "x", "x".match(Regexp.new("(?<a>x){0}\\g<a>"))[0]
  assert_equal "x", "x".match(Regexp.new("(?:(?<a>x)){0}\\g<a>"))[0]
end

assert("Regexp - {0} erases a call's code but not its reference checks") do
  # The reference a {0}-erased call spelled is still read against the whole
  # pattern: CRuby refuses each of these, erased or not. No call survives to
  # run, so no backtracking stack is asked for.
  assert_raise_with_message(RegexpError,
                            "undefined name <nope> reference: /(?:\\g<nope>){0}/") do
    Regexp.new("(?:\\g<nope>){0}")
  end
  assert_raise_with_message(RegexpError,
                            "undefined group <9> reference: /(?:\\g<+9>){0}/") do
    Regexp.new("(?:\\g<+9>){0}")
  end
  assert_raise_with_message(RegexpError,
                            "undefined group <2> reference: /(?:\\g<2>){0}(x)/") do
    Regexp.new("(?:\\g<2>){0}(x)")
  end
  assert_raise_with_message(RegexpError,
                            "multiplex definition name <a> call: /(?:\\g<a>){0}(?<a>x)(?<a>y)/") do
    Regexp.new("(?:\\g<a>){0}(?<a>x)(?<a>y)")
  end

  # a reference the pattern does define stays fine erased
  assert_equal "x", "x".match(Regexp.new("(?:\\g<a>){0}(?<a>x)"))[0]
end

assert("Regexp - {0} keeps a group \\k reads as never matched") do
  # No call machinery runs here, so no stack is asked for: the erased group
  # never matches, and a backreference to it fails, as in CRuby.
  assert_nil Regexp.new("(?<a>x){0}\\k<a>y").match("y")
end

assert("Regexp - calls and lookarounds") do
  need_backtracking_stack
  # a call inside a lookaround runs there
  assert_equal "xx", "xx".match(Regexp.new("(?<a>x)(?=\\g<a>)x"))[0]
  assert_equal "x", "xx".match(Regexp.new("(?<a>x)(?<=\\g<a>)"))[0]
  assert_equal "x", "xy".match(Regexp.new("(?<a>x)(?!\\g<a>)"))[0]

  # a group defined inside a lookaround is callable from outside it
  assert_equal "x", "xx".match(Regexp.new("(?=(?<a>x))\\g<a>"))[0]
  assert_equal "y", "yq".match(Regexp.new("(?!(?<a>y)z)\\g<a>"))[0]

  # an anchor inside the body asserts against the real position
  assert_equal "xy", "xy".match(Regexp.new("(?<a>^x)y\\g<a>?"))[0]
end

assert("Regexp - a call in a lookbehind must measure fixed") do
  need_backtracking_stack
  assert_equal "xy", "xy".match(Regexp.new("(?<a>xy)(?<=\\g<a>)"))[0]

  # a variable-length or recursive body does not measure, and is refused as
  # every unfixed lookbehind is, message and all as in CRuby
  msg = "invalid pattern in look-behind"
  assert_raise_with_message(RegexpError, "#{msg}: /(?<a>x+)(?<=\\g<a>)/") do
    Regexp.new("(?<a>x+)(?<=\\g<a>)")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(?<p>\\(\\g<p>?\\))(?<=\\g<p>)/") do
    Regexp.new("(?<p>\\(\\g<p>?\\))(?<=\\g<p>)")
  end
end

assert("Regexp - the group numbering calls use") do
  need_backtracking_stack
  # a pattern that names a group demotes the plain ones from capturing, and
  # the numbered call spellings are refused with them, \g<0> excepted
  assert_equal "xyx", "xyx".match(Regexp.new("(?<a>x)(y)\\g<a>"))[0]
  assert_equal "xyxy", "xyxy".match(Regexp.new("(?<a>x)y\\g<0>?"))[0]
  msg = "numbered backref/call is not allowed. (use name)"
  assert_raise_with_message(RegexpError, "#{msg}: /(?<a>x)(y)\\g<1>/") do
    Regexp.new("(?<a>x)(y)\\g<1>")
  end
  ["(?<a>x)\\g<-1>", "(?<a>x)\\g<+1>(y)"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # a call does not open a group, so the numbers after it are unmoved
  assert_equal "xxyy", "xxyy".match(Regexp.new("(x)\\g<1>(y)\\2"))[0]

  # a call's name is read whole, signs included: a group whose name holds one
  # is reachable by \g where \k's level cut puts it out of reach
  assert_equal "xx", "xx".match(Regexp.new("(?<a-1>x)\\g<a-1>"))[0]
end

assert("Regexp - the references a call refuses") do
  # every one of these is the parser or the resolver answering, so no
  # backtracking stack is asked for
  assert_raise_with_message(RegexpError, "undefined group <3> reference: /(a)\\g<3>/") do
    Regexp.new("(a)\\g<3>")
  end
  assert_raise_with_message(RegexpError, "undefined group <5> reference: /(a)\\g<+5>/") do
    Regexp.new("(a)\\g<+5>")
  end
  assert_raise_with_message(RegexpError, "undefined name <b> reference: /(?<a>x)\\g<b>/") do
    Regexp.new("(?<a>x)\\g<b>")
  end
  assert_raise_with_message(RegexpError, "invalid backref number/name: /(a)\\g<-3>/") do
    Regexp.new("(a)\\g<-3>")
  end
  assert_raise_with_message(RegexpError, "group name is empty: /(a)\\g<>/") do
    Regexp.new("(a)\\g<>")
  end
  assert_raise_with_message(RegexpError, "too big number: /(a)\\g<99999999999999999999>/") do
    Regexp.new("(a)\\g<99999999999999999999>")
  end

  # a number holding another character is CRuby's `invalid char in group
  # name`, quoted as read where CRuby quotes to the end of the pattern
  assert_raise_with_message(RegexpError, "invalid char in group name <1x>: /(a)\\g<1x>/") do
    Regexp.new("(a)\\g<1x>")
  end

  # a signed zero names no group; CRuby quotes -0 as written and +0 without
  # its sign
  assert_raise_with_message(RegexpError, "invalid group name <-0>: /(a)\\g<-0>/") do
    Regexp.new("(a)\\g<-0>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)\\g<+0>/") do
    Regexp.new("(a)\\g<+0>")
  end

  # a name two groups carry is refused where a call reads it, and only there
  assert_raise_with_message(RegexpError,
                            "multiplex definition name <a> call: /(?<a>x)(?<a>y)\\g<a>/") do
    Regexp.new("(?<a>x)(?<a>y)\\g<a>")
  end
  assert_equal "xy", "xy".match(Regexp.new("(?<a>x)(?<a>y)"))[0]

  # the name scan is every other one's: a ')' past the first byte ends it
  # with the name quoted to the end of the pattern, `>` included, which is
  # CRuby's quote for a scan its delimiter never closed, and the end of the
  # pattern ends the name the same way, quoted as read as the \k arm quotes
  # one
  assert_raise_with_message(RegexpError,
                            "invalid group name <a)b>>: /(?<a>x)\\g<a)b>/") do
    Regexp.new("(?<a>x)\\g<a)b>")
  end
  assert_raise_with_message(RegexpError,
                            "invalid group name <a>: /(?<a>x)\\g<a/") do
    Regexp.new("(?<a>x)\\g<a")
  end
end

assert("Regexp - a recursion no input could end is refused") do
  # the check is CRuby's, message and line: a call cycle reachable with
  # nothing consumed, or a group no invocation completes without re-entering
  msg = "never ending recursion"
  assert_raise_with_message(RegexpError, "#{msg}: /(?<a>\\g<a>)/") do
    Regexp.new("(?<a>\\g<a>)")
  end
  ["(?<a>x\\g<a>)",             # must recurse to match at all
   "(?<a>x|\\g<a>)",            # reachable with nothing consumed
   "(?<a>\\g<a>?x)",            # optional, but still at the head
   "(?<a>x\\g<a>{1,2})",        # a lower bound of one is mandatory
   "(?:(?<a>x\\g<a>))?",        # optional outside the group saves nothing
   "(?<a>(?:\\g<a>)*)",         # the loop re-enters at the position it left
   "(?<a>(?<b>\\g<a>)?)",       # through a nested group, still at the head
   "(?<a>(?=\\g<a>)x)",         # a lookaround's sub-pattern runs at its head
   "(?<a>(?!\\g<a>)x)",
   "(?<a>x(?=\\g<a>))",         # and a positive one is not an alternative
   "(?<a>x(?!\\g<a>))",
   "(?<a>(?>\\g<a>))",
   "(?<e>z)(?<a>\\g<e>\\g<a>)", # every path recurses, \g<e> escaping nothing
   "(?<b>x?)(?<a>\\k<b>\\g<a>)", # a backreference's minimum is zero
   "(?<a>^\\g<a>)",
   "(?<a>\\g<b>)(?<b>\\g<a>)",  # mutual recursion counts the same
   "\\g<0>",
   "x(?=\\g<0>)",
   "(?<a>x)\\g<0>"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end
end

assert("Regexp - the recursions an input can end compile") do
  need_backtracking_stack
  # a call every path can decline escapes both refusals
  assert_equal "xxx", "xxx".match(Regexp.new("(?<a>x\\g<a>?)"))[0]
  assert_equal "xxx", "xxx".match(Regexp.new("(?<a>x\\g<a>*)"))[0]
  assert_equal "x", "xx".match(Regexp.new("(?<a>x\\g<a>??)"))[0]
  assert_equal "xxy", "xxy".match(Regexp.new("(?<a>x\\g<a>|y)"))[0]
  assert_equal "xx", "xx".match(Regexp.new("(?<a>(?:x\\g<a>)?)"))[0]
  assert_equal "xyxy", "xyxy".match(Regexp.new("(?<a>x\\g<b>?)(?<b>y\\g<a>?)"))[0]
  assert_equal "xx", "xx".match(Regexp.new("x\\g<0>*"))[0]
  assert_equal "xy", "xy".match(Regexp.new("(?<a>x(?:(?=\\g<a>)|y))"))[0]
  assert_equal "xx", "xx".match(Regexp.new("(?<a>(?>x)\\g<a>?)"))[0]

  # mutual recursion is admitted from whichever body stands on its own: b's
  # can stop at its `x`, and that is what a's one call steps through, where
  # walling off every group a call chain leads back from would have left a's
  # body with nowhere to go
  assert_equal "xx", "xx".match(Regexp.new("(?<a>\\g<b>)(?<b>x\\g<a>?)"))[0]
  assert_equal "xxx",
               "xxx".match(Regexp.new("(?<a>\\g<b>){0}(?<b>x(?:\\g<a>)?){0}\\g<a>"))[0]
  assert_equal "xxx",
               "xxx".match(Regexp.new("(?<a>\\g<b>){0}(?<b>\\g<c>){0}(?<c>x\\g<a>?){0}\\g<a>"))[0]
  assert_equal "yx",
               "yxyx".match(Regexp.new("(?<a>\\g<b>x){0}(?<b>y\\g<a>?){0}\\g<a>"))[0]

  # a call on the way to a recursion prices in its body's minimum, so a call
  # to a group that always consumes keeps what follows off the head:
  # \g<0> here stands behind a's `b` and compiles, where a call to an
  # empty-bodied group leaves the head reachable and is refused above
  assert_nil Regexp.new("(?<a>b\\g<a>??)$\\g<0>?").match("bbaa")
  assert_equal "b", "b".match(Regexp.new("(?<a>b\\g<a>??)\\g<0>?"))[0]
end

assert("Regexp - recursion depth is bounded by the stack limit") do
  # every call frame is an entry on the backtracking stack, so a recursion
  # deeper than MRB_REGEXP_STACK_LIMIT admits stops at the limit rather than
  # running away; the test sizes its subject from the limit, and steps aside
  # where that subject would be unreasonable to build
  skip "stack limit too high to exercise" if Regexp::STACK_LIMIT > 100_000
  depth = Regexp::STACK_LIMIT + 1
  subject = ("(" * depth) + (")" * depth)
  assert_raise(RegexpError) do
    Regexp.new("(?<p>\\(\\g<p>?\\))").match(subject)
  end
end
