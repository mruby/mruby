assert("Regexp - character class") do
  re = Regexp.new("[a-z]+")
  md = re.match("123abc456")
  assert_equal "abc", md[0]
end

assert("Regexp - a class the pattern ends inside says which it was") do
  # A class no ']' closes is `premature end of char-class` in CRuby, whatever
  # stands unfinished inside it: a member, a range, or a POSIX bracket that
  # ends early enough to leave the class open as well.
  ["[", "[a", "[a-", "[^a", "[[:alpha", "[[:alpha:", "[[:alpha:]",
   "[a[:alpha:]", "[[:word:]x"].each do |src|
    assert_raise_with_message(RegexpError, "premature end of char-class: /#{src}/", src) do
      Regexp.new(src)
    end
  end
end

assert("Regexp - reversed character class range") do
  # A range written backwards holds nothing. It used to compile to a class
  # that silently lacked the span, or in the negated form admitted every
  # character; CRuby raises for either.
  assert_raise_with_message(RegexpError, "empty range in char class: /[b-a]/") do
    Regexp.new("[b-a]")
  end
  assert_raise_with_message(RegexpError, "empty range in char class: /[^b-a]/") do
    Regexp.new("[^b-a]")
  end
  assert_raise(RegexpError) { Regexp.new("[xz-ay]") }
  # a range of one is not empty
  assert_equal ["a"], "abc".scan(/[a-a]/)
  # a '-' at either edge is a member, not a range
  assert_equal ["-", "a"], "-ab".scan(/[-a]/)
  assert_equal ["a", "-"], "abc-".scan(/[a-]/)
end

assert("Regexp - POSIX bracket classes") do
  # ASCII semantics, like this gem's \w/\d shorthands.
  assert_equal "abc", "123abc456".match(/[[:alpha:]]+/)[0]
  assert_equal "123", "123abc".match(/[[:digit:]]+/)[0]
  assert_equal "abc123", "abc123!".match(/[[:alnum:]]+/)[0]
  assert_equal "deadBEEF", "deadBEEF".match(/[[:xdigit:]]+/)[0]
  assert_equal "snake_case", "snake_case".match(/[[:word:]]+/)[0]
  assert_equal "!", "ab!cd".match(/[[:punct:]]/)[0]
  assert_equal "AB", "abAB".match(/[[:upper:]]+/)[0]
  # /i makes the two letter-case classes equivalent.
  assert_equal "abAB", "abAB".match(/[[:upper:]]+/i)[0]
  assert_equal "abAB", "abAB".match(/[[:lower:]]+/i)[0]
  # combine with literals and other classes
  assert_equal "a1", "a1-".match(/[a[:digit:]]+/)[0]
  assert_equal "ab12", "ab12 ".match(/[[:alpha:][:digit:]]+/)[0]
  # negated forms
  assert_equal "abc", "abc123".match(/[[:^digit:]]+/)[0]
  assert_equal "x", " x".match(/[^[:space:]]/)[0]
  # an unknown class name is an error, named as CRuby names it
  assert_raise_with_message(RegexpError,
                            "invalid POSIX bracket type: /[[:bogus:]]/") do
    Regexp.new("[[:bogus:]]")
  end
  assert_raise_with_message(RegexpError,
                            "invalid POSIX bracket type: /[[:^bogus:]]/") do
    Regexp.new("[[:^bogus:]]")
  end
  # The name length used to be truncated with a (uint16_t) cast, so a name
  # 65536 bytes longer than "alpha" compared equal to "alpha" and compiled
  # as [[:alpha:]] instead of raising.
  long = "alpha" + "A" * 65536
  assert_raise(RegexpError) { Regexp.new("[[:#{long}:]]") }
  assert_raise(RegexpError) { Regexp.new("[[:^#{long}:]]") }
end

assert("Regexp - \\b inside character class is backspace") do
  # Outside [...], \b is the word boundary assertion; inside [...]
  # it must mean U+0008 (backspace), matching MRI/Onigmo.
  assert_equal "Ruby", "Ruby".gsub(/[\b]/, "X")
  assert_equal "aXc", "a\bc".gsub(/[\b]/, "X")
  assert_equal ["\b", "\t", "\n"], "ABC\b\t\n".scan(/[\b-\n]/)
end

assert("Regexp - dot") do
  re = Regexp.new("a.c")
  assert_true re.match?("abc")
  assert_true re.match?("axc")
  assert_false re.match?("ac")
end

assert("Regexp - alternation") do
  re = Regexp.new("cat|dog")
  assert_equal "cat", re.match("I have a cat")[0]
  assert_equal "dog", re.match("I have a dog")[0]
end

assert("Regexp - alternation is leftmost-first") do
  # Ruby tries alternatives left to right and keeps the first that lets the
  # whole pattern match -- not the longest. The linear-time engine used to
  # pick the longest branch instead.
  assert_equal "a", "ab".match(/a|ab/)[0]
  assert_equal "foo", "foobar".match(/foo|foobar/)[0]
  assert_equal "ab", "ab".match(/ab|a/)[0]
  assert_equal ["abc", "ab", "c"], "abcd".match(/(ab|abc)(c|cd)/).to_a
  assert_equal "aa", "aaa".match(/aa|a/)[0]
  # three or more branches keep source order, not just the first two
  assert_equal "car", "cart".match(/cat|car|cart/)[0]
  assert_equal "cart", "cart".match(/cat|cart|car/)[0]
  assert_equal "a", "abc".match(/a|ab|abc/)[0]
  assert_equal "abc", "abc".match(/abc|ab|a/)[0]
  # greedy quantifiers stay longest-match
  assert_equal "aaa", "aaa".match(/a+/)[0]
end

assert("Regexp - quantifiers") do
  assert_equal "aaa", Regexp.new("a+").match("aaa")[0]
  assert_equal "", Regexp.new("a*").match("bbb")[0]
  assert_equal "ab", Regexp.new("ab?").match("ab")[0]
  assert_equal "a", Regexp.new("ab?").match("ac")[0]
end

assert("Regexp - a repetition stops on an empty iteration") do
  # A repetition whose body matches empty runs that iteration and then stops,
  # so a body that prefers the empty branch ends the loop at once instead of
  # going around again on the branch that consumes.
  assert_equal "", "a".match(/(|a)*/)[0]
  assert_equal "", "aaa".match(/(|a)*/)[0]
  assert_equal "", "a".match(/(?:|a)+/)[0]
  # a body that can only match empty after consuming still consumes first
  assert_equal "aaa", "aaa".match(/(a|)*/)[0]
  assert_equal "aa", "aab".match(/(a?)*/)[0]
end

assert("Regexp - a repetition keeps its last, empty iteration's capture") do
  # The final iteration is the empty one, and the group keeps what it
  # captured: the empty string where the loop stopped. The linear-time engine
  # used to drop that iteration and report the previous one's text, or nil
  # when there was no previous one.
  md = "a".match(/(a?)*/)
  assert_equal "a", md[0]
  assert_equal "", md[1]
  assert_equal 1, md.begin(1)
  assert_equal "", "aab".match(/(a*)*b/)[1]
  assert_equal "", "a".match(/(a|)*/)[1]
  assert_equal "", "a".match(/(a?)+/)[1]
  # with no earlier iteration the group still participates
  assert_equal "", "b".match(/(a?)*/)[1]
  assert_equal "", "".match(/(a?)*/)[1]
  assert_equal "", "b".match(/(a*)*b/)[1]
  # a nullable body nested in a repetition reaches the same answer
  assert_equal "", "a".match(/((a?)*)*/)[1]
end

assert("Regexp - both engines keep the last, empty iteration's capture") do
  need_backtracking_stack
  # The same patterns with a lookaround in front, which routes them to the
  # other engine: what the two answer must not differ.
  assert_equal "", "a".match(/(?=a)(a?)*/)[1]
  assert_equal 1, "a".match(/(?=a)(a?)*/).begin(1)
  assert_equal "", "b".match(/(?=b)(a?)*/)[1]
end

assert("Regexp - a repetition whose body always consumes is unaffected") do
  assert_equal "b", "ab".match(/(a|b)*/)[1]
  assert_nil "c".match(/(a|b)*/)[1]
  assert_equal "aa", "aa".match(/(a)*/)[0]
  assert_equal "a", "aa".match(/(a)*/)[1]
  assert_equal ["", "b", ""], "ab".split(/(?:a?)*/, -1)
end

assert("String#split and String#scan see the empty iteration's capture") do
  assert_equal ["", "", "b", "", ""], "ab".split(/(a?)*/, -1)
  assert_equal [[""], [""], [""]], "ab".scan(/(a?)*/)
end

assert("Regexp - the backtracking engine stops a repetition on an empty iteration") do
  need_backtracking_stack
  # The same rule under the engine a lazy quantifier routes a pattern to. It
  # used to run such a loop until its recursion limit refused the next frame,
  # and answer with whatever the alternatives left inside the limit produced:
  # nothing here, since the outer loop went round again in the frame at the
  # limit until the step budget was gone.
  assert_equal 0, /(?:(?:b*)+)+?/ =~ ""
  assert_equal 0, /(?:(?:b*)+)+?/ =~ "b"
  assert_equal 0, /(?:(?:b?)+)+?/ =~ ""
  assert_equal 0, /(?:(?:b*)+)+?/ =~ "abcdefghij"
  # The loop stops with its lazy body still empty, rather than unwinding into
  # the body's other branches once the limit is reached.
  assert_equal "ca", /ca(?:b??b??)+a*?/.match("cab")[0]
  assert_equal "aa", /(?:a?)*b??/.match("aab")[0]
  assert_equal "aa", /(?:a?){2,}b??/.match("aab")[0]
  assert_equal "", /(?:a??)+/.match("aa")[0]
  assert_equal "a", /(?:a?|b)+c??/.match("ababab")[0]
  # Each layout of a repetition stops on its own, greedy or lazy: e* jumps
  # back to its head, e+ forks at the end of its body, e{n,} is copies of the
  # body before an e*.
  assert_equal "aab", /(?:a?)*?b/.match("aab")[0]
  assert_equal "aab", /(?:a??)+b/.match("aab")[0]
  assert_equal "aab", /(?:a??)+?b/.match("aab")[0]
  assert_equal "aab", /(?:a?){2,}?b/.match("aab")[0]
  assert_equal "aab", /(?:(?:a?)*)*?b/.match("aab")[0]
  assert_equal "", /(?:(?:a?)*?)*b??/.match("aab")[0]
  assert_equal "aab", /(?:(?:a??)+?)+?b/.match("aab")[0]
  # The fork closing e+? stops there too. It takes what follows the loop able
  # to match empty as well to tell: where the loop's exit leads straight to a
  # byte that has to match, the frame that went round again fails anyway.
  assert_equal "b", /(?:[ab]?)+?b*b/.match("ba")[0]
  assert_equal "b", /(?:(?:[ab]?)+?b*)b/.match("ba")[0]
  assert_equal "aa", /(?:(?:a?|a)+?a+){2,}/.match("aa")[0]
  # The empty iteration's capture is kept, as under the linear-time engine.
  md = /(a?)*b??/.match("a")
  assert_equal "", md[1]
  assert_equal 1, md.begin(1)
  assert_equal 1, /(a??)+b/.match("ab").begin(1)
  assert_equal 2, /(a*?)*b/.match("aab").begin(1)
  assert_equal 1, /(a*?)*?b/.match("aab").begin(1)
  assert_equal "", /((c*)*)a??/.match("c")[2]
end

assert("Regexp - a repetition of a lookaround or a backreference stops the same way") do
  need_backtracking_stack
  # A lookaround is zero-width, and a backreference to a group that
  # captured empty consumes nothing, so a repetition of either can run an
  # empty iteration and stops on it. It used to run to the recursion limit,
  # where the frame at the limit could not open the branch of a `?` that
  # follows and answered with the branch that skips it.
  assert_equal 0, /(?=)+/ =~ "a"
  assert_equal "a", /(?=a)+a/.match("a")[0]
  assert_equal "b", /(?:(?!a))*b?/.match("b")[0]
  assert_equal "ab", /a(?:(?<=a))*b?/.match("ab")[0]
  assert_equal 1, /(?:(?<!x))+b/ =~ "ab"
  assert_equal "aab", /(?:a|(?!b))+?b/.match("aab")[0]
  assert_equal "a", /(?>(?:(?=a))*)a/.match("a")[0]
  assert_equal "aa", /(a*)\1*/.match("aa")[0]
  assert_equal "b", /(a?)\1*b/.match("b")[0]
  assert_equal "aab", /(?:(a)|\1)*b/.match("aab")[0]
  assert_equal 1, /(?:(?=(a))\1?)*?b/.match("aab").begin(1)
  # An iteration's record is undone when the iteration is backtracked out
  # of, so an alternative of the earlier iteration that ends at the same
  # position is not taken for an empty one: the loop goes round again from
  # there, and the backreference sees what that alternative captured.
  md = /(?:(a)b|a(b)|\2)*?c/.match("abbc")
  assert_equal "abbc", md[0]
  assert_equal "b", md[2]
  assert_equal "abbc", /(?:(a)b|a(b)|\2)+c/.match("abbc")[0]
end

assert("Regexp - a backreference reads no group the running iteration reopened") do
  need_backtracking_stack
  # A group's pair in captures[] is a span only while the group is closed:
  # its end slot used to keep the previous iteration's end after a repetition
  # re-entered the group, so a backreference read the running iteration's
  # start against it: an empty span where the two coincided, and a negative
  # one where the start was past it. CRuby reads a group that has not closed
  # as one that has not matched, so the backreference fails and the branch
  # holding it is not taken.
  assert_equal ["x", "x", "x"], /((x|\2b))*/.match("xb").to_a  # was ["xb", "b", "b"]
  assert_equal ["a", "a", "a"], /((\2|a))*/.match("a").to_a    # was ["a", "", ""]
  # The negative span left a MatchData whose capture sat outside the match,
  # and where the repetition had no upper bound it walked the position back
  # as far as the iteration had come, so the loop made no progress and the
  # search died at the recursion limit.
  md = /(?:(.)(\2?)){2}/.match("ab")                # answered ["a", "b", nil]
  assert_equal ["ab", "b", ""], md.to_a
  assert_equal [0, 2, 1, 2, 2, 2],
               [md.begin(0), md.end(0), md.begin(1), md.end(1), md.begin(2), md.end(2)]
  assert_equal ["xx", ""], /(?:x(\1?))+/.match("xx").to_a          # raised at the limit
  assert_equal ["abc", "c", ""], /(?:(.)(\2?))+/.match("abc").to_a # raised at the limit
  assert_equal ["ab", "b", ""], /(?>(.)(\2?))+/.match("ab").to_a   # raised at the limit
  assert_equal ["xX", ""], /(?:x(\1?))+/i.match("xX").to_a
  # With text between the group's start and the backreference the two sides
  # of the negative span part company, and the compare read past the subject
  # (a negative length as memcmp's size).
  assert_equal ["xyxy", "y"], /(?:x(y\1?))+/.match("xyxy").to_a
  assert_equal ["xx", "x"], /(?<g>x(\k<g>?))+/.match("xx").to_a
  assert_equal ["xxx", ""], /(?:x(\1{0,2}))+/.match("xxx").to_a
  # A group that never closes is a group that never matches, so a pattern
  # whose only path runs the backreference inside its own group has no match
  # at all, not an empty one.
  assert_nil /(\1)/.match("aa")
  assert_nil /(a\1)/.match("aa")
  assert_equal ["", nil, nil], /((\1))*/.match("a").to_a
  # What the guard now refuses is the open group alone: a closed group that
  # captured empty still reads, and one closed by an earlier iteration still
  # reads from a branch that does not reopen it.
  assert_equal ["", ""], /()\1/.match("x").to_a
  assert_equal ["y", ""], /(x?)y\1/.match("y").to_a
  assert_equal ["aba", "a"], /(?:(a)|b\1)+/.match("aba").to_a
  # Backtracking out of a reopened group restores the pair, so the failed
  # iteration leaves the previous capture readable rather than half its own.
  assert_equal ["x", "", "x"], /((x|\2b)?)*/.match("xb").to_a
end

assert("Regexp - a pattern nested past the parse depth limit raises") do
  # The parser recurses once per nesting level, so a deep enough pattern used
  # to reach the end of the C stack, which is a crash and not an error:
  # `(?:` x 50000 was a SIGSEGV. MRB_REGEXP_PARSE_DEPTH_LIMIT bounds it, and
  # CRuby's message for the same refusal is the one raised here.
  #
  # A nesting the guard must not refuse: the count has to leave a pattern
  # inside the limit alone, and a deep one it accepts has to still match. The
  # depth comes from the build's own limit, since a build may set one below
  # any figure written here.
  limit = Regexp::PARSE_DEPTH_LIMIT
  inside = limit > 200 ? 200 : limit - 1
  if inside > 0
    assert_equal 0, (Regexp.new("(?:" * inside + "a" + ")" * inside) =~ "a")
    assert_equal 0, (Regexp.new("(?i)" * inside + "a") =~ "A")
  end

  # The refusal itself is sized from `Regexp::PARSE_DEPTH_LIMIT`, which reads
  # back what the build set. Reaching it costs the stack the limit stands for:
  # the count is checked at the bottom of the recursion, so a pattern past the
  # limit descends to it before being refused, and at the CRuby-exact default
  # that is some 2.4 MiB -- more C stack than a Windows thread has at all, and
  # the very thing the limit exists to keep a pattern from spending. So a
  # build whose limit stands above what a test may descend is left out rather
  # than crashed, and the refusal is covered by a build that sets a limit a
  # test can reach: the `ascii-ctype` build of build_config/ci/gcc-clang.rb
  # sets 512, which runs on every entry of the CI matrix. The arithmetic that
  # refuses is the same at any limit.
  if limit > 512
    skip "reaching this build's parse depth limit costs more C stack than a test may spend"
  end

  # Every construct that opens a level is asked, an inline toggle among them:
  # it encloses the rest of the group it stands in, so it is a level of its
  # own, and CRuby counts it as one too.
  fits = limit - 1
  assert_equal 0, (Regexp.new("(?:" * fits + "a" + ")" * fits) =~ "a")
  assert_equal 0, (Regexp.new("(?i)" * fits + "a") =~ "A")

  [
    ["(?:" * limit + "a" + ")" * limit, "a plain group"],
    ["(?i)" * limit + "a",              "an option toggle"],
    ["(?i:" * limit + "a" + ")" * limit, "a scoped option group"],
    ["(?=" * limit + "a" + ")" * limit, "a lookahead"],
    ["(?>" * limit + "a" + ")" * limit, "an atomic group"],
  ].each do |pattern, what|
    assert_raise_with_message(RegexpError,
                              "parse depth limit over: /#{pattern}/", what) do
      Regexp.new(pattern)
    end
  end
end

assert("Regexp - the backtracking engine raises at a limit rather than answer short") do
  need_backtracking_stack
  # A search gives up at MRB_REGEXP_STACK_LIMIT or MRB_REGEXP_STEP_LIMIT,
  # and what was running above it used to read that as the branch having
  # failed and go on with its other branches: the search answered with a
  # shorter match, a later one or none, told from the real answer by nothing.
  # A limit is the search's answer now, and the caller raises on it; the same
  # patterns match whole on a subject inside the limits, as in CRuby.
  #
  # The subjects and the patterns here are sized from the stack limit, which
  # the build sets and `Regexp::STACK_LIMIT` reads back. What a pattern holds
  # per iteration differs (a repetition holds at least the branch it has not
  # taken, a capture or a group's opener costs on top of that), so the run
  # that is past the limit is a multiple of it and the run that fits is a
  # fraction, rather than either being the limit itself.
  #
  # A build may set the limit where neither is sized any more. Filling the
  # stack costs a handful of steps an entry, so a limit high enough puts
  # itself out of the step limit's reach and these searches stop at the step
  # limit instead; and a chain sized from it is more pattern than one may
  # spell. Such a build is left out rather than have the assertions read the
  # wrong limit.
  if Regexp::STACK_LIMIT * 8 > Regexp::STEP_LIMIT
    skip "the stack limit stands out of the step limit's reach here"
  end
  begin
    Regexp.new("(?>a)" * (Regexp::STACK_LIMIT / 2 + 1))
    Regexp.new("(?=a)" * (Regexp::STACK_LIMIT / 2 + 1))
  rescue RegexpError
    skip "a chain sized from the stack limit is more pattern than one may spell"
  end
  limit = Regexp::STACK_LIMIT
  over = "a" * (2 * limit)
  fits = "a" * (limit / 32)
  assert_raise(RegexpError) { over.match(/(?:(?>a))*/) }       # answered a part of the run
  assert_raise(RegexpError) { over.match(/(?:(?=a)a)*/) }      # a part of the run
  assert_raise(RegexpError) { (over + "b").match(/(a)*?b/) }   # began later than it should
  assert_raise(RegexpError) { over.match(/(?:(?>a))*\z/) }    # began later than it should
  assert_equal fits, fits.match(/(?:(?>a))*/)[0]
  assert_equal fits, fits.match(/(?:(?=a)a)*/)[0]
  assert_equal 0, (fits + "b").match(/(a)*?b/).begin(0)
  assert_equal 0, fits.match(/(?:(?>a))*\z/).begin(0)
  assert_equal "a", "a".match(Regexp.new("(?=a)" * (limit / 32) + "a"))[0]
  assert_equal fits, fits.match(Regexp.new("(?>a)" * (limit / 32 - 1) + "a"))[0]
  # A chain of atomic groups or of lookarounds holds nothing per link once
  # each has closed, so its length is bounded by the pattern and not by the
  # limit.
  chain = limit / 2 + 1
  assert_equal "a" * (chain + 1), over.match(Regexp.new("(?>a)" * chain + "a"))[0]
  assert_equal "a", "a".match(Regexp.new("(?=a)" * chain + "a"))[0]
  # The message names the limit, so that whoever hits one on a legitimate
  # subject knows which knob to turn, and the constant says where it stands.
  assert_raise_with_message(RegexpError, "stack limit over (MRB_REGEXP_STACK_LIMIT)") do
    over.match(/(?:(?>a))*/)
  end
  # A limit ends the search at the start position it was hit at: the
  # positions after it would say where the first match is only once this one
  # has none. The search that reads no captures raises the same.
  assert_raise(RegexpError) { ("b" + over + "c").match(/b(?:(?>a))*c|a/) }   # began at 1
  assert_raise(RegexpError) { over.match?(/(?:(?>a))*\z/) }
  assert_raise(RegexpError) { /(?:(?>a))*\z/ === over }
end

assert("Regexp - a search that runs too long raises at the step limit") do
  need_backtracking_stack
  # The step limit bounds the work one search may do, where the stack limit
  # bounds the state it holds while doing it, and a search reaches this one
  # holding almost nothing: `(?:a|a|a|a)+(z)\1` tries four branches per
  # character of the run and holds about three entries per character, so
  # 4**m steps stand against 3*m entries. Sizing the run from the step
  # limit's width that way is what lets the assertion read the same on a
  # build with a low stack limit, where a pattern that reaches the step
  # limit by the length of its run alone would fill the stack first and pin
  # the other limit.
  #
  # The width is counted by shifting the limit down rather than 1 up to it,
  # so that a build setting the limit near the width of `mrb_int` has no
  # shift of its own to overflow. `(?:a)+` alone never reaches this engine,
  # the Pike VM running it in no stack at all, so the backreference is what
  # sends the pattern here.
  n = 0
  n += 1 while (Regexp::STEP_LIMIT - 1) >> n > 0
  steps = "a" * (n / 2 + 1)
  assert_raise_with_message(RegexpError, "step limit over (MRB_REGEXP_STEP_LIMIT)") do
    steps.match(/(?:a|a|a|a)+(z)\1/)
  end
end

assert("Regexp - the two limits say where they stand") do
  # Whichever engine a pattern reaches, the build's answer to both knobs is
  # readable: a build that sets one low is where the constants matter most.
  assert_kind_of Integer, Regexp::STACK_LIMIT
  assert_kind_of Integer, Regexp::STEP_LIMIT
  assert_true Regexp::STACK_LIMIT >= 1
  assert_true Regexp::STEP_LIMIT >= 1
end

assert("Regexp - a greedy repetition costs the backtracking engine no C stack") do
  need_backtracking_stack
  # A greedy repetition tries its body first and keeps the exit for later,
  # whatever the body holds, so the branch it has not taken used to be a C
  # frame it recursed into and the frames accumulated one per iteration: the
  # run a search could cross was what the C stack held, and `(?:a)*` reached
  # the limit on a subject that is nothing out of the ordinary. The branch is
  # a choice point on the heap now, and what bounds the run is how many of
  # those MRB_REGEXP_STACK_LIMIT allows.
  #
  # `(?:a)*` alone never reaches this engine, the Pike VM running it in no
  # stack at all, so the backreference is what sends the pattern here.
  if Regexp::STACK_LIMIT * 8 > Regexp::STEP_LIMIT
    skip "the stack limit stands out of the step limit's reach here"
  end
  s = "a" * (Regexp::STACK_LIMIT / 4) + "bb"
  assert_equal 0, s.match(/(?:a)*(b)\1/).begin(0)
  assert_equal 0, s.match(/(?:a)+(b)\1/).begin(0)
  assert_equal 0, s.match(/(?:a)*?(b)\1/).begin(0)
  assert_equal s, s.match(/(?:a)*(b)\1/)[0]
end

assert("Regexp - a capture costs the backtracking engine no C stack") do
  need_backtracking_stack
  # A capture wrote its slot and recursed, so that the frame could put back
  # what the slot held when the branch below it failed: a repetition of a
  # capturing group spent two frames an iteration, and `(a)*?b`, which runs
  # its iterations in one frame otherwise, reached the limit by the length
  # of the run. The write is logged now, and taking it back is the log
  # unwinding to where the branch was left.
  if Regexp::STACK_LIMIT * 8 > Regexp::STEP_LIMIT
    skip "the stack limit stands out of the step limit's reach here"
  end
  n = Regexp::STACK_LIMIT / 4
  s = "a" * n + "b"
  md = s.match(/(a)*?b/)
  assert_equal 0, md.begin(0)
  assert_equal s, md[0]
  assert_equal "a", md[1]
  assert_equal n - 1, md.begin(1)
end

assert("Regexp - what the undo log puts back is what the branch found") do
  # A slot goes back to what stood in it when the branch that wrote it was
  # taken, so a group that a later branch did not enter reads as it did
  # before rather than as the failed branch left it.
  assert_equal [nil, "b"], "ab".match(/(?:(x)|a)(b)/).captures
end

assert("Regexp - what a start position records is not read by the next one") do
  need_backtracking_stack
  # Where the running iteration of an empty-matchable repetition began is
  # recorded on the undo log, which a match does not unwind and a failure
  # unwinds only as far as the branch it goes back to. A start position that
  # has failed must leave none of it behind: the first iteration of an `e+`
  # reads the record without having written it, and one left at the offset
  # the next start position reaches would stop that repetition before it had
  # gone round, which shows up as a capture the answer should not hold rather
  # than as a crash. The search from a later position therefore has to answer
  # what the same search over the tail alone answers.
  ["(?:(a)|)+b", "(?:(a)|)+?b", "(?:(a)|)*b\\1", "((?:a|)+)b\\1",
   "(?:(a*))+?b\\1", "(?:(?>a*)(b|))+?c"].each do |src|
    re = Regexp.new(src)
    ["ab", "aab", "b", "abb"].each do |tail|
      # a head the patterns cannot reach into, so that the leftmost match of
      # the whole is the tail's own
      %w[x xx xyx].each do |head|
        md = (head + tail).match(re)
        want = tail.match(re)
        if want
          assert_equal want[0], md && md[0], "#{src} on #{head + tail}"
          assert_equal want.captures, md && md.captures, "#{src} on #{head + tail}"
        end
      end
    end
  end
end

assert("Regexp - an atomic group costs the backtracking engine no C stack") do
  need_backtracking_stack
  # The group used to run its body, and then the text after it, in frames of
  # its own, so that a failure after the group could unwind the frames the
  # body left rather than backtrack into them: a repetition of one spent two
  # frames an iteration and a chain of them two a link. Entering the group
  # pushes a barrier onto the choice point stack instead, and its end drops
  # the barrier and every alternative the body left above it: the cut, as
  # a truncation.
  if Regexp::STACK_LIMIT * 8 > Regexp::STEP_LIMIT
    skip "the stack limit stands out of the step limit's reach here"
  end
  begin
    Regexp.new("(?>a)" * (Regexp::STACK_LIMIT / 2 + 1))
  rescue RegexpError
    skip "a chain sized from the stack limit is more pattern than one may spell"
  end
  limit = Regexp::STACK_LIMIT
  s = "a" * (limit / 4)
  assert_equal s, s.match(/(?:(?>a))*/)[0]
  assert_equal 0, s.match(/(?:(?>a))*\z/).begin(0)
  n = limit / 2 + 1
  assert_equal "a" * (n + 1), ("a" * (n + 1)).match(Regexp.new("(?>a)" * n + "a"))[0]
  # What the truncation leaves is what the frames left: the body's captures
  # stay once it has matched, and go when the search backtracks past where
  # the group began.
  assert_equal "a", /(?>(a))a/.match("aa")[1]
  assert_nil /(?:(?>(a))b|a)/.match("a")[1]
  assert_nil /(?>a*)a/ =~ "aaa"
  assert_equal 0, /(?>a*)b/ =~ "aab"
end

assert("Regexp - a lookaround costs the backtracking engine no C stack") do
  need_backtracking_stack
  # A lookaround ran its sub-pattern in a call of its own, and a positive one
  # ran the text after it inside that call as well, so that a failure there
  # could come back as a cut and undo what the sub-pattern had captured: two
  # frames a lookaround, and a repetition of one or a chain of them reached
  # the limit by their length. Entering one pushes a barrier instead, and its
  # end drops the barrier and the alternatives the sub-pattern left, going on
  # with the text after from where the lookaround was entered.
  if Regexp::STACK_LIMIT * 8 > Regexp::STEP_LIMIT
    skip "the stack limit stands out of the step limit's reach here"
  end
  begin
    Regexp.new("(?=a)" * (Regexp::STACK_LIMIT / 2 + 1))
  rescue RegexpError
    skip "a chain sized from the stack limit is more pattern than one may spell"
  end
  limit = Regexp::STACK_LIMIT
  s = "a" * (limit / 4)
  assert_equal s, s.match(/(?:(?=a)a)*/)[0]
  assert_equal s, s.match(/(?:a(?<=a))*/)[0]
  assert_equal s, s.match(/(?:(?!b)a)*/)[0]
  n = limit / 2 + 1
  assert_equal "a", "a".match(Regexp.new("(?=a)" * n + "a"))[0]
  # What the barrier keeps and what it takes back is what the frames did: a
  # positive lookaround's captures outlive it and a negative one's do not,
  # and neither survives the search going back past where it was entered.
  assert_equal "a", /(?=(a))a/.match("a")[1]
  assert_equal ["b", "a"], /(?<=(a))b/.match("ab").to_a
  assert_nil /(?!(a))|/.match("a")[1]
  assert_nil /(?:(?=(a))b|)/.match("a")[1]
  assert_nil /(?=(a|ab))\1c/ =~ "abc"
end

assert("Regexp - quantified first alternative does not leak into the next") do
  # A quantifier loops back to its own atom. When the atom starts the first
  # alternative, the alternation SPLIT is inserted in front of it; the
  # loop-back must follow the atom, not land on the new SPLIT (which used to
  # let /\d+|\w/ match "1b" by re-entering the alternation after "1").
  assert_equal "1", "1b2c3".match(/\d+|\w/)[0]
  assert_equal ["a", "1", "b", "2", "c", "3"], "a1b2c3".scan(/\d+|\w/)
  assert_equal "aaa", "aaa".match(/a+|b/)[0]
  assert_equal "123", "123abc".match(/\d+|\w+/)[0]
end

assert("Regexp - captures") do
  re = Regexp.new("(\\w+)@(\\w+)")
  md = re.match("user@host")
  assert_equal "user@host", md[0]
  assert_equal "user", md[1]
  assert_equal "host", md[2]
end

assert("Regexp - \\d \\w \\s") do
  assert_true Regexp.new("\\d+").match?("123")
  assert_true Regexp.new("\\w+").match?("abc_123")
  assert_true Regexp.new("\\s+").match?("  ")
  assert_false Regexp.new("\\d+").match?("abc")
end

assert("Regexp - negated shorthands \\D \\W \\S") do
  # \D \W \S must be the complement of \d \w \s, not aliases of them.
  # (A double negation in the compiler made \D match digits, etc.)
  assert_equal ["a", " ", "b"], "a1 b2".scan(/\D/)
  assert_equal [" "],           "a1 b2".scan(/\W/)
  assert_equal ["a", "1", "b", "2"], "a1 b2".scan(/\S/)
  assert_equal "_9__", "x9 z".gsub(/\D/, "_")
  # inside [...] the shorthands keep working, including mixed full-range sets
  assert_equal ["a", " ", "b"], "a5 b".scan(/[\D]/)
  assert_equal ["a", "5", " ", "b"], "a5 b".scan(/[\s\S]/)
  assert_equal [" "], "foo BAR".scan(/[\W\d]/)
end

assert("Regexp - anchors") do
  assert_true Regexp.new("^abc").match?("abc")
  assert_false Regexp.new("^abc").match?("xabc")
  assert_true Regexp.new("abc$").match?("abc")
  assert_false Regexp.new("abc$").match?("abcx")
end

assert("Regexp - ^ and $ always match at line boundaries") do
  # In Ruby ^ and $ are line anchors regardless of /m (which only makes `.`
  # match a newline). \A and \z stay anchored to the whole string.
  assert_equal "bar", "foo\nbar".match(/^bar/)[0]
  assert_equal "foo", "foo\nbar".match(/foo$/)[0]
  assert_equal ["a", "b", "c"], "a\nb\nc".scan(/^./)
  assert_equal ["a", "b", "c"], "a\nb\nc".scan(/.$/)
  assert_equal 3, "a\nb\nc".scan(/^/).size
  # a trailing newline opens no final line, so ^ does not match at the end
  assert_equal 1, "a\n".scan(/^/).size
  assert_equal ">a\n>b\n>c", "a\nb\nc".gsub(/^/, ">")
  assert_equal ["a\n", "b\n", "c"], "a\nb\nc".split(/^/)
  # \A / \z remain absolute
  assert_nil(/\Abar/.match("foo\nbar"))
  assert_nil(/foo\z/.match("foo\nbar"))
  assert_equal "bar", "foo\nbar".match(/bar\z/)[0]
end

assert("Regexp - \\Z matches before a trailing newline") do
  # \Z is the string end or the position just before a final newline.
  assert_equal 0, "a" =~ /a\Z/
  assert_equal 0, "a\n" =~ /a\Z/
  assert_nil "a\n\n" =~ /a\Z/
  assert_nil "ab" =~ /a\Z/
end

assert("Regexp - \\Z matches before a trailing newline under the backtracking engine too") do
  need_backtracking_stack
  # The same four, with a lazy quantifier or a lookaround to route the
  # pattern to the other engine, which had no case for the opcode and so
  # failed every \Z it saw.
  assert_equal 0, "a" =~ /a\Za*?/
  assert_equal 0, "a\n" =~ /a\Z.*?/
  assert_nil "a\n\n" =~ /a\Z.*?/
  assert_nil "ab" =~ /a\Zb*?/
  assert_equal "aX\n", "ab\n".sub(/b\Z(?=)/, "X")
end

assert("Regexp - case insensitive") do
  re = Regexp.new("abc", Regexp::IGNORECASE)
  assert_true re.match?("ABC")
  assert_true re.match?("Abc")
end

assert("Regexp - /i literals share one class per letter") do
  # Under /i a letter compiles to a class of its cases, and a class id is a
  # byte, so a pattern holds at most 256 of them. Every occurrence used to take
  # one, and a phrase of a few hundred letters was refused as too many
  # character classes; the second occurrence of a letter now names the class
  # the first one made.
  re = Regexp.new("a" * 300, Regexp::IGNORECASE)
  assert_true re.match?("A" * 300)
  assert_true re.match?("a" * 300)
  assert_false re.match?("A" * 299)
  re = Regexp.new("aA" * 150, Regexp::IGNORECASE)
  assert_true re.match?("AA" * 150)
  assert_true re.match?("aa" * 150)
  # The class is consulted only where /i is on: outside it the same letter
  # matches its own case alone.
  re = Regexp.new("(?i:a)a")
  assert_true re.match?("Aa")
  assert_false re.match?("AA")
end

assert("Regexp - case insensitive character class") do
  # /i used to be folded in only where a single literal was emitted, so a
  # character class ignored it entirely.
  assert_true(/[abc]/i.match?("A"))
  assert_true(/[a-c]/i.match?("A"))
  assert_true(/[A-C]/i.match?("a"))
  assert_true(/[a-c]+/i.match?("AB"))
  assert_true Regexp.new("[a-c]", Regexp::IGNORECASE).match?("A")
  # A negated class matched what it had to reject, which is a false positive.
  assert_false(/[^a-c]/i.match?("A"))
  assert_false(/[^A-C]/i.match?("a"))
  assert_true(/[^a-c]/i.match?("d"))
  # Folding must not widen the class beyond the ASCII letters.
  assert_false(/[a-c]/i.match?("D"))
  assert_false(/[\[]/i.match?("{"))  # `[` and `{` are 32 apart but are not a case pair
  assert_false(/[@]/i.match?("`"))
end

assert("Regexp - /i folds an ASCII letter's class whole") do
  # U+017F folds to "s" and U+212A to "k". They are the only two foldings
  # whose result is an ASCII letter, and every build carries them, so that
  # folding "ASCII only" covers the whole of the equivalence class an ASCII
  # letter belongs to rather than the part of it that is ASCII. Left out, the
  # negated forms below would accept what they were written to reject.
  # Both sources lie above ASCII, so they are characters to fold only where
  # the pattern and the subject are read as characters.
  skip unless __ENCODING__ == "UTF-8"
  kelvin = "K"
  long_s = "ſ"
  assert_true Regexp.new("k", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("K", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[k]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[a-z]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[j-l]", Regexp::IGNORECASE).match?(kelvin)
  assert_false Regexp.new("[^k]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new(kelvin, Regexp::IGNORECASE).match?("k")
  assert_true Regexp.new(kelvin, Regexp::IGNORECASE).match?("K")
  assert_true Regexp.new("[#{kelvin}]", Regexp::IGNORECASE).match?("K")
  assert_true Regexp.new("s", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^s]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new(long_s, Regexp::IGNORECASE).match?("S")
  # A backreference compares the same way, so the capture and the repeat need
  # not hold the same bytes.
  assert_equal "k#{kelvin}", "k#{kelvin}".match(Regexp.new("(k)\\1", Regexp::IGNORECASE))[0]
  # Without /i none of it folds.
  assert_false Regexp.new("k").match?(kelvin)
  assert_true Regexp.new("[^k]").match?(kelvin)
end

assert("Regexp - /i keeps the word class inside ASCII") do
  # `\w` is [a-zA-Z0-9_] and no more, and [:word:] and [:ascii:] are sets
  # ASCII defines the same way, so /i folds none of them across the boundary:
  # the fold of a member that leaves ASCII leaves the set. CRuby reads them
  # the same way. The negated forms are where it shows: [^\w] under /i has to
  # accept U+212A and U+017F, which are not word characters, and used to
  # reject them because the closure of [k] and [s] had been applied to `\w`.
  # Both sources lie above ASCII, so they are characters only where the
  # pattern and the subject are read as characters.
  skip unless __ENCODING__ == "UTF-8"
  kelvin = "K"
  long_s = "ſ"
  [kelvin, long_s].each do |ch|
    assert_false Regexp.new("[\\w]", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("[^\\w]", Regexp::IGNORECASE).match?(ch)
    assert_false Regexp.new("[[:ascii:]]", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("[^[:ascii:]]", Regexp::IGNORECASE).match?(ch)
    # `\W` holds neither letter and everything above ASCII, so it takes both
    # with or without the fold; the negated form is what a fold would break.
    assert_true Regexp.new("[\\W]", Regexp::IGNORECASE).match?(ch)
    assert_false Regexp.new("[^\\W]", Regexp::IGNORECASE).match?(ch)
    # Outside a class the shorthand never folded, and still does not.
    assert_false Regexp.new("\\w", Regexp::IGNORECASE).match?(ch)
    assert_true Regexp.new("\\W", Regexp::IGNORECASE).match?(ch)
    # /i does not move either in or out of [:word:], whatever the set holds:
    # the ASCII word characters on a build without the type table, every
    # Unicode word character on one with it, these two among them.
    assert_equal Regexp.new("[[:word:]]").match?(ch),
                 Regexp.new("[[:word:]]", Regexp::IGNORECASE).match?(ch)
    assert_equal Regexp.new("[^[:word:]]").match?(ch),
                 Regexp.new("[^[:word:]]", Regexp::IGNORECASE).match?(ch)
  end
  # A letter written out beside the shorthand folds as it does on its own:
  # the class then holds it by name as well as through `\w`, and the name is
  # what folds. Either case of the letter, in either order, and a range too.
  assert_true Regexp.new("[\\ws]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new("[\\wS]", Regexp::IGNORECASE).match?(long_s)
  assert_true Regexp.new("[k\\w]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[\\wa-z]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^\\ws]", Regexp::IGNORECASE).match?(long_s)
  # Naming one letter folds that letter and no other.
  assert_false Regexp.new("[\\wk]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[\\ws]", Regexp::IGNORECASE).match?(kelvin)
  # The other direction is untouched: a member above ASCII still folds to the
  # letter, and reaches the letter's other case through it.
  assert_true Regexp.new("[\\w#{long_s}]", Regexp::IGNORECASE).match?("S")
  assert_false Regexp.new("[^\\w#{long_s}]", Regexp::IGNORECASE).match?("S")
  # The other POSIX brackets fold like a written range: [:lower:] holds `k`,
  # so under /i it reaches U+212A, and [^[:alpha:]] rejects U+017F. Where the
  # build has the type table both hold the letter without /i as well; the
  # answers below are the same either way.
  assert_true Regexp.new("[[:lower:]]", Regexp::IGNORECASE).match?(kelvin)
  assert_true Regexp.new("[[:alpha:]]", Regexp::IGNORECASE).match?(long_s)
  assert_false Regexp.new("[^[:alpha:]]", Regexp::IGNORECASE).match?(long_s)
end

assert("Regexp - repetition {n,m}") do
  assert_equal "aaa", Regexp.new("a{3}").match("aaaa")[0]
  assert_equal "aa", Regexp.new("a{2,3}").match("aa")[0]
  assert_equal "aaa", Regexp.new("a{2,3}").match("aaaa")[0]
end

assert("Regexp - an upper bound below the lower one is an error") do
  # `{n,m}` with m < n names no repeat count, and CRuby raises rather than
  # compiling it; it used to compile as `{n}` and match exactly n repeats.
  assert_raise(RegexpError) { Regexp.new("a{2,1}") }
  assert_raise(RegexpError) { Regexp.new("^a{3,1}$") }
  # The non-greedy marker comes after the `}`, so it does not save the range.
  assert_raise(RegexpError) { Regexp.new("a{3,1}?") }
  # Nor does a group, a class, or a lookaround.
  assert_raise(RegexpError) { Regexp.new("(ab){2,1}") }
  assert_raise(RegexpError) { Regexp.new("[a-z]{5,3}") }
  assert_raise(RegexpError) { Regexp.new("(?=a{2,1})") }
  # A body that matches empty reads its quantifiers and emits nothing for
  # them, and the range is read there too.
  assert_raise(RegexpError) { Regexp.new("a{0}{2,1}") }
  assert_raise(RegexpError) { Regexp.new("(?:){2,1}") }
  # Where there is no atom to repeat, the range is what CRuby reports, ahead
  # of the missing target.
  assert_raise(RegexpError) { Regexp.new("{2,1}") }
  # Equal bounds are a repeat of exactly n, and an omitted upper bound is
  # unlimited: neither is below the lower bound.
  assert_equal "aa", Regexp.new("a{2,2}").match("aaa")[0]
  assert_equal "aaa", Regexp.new("a{2,}").match("aaa")[0]
  assert_equal "", Regexp.new("a{0,0}").match("aaa")[0]
  # A `{...}` that is no quantifier is still a literal brace rather than a
  # bad range, whether the `}` is missing or the braces are escaped.
  assert_equal "a{2,1", "a{2,1".match(/a{2,1/)[0]
  assert_equal "a{2,1}", "a{2,1}".match(/a\{2,1}/)[0]
  assert_equal "{2,1}", "a{2,1}".match(/[{]2,1[}]/)[0]
end

assert("Regexp - repeated group keeps each iteration self-contained") do
  # Copying a grouped quantifier body must relocate its internal jumps, or a
  # later copy jumps back into the first and reports the wrong capture span.
  m = "aaaaab".match(/(a{2,3}){2}/)
  assert_equal "aaaaa", m[0]
  assert_equal "aa", m[1]
  assert_equal "ab", "ababab".match(/(ab){2}/)[1]
  assert_equal "a", "abab".match(/(a|b){3}/)[1]
  assert_equal ["abab", "ab"], "abab".match(/((a)(b)){2}/).to_a[0, 2]
  assert_equal "34", "1234".match(/(\d{2}){2}/)[1]
end

assert("Regexp - repetition with a zero lower bound") do
  # A zero lower bound must not force the one already-compiled copy: {0,m}
  # caps at m (it used to match m+1), {0} matches nothing, {0,} is just *.
  assert_equal "aaa", "aaaa".match(/a{0,3}/)[0]
  assert_equal "aaa", "aaaa".match(/a{,3}/)[0]
  assert_equal "", "aaa".match(/a{0}/)[0]
  assert_equal "b", "b".match(/a{0}b/)[0]
  assert_equal "aaaa", "aaaa".match(/a{0,}/)[0]
  assert_equal "bc", "bc".match(/ba{0,2}c/)[0]
  assert_equal "baac", "baac".match(/ba{0,2}c/)[0]
  assert_nil "baaac".match(/\Aba{0,2}c\z/)
end

assert("Regexp - a curly brace that is not a quantifier is a literal") do
  # An invalid {...} used to spin the compiler forever (issue #6914); it must
  # be treated as a literal brace, matching CRuby. A well-formed quantifier
  # with nothing to repeat is an error instead.
  assert_equal "{a}", "x{a}y".match(/{a}/)[0]
  assert_equal "{", "a{b".match(/{/)[0]
  assert_equal "{}", "a{}b".match(/{}/)[0]
  assert_equal "a{}", "a{}".match(/a{}/)[0]
  assert_equal "{,}", "x{,}y".match(/{,}/)[0]
  assert_equal "a{b}c", "a{b}c".match(/a{b}c/)[0]
  assert_raise(RegexpError) { Regexp.new("{2}") }
end

assert("Regexp - a quantifier after a quantifier repeats the repeat") do
  # CRuby reads a second quantifier as binding the repeat before it, not the
  # atom: `a**` is `(?:a*)*`. Two spellings are not that, and are read where
  # the first quantifier is. Every repeat here is greedy, so the Pike VM
  # runs them all; the two spellings that are not a quantifier at all, the
  # non-greedy marker and the possessive one, are pinned below.
  assert_equal ["aaa"], /a**/.match("aaa").to_a
  assert_equal ["aaa"], /a+*/.match("aaa").to_a
  assert_equal ["aaa"], /a?*/.match("aaa").to_a
  assert_equal ["aaaaaa"], /a{2}{3}/.match("aaaaaa").to_a
  assert_equal ["aaaa"], /a{2}+/.match("aaaa").to_a
  assert_equal ["aaa"], /a{1,2}*/.match("aaa").to_a
  assert_equal ["aaa"], /a***/.match("aaa").to_a
  assert_equal ["aa"], /(?:a?){2}/.match("aaa").to_a
  assert_equal ["aa"], /a?{2}/.match("aaa").to_a
  assert_equal ["aa"], /(?:a?)+/.match("aa").to_a

  # `{n}` has no non-greedy form, so its `?` is a quantifier: `a{3}?` matches
  # empty where the lazy `a{3,3}?` does not. The optional wraps the copies of
  # `{n}` alone, so the rest of the pattern still has to match: `xa{2}?y`
  # takes "xy" and "xaay" but nothing between, and inside a group the capture
  # is what the wrapper let through.
  assert_equal [""], /a{3}?/.match("").to_a
  assert_equal [""], /a{3}?/.match("aa").to_a
  assert_equal ["xy"], /xa{2}?y/.match("xy").to_a
  assert_equal ["xaay"], /xa{2}?y/.match("xaay").to_a
  assert_nil /xa{2}?y/.match("xay")
  assert_equal ["xy"], /xa{1}?y/.match("xy").to_a
  assert_equal [""], /^a{2}?$/.match("").to_a
  assert_equal ["xy", ""], /x(a{2}?)y/.match("xy").to_a
  assert_equal ["xaay", "aa"], /x(a{2}?)y/.match("xaay").to_a

  # A body that matches no times matches empty however it is repeated.
  assert_equal [""], /a{0}*/.match("").to_a
  assert_equal [""], /a{0}{2}?/.match("aa").to_a

  # A repeat of a repeat is an empty-matching loop by construction; the null
  # check of both engines stops it rather than the stack limit.
  assert_equal ["aaa"], /(?:a?)**/.match("aaa").to_a
  assert_equal ["b"], /(?:a?)**b/.match("b").to_a
end

assert("Regexp - the non-greedy and possessive markers are read where the quantifier is") do
  need_backtracking_stack
  # The two spellings the block above leaves out, both of which send the
  # pattern to the backtracking engine.

  # `?` right after a greedy `*`, `+`, `?` or a `{n,m}` written with a comma
  # is the non-greedy marker; written without one it is the quantifier the
  # block above pins.
  assert_equal [""], /a*?/.match("aaa").to_a
  assert_nil /a{3,3}?/.match("aa")
  assert_equal ["aaa"], /a{3,3}?/.match("aaa").to_a
  assert_equal [""], /a{0,2}?/.match("aa").to_a
  assert_nil /xa{2,2}?y/.match("xy")

  # `+` right after a greedy `*`, `+` or `?` is possessive, `a*+` being
  # `(?>a*)`: `a?+` takes one `a` out of "aa" where `(?:a?)+` takes two. After
  # a lazy repeat, a possessive one or a `{...}` it is a quantifier again.
  assert_equal ["a"], /a?+/.match("aa").to_a
  assert_nil /a?+a/.match("a")
  assert_equal ["aa"], /a?+a/.match("aa").to_a
  assert_equal ["aaa"], /a*+/.match("aaa").to_a
  assert_equal ["aa"], /a+?+/.match("aa").to_a
  assert_equal ["a"], /a+?+?/.match("aa").to_a
  assert_equal ["aa"], /a?++/.match("aa").to_a

  # A repeat of a repeat is an empty-matching loop by construction; the null
  # check of both engines stops it rather than the stack limit.
  assert_equal ["aaa"], /(?:a*)*+/.match("aaa").to_a
end

assert("Regexp - a backreference names a group the pattern has") do
  # The count is the pattern's, not the one standing where the reference is,
  # so a reference past it is refused where the pattern is read.
  assert_raise(RegexpError) { Regexp.new("\\1") }
  assert_raise(RegexpError) { Regexp.new("(a)\\2") }
  assert_raise(RegexpError) { Regexp.new("(a)(b)\\3") }
  assert_raise(RegexpError) { Regexp.new("\\9") }
  # Both spellings ask the same count, so both refuse the same references.
  assert_raise(RegexpError) { Regexp.new("\\k<1>") }
  assert_raise(RegexpError) { Regexp.new("(a)\\k<2>") }
  assert_raise(RegexpError) { Regexp.new("(a)(b)\\k<3>") }
  # A number above the capture limit names no group whatever follows it.
  assert_raise(RegexpError) { Regexp.new("\\k<32>(a)") }
  # A named pattern refuses a numbered reference before it counts.
  assert_raise(RegexpError) { Regexp.new("(?<n>a)\\1") }
  assert_raise(RegexpError) { Regexp.new("(?<n>a)\\2") }
end

assert("Regexp - a backreference reaches a group written after it") do
  need_backtracking_stack
  # The count being the pattern's rather than the one standing where the
  # reference is, a reference to a group written later is valid; the group
  # has captured nothing where the reference is read, so it does not match.
  assert_equal ["aa", "a"], /(a)\1/.match("aa").to_a
  assert_nil(/\1(a)/ =~ "a")
  assert_equal 0, Regexp.new("(a)" * 9 + "\\9") =~ "aaaaaaaaaa"

  # The `\k<n>` spelling of the same reference reaches the same group: it used
  # to be checked where it stood while `\1` was checked after the parse, so
  # the two spellings of one forward reference disagreed.
  assert_nil(Regexp.new("\\k<1>(a)") =~ "a")
  assert_equal ["aa", "a"], Regexp.new("(a)\\k<1>").match("aa").to_a
  assert_nil(Regexp.new("\\k<2>(a)(b)") =~ "ab")
  assert_nil(Regexp.new("(a)\\k<2>(b)") =~ "ab")

  # What the forward reference is for: the group has captured by the second
  # iteration, so the reference matches there.
  assert_equal ["aa", "a"], Regexp.new("(?:\\1|(a))+").match("aa").to_a
  assert_equal ["cc", "c"], Regexp.new("(?:\\k<1>|(c))+").match("cc").to_a

  # The relative form stays as it was: `\k<-n>` counts back from where it
  # stands, so it names one of the groups already open and never a later one.
  assert_equal "abba", "abba".match(Regexp.new("(.)(.)\\k<-1>\\k<-2>"))[0]
  assert_raise(RegexpError) { Regexp.new("\\k<-1>(a)") }
  assert_raise(RegexpError) { Regexp.new("(?:\\k<-1>|(c))+") }
end

assert("Regexp - patterns that used to hang the compiler now raise (A1)") do
  # These once looped forever in the compiler at 100% CPU instead of raising.
  # Regexp.new is used so the pattern reaches the regexp compiler directly,
  # bypassing the literal validation the parser performs on /.../ literals.

  # (?X) with an unsupported X: the absent operator (?~...) and conditionals
  # (?(...)) are not implemented (inline options (?i)/(?i:...) now are).
  assert_raise(RegexpError) { Regexp.new("(?~foo)") }
  assert_raise(RegexpError) { Regexp.new("(?(<x>)a|b)") }
  assert_raise(RegexpError) { Regexp.new("(?") }
  assert_raise(RegexpError) { Regexp.new("(?<") }

  # A quantifier metacharacter with no atom to repeat. `a***` has one, the
  # repeat before it: it is `(?:(?:a*)*)*`.
  assert_equal ["aaa"], Regexp.new("a***").match("aaa").to_a
  assert_raise(RegexpError) { Regexp.new("*") }
  assert_raise(RegexpError) { Regexp.new("+") }
  assert_raise(RegexpError) { Regexp.new("?abc") }
end

assert("Regexp - inline options (?i) / (?i:...)") do
  # Toggle form: options apply to the rest of the enclosing group.
  assert_equal 0, (/(?i)abc/ =~ "ABC")
  assert_equal 0, (/a(?i)b/ =~ "aB")
  assert_nil (/a(?i)b/ =~ "Ab")          # the leading `a` stays case-sensitive
  assert_equal 0, (/(?i)a(?-i)b/ =~ "Ab") # `-i` turns it back off
  assert_nil (/(?i)a(?-i)b/ =~ "AB")

  # Scoped form: a non-capturing group whose options apply only to its body.
  assert_equal 0, (/(?i:abc)/ =~ "ABC")
  assert_nil (/(?i:a)b/ =~ "aB")          # option must not leak past the `)`
  assert_equal 0, (/(?i:ab)+/ =~ "AbaB")  # scoped group is still quantifiable

  # A character class reads the inline-scoped flag, not the pattern-wide one.
  assert_equal 0, (/(?i)[a-c]/ =~ "A")
  assert_equal 0, (/(?i:[a-c])/ =~ "A")
  assert_nil (/(?i:[a-c])[a-c]/ =~ "AB")  # option must not leak past the `)`

  # The toggle inside a group is confined to that group.
  assert_equal 0, (/(a(?i)b)c/ =~ "aBc")
  assert_nil (/(a(?i)b)c/ =~ "aBC")       # trailing `c` is case-sensitive again

  # The rest of the group is the toggle's scope alternatives and all, so
  # `a(?i)b|c` is `a(?i:b|c)`: the `|` splits inside the scope, not at the
  # level the toggle was written at, and the `c` still wants the `a` before
  # it. Without that, an alternative after a toggle matched on its own.
  assert_nil (/a(?i)b|c/ =~ "c")
  assert_nil (/a(?i)b|c/ =~ "C")
  assert_equal 0, (/a(?i)b|c/ =~ "ac")
  assert_equal 0, (/a(?i)b|c/ =~ "aC")    # and the option reaches that `c`
  assert_nil (/a(?i)|b/ =~ "b")           # an empty first alternative too
  assert_nil (/x|a(?i)b|c/ =~ "c")        # a toggle in the second alternative
  assert_nil (/(a(?i)b|c)d/ =~ "cd")
  assert_equal 0, (/x(a(?i)b|c)d/ =~ "xacd")
  assert_equal ["acd", "ac", "d"], /(a(?i)b|c)(d)/.match("acd").to_a
  assert_equal 0, (/a(?i)b(?m).|c/ =~ "aB\n")  # a toggle within that scope
  assert_equal 0, (/a(?x) b|c/ =~ "ac")   # x is scoped the same way
  assert_nil (/a(?x) b|c/ =~ "c")

  # Turning an option off is scoped alike, so the alternative after `(?-i)`
  # is case-sensitive whether or not the pattern is /i.
  assert_equal 0, (/a(?-i)b|c/i =~ "ac")
  assert_nil (/a(?-i)b|c/i =~ "aC")
  assert_nil (/a(?-i)b|c/i =~ "C")

  # m enables dot-matches-newline for its scope.
  assert_equal 0, (/(?m:a.b)/ =~ "a\nb")
  assert_nil (/a.b/ =~ "a\nb")

  # x (extended) is scoped inline like the other two: the toggle form
  # reaches the end of the enclosing group, the scoped form its own body.
  assert_equal 0, (/(?x)a b/ =~ "ab")
  assert_nil (/(?x)a b/ =~ "a b")
  assert_equal 0, (/(?x:a b)c d/ =~ "abc d")
  assert_equal 0, (/(a(?x)b c)d e/ =~ "abcd e")
  assert_equal 0, (/(?<n>(?x)a b)c d/ =~ "abc d")
  assert_equal 0, (/(?xi)a b/ =~ "AB")
  assert_equal 0, (/(?x)a b(?-x)c d/ =~ "abc d")
  assert_equal 0, (/(?x:a(?-x:b c)d)/ =~ "ab cd")

  # Free-spacing follows the scope: a comment runs to the end of the line,
  # a (?# group is dropped as always, and an escape or a class keeps its
  # whitespace.
  assert_equal 0, (/(?x)a#c
b/ =~ "ab")
  assert_equal 0, (/(?x)(?#c d) e/ =~ "e")
  assert_equal 0, (/(?x)a\ b/ =~ "a b")
  assert_equal 0, (/(?x)[a b]/ =~ " ")
  assert_equal 0, (/[(?x] a/ =~ "( a")
  assert_equal 0, (/\(?x a/ =~ "(x a")

  # A comment swallows the rest of its line, closing parenthesis included,
  # as it does in CRuby.
  assert_true Regexp.new("(?x)a #b)").match?("a")
  assert_raise(RegexpError) { Regexp.new("(?x)a #b\n(c") }

  # Turning it off inside a pattern that is itself extended brings the
  # whitespace back for that scope.
  assert_equal 0, (/(?-x:a b)/ =~ "a b")
  assert_equal 0, (/(?i-mx:a)b/ =~ "Ab")
  assert_true Regexp.new("(?-mix:a b)").match?("a b")
  assert_true Regexp.new("(?-x:a b)", Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new("(?-x)a b", Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new("(?x)a b", Regexp::EXTENDED).match?("ab")

  # A group that names no letter is still a group: `-` may stand alone or
  # come twice, so a generator emitting `(?#{on}-#{off}:...)` with both
  # lists empty is read rather than refused.
  assert_equal 0, (Regexp.new("(?-)a") =~ "a")
  assert_equal 0, (Regexp.new("(?-:a)") =~ "a")
  assert_equal 0, (Regexp.new("(?--i)a") =~ "a")
  assert_equal 0, (Regexp.new("(?i-)a") =~ "A")
  assert_nil (Regexp.new("(?--i)a") =~ "A")   # the second `-` switches off
  assert_nil (Regexp.new("(?i)a(?--i)b") =~ "aB")
  assert_true Regexp.new("(?-)a b", Regexp::EXTENDED).match?("ab")
  assert_true Regexp.new("(?--x)a b", Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new("(?--x:a b)", Regexp::EXTENDED).match?("a b")

  # What stops the letters is still checked, so a letter that is not an
  # option and an unterminated group raise as before. `(?)` names no option
  # byte at all, not even a `-`, and is not an option group to begin with.
  assert_raise(RegexpError) { Regexp.new("(?-a)b") }
  assert_raise(RegexpError) { Regexp.new("(?-") }
  assert_raise(RegexpError) { Regexp.new("(?)") }
end

assert("Regexp - an inline option reaches a lookaround and a backreference") do
  need_backtracking_stack
  # A backreference takes the options in effect where it appears, not the
  # pattern's own, so an inline toggle reaches it like any other atom; a
  # lookaround's sub-pattern is a scope of its own for the toggle inside it.
  assert_equal 0, (/(a)(?i)\1/ =~ "aA")
  assert_equal 0, (/(a)(?i:\1)/ =~ "aA")
  assert_nil (/(?-i:(a)\1)/i =~ "aA")
  assert_equal 0, (/(?=(?x)a b)ab c/ =~ "ab c")
  # The sub-pattern is where a toggle inside it ends, so the alternation it
  # takes in ends there too: this is `(?=a(?i:b|c))`.
  assert_nil (/(?=a(?i)b|c)/ =~ "c")
  assert_equal 0, (/(?=a(?i)b|c)/ =~ "aC")
end

assert("Regexp - comment groups (?#...)") do
  # The group is removed before the pattern is parsed, so it can stand
  # anywhere, including where an atom cannot.
  assert_true(/a(?#note)b/.match?("ab"))
  assert_true Regexp.new("(?#lead)ab").match?("ab")
  assert_true Regexp.new("ab(?#trail)").match?("ab")
  assert_true Regexp.new("a(?#)b").match?("ab")          # empty comment
  assert_true Regexp.new("a(?#no\nte)b").match?("ab")    # newline is comment text
  assert_equal ["ab", "ab"], Regexp.new("(a(?#c)b)").match("ab").to_a

  # The group is not an atom: a quantifier after it repeats what came before.
  assert_equal 0, (Regexp.new("a(?#x)*") =~ "aaa")
  assert_raise(RegexpError) { Regexp.new("(?#x)*") }

  # A backslash escapes the following byte, so \) does not close the group.
  assert_true Regexp.new("a(?#x\\)y)b").match?("ab")
  # ... but an escaped backslash does not reach the ')', which then closes
  # the group and leaves the second one unmatched.
  assert_raise(RegexpError) { Regexp.new("a(?#x\\\\)y)b") }

  # Comment groups do not nest: the first ')' closes, the second is unmatched.
  assert_raise(RegexpError) { Regexp.new("x(?#a(?#b))y") }

  # An unterminated group raises rather than swallowing the rest.
  assert_raise_with_message(RegexpError, "end pattern in group: /a(?#note/") do
    Regexp.new("a(?#note")
  end

  # Inside a character class the same bytes are ordinary members.
  assert_true Regexp.new("a[(?#c)]b").match?("a#b")
  assert_true Regexp.new("a[(?#c)]b").match?("a(b")

  # An escaped '(' does not open a comment group.
  assert_raise(RegexpError) { Regexp.new("a\\(?#note)b") }
end

assert("Regexp extended mode (x flag)") do
  # whitespace is ignored
  re = Regexp.new('a b c', Regexp::EXTENDED)
  assert_true re.match?("abc")
  assert_false re.match?("a b c")

  # comments are ignored
  re = Regexp.new("a  # match a\nb  # match b\nc", Regexp::EXTENDED)
  assert_true re.match?("abc")

  # whitespace inside character class is literal
  re = Regexp.new('[ ]', Regexp::EXTENDED)
  assert_true re.match?(" ")

  # a POSIX bracket does not end the class, so what follows it is still
  # class content
  re = Regexp.new('[[:alpha:] ]', Regexp::EXTENDED)
  assert_true re.match?(" ")
  assert_true re.match?("a")

  re = Regexp.new('[[:alpha:]#x]', Regexp::EXTENDED)
  assert_true re.match?("#")

  assert_equal " 1 ", Regexp.new('[[:digit:] ]+', Regexp::EXTENDED).match(" 1 ")[0]

  # a bracket the pattern truncates leaves the scan with nothing after the
  # name, and the class is still the parser's error to report
  assert_raise_with_message(RegexpError, "premature end of char-class: /[[:alpha/x") do
    Regexp.new("[[:alpha", Regexp::EXTENDED)
  end
  assert_raise_with_message(RegexpError, "premature end of char-class: /[[:alpha:/x") do
    Regexp.new("[[:alpha:", Regexp::EXTENDED)
  end

  # a ']' written first in a class is a literal member, so the class is
  # still open after it
  re = Regexp.new('[] ]', Regexp::EXTENDED)
  assert_true re.match?(" ")
  assert_true re.match?("]")

  re = Regexp.new('[^] ]', Regexp::EXTENDED)
  assert_false re.match?(" ")
  assert_true re.match?("a")

  # escaped whitespace is preserved
  re = Regexp.new('a\\ b', Regexp::EXTENDED)
  assert_true re.match?("a b")

  # whitespace keeps an escape spelled with digits apart from a digit that
  # follows it, as CRuby's tokenizer does: \x1 2 is two bytes, not \x12
  assert_equal 0, (Regexp.new('\x1 2', Regexp::EXTENDED) =~ "\x012")
  assert_nil Regexp.new('\x1 2', Regexp::EXTENDED) =~ "\x12"
  assert_equal 0, (Regexp.new('\x1 a', Regexp::EXTENDED) =~ "\x01a")
  assert_equal 0, (Regexp.new('\01 2', Regexp::EXTENDED) =~ "\x012")
  assert_raise_with_message(RegexpError, "end pattern with unmatched parenthesis: /\\x1 2(/x") do
    Regexp.new('\x1 2(', Regexp::EXTENDED)
  end

  # a comment group is removed ahead of the line-comment pass, so its ')'
  # survives the '#' inside it
  re = Regexp.new("a (?#note) b", Regexp::EXTENDED)
  assert_true re.match?("ab")

  re = Regexp.new("a (?#note) b # tail\nc", Regexp::EXTENDED)
  assert_true re.match?("abc")

  assert_raise_with_message(RegexpError, "end pattern in group: /a (?#note/x") do
    Regexp.new("a (?#note", Regexp::EXTENDED)
  end

  # inspect shows x flag
  assert_equal "/abc/x", Regexp.new("abc", Regexp::EXTENDED).inspect

  # to_s shows x flag
  assert_equal "(?x-mi:abc)", Regexp.new("abc", Regexp::EXTENDED).to_s

  # errors quote the pattern as written, not the text with the comment removed
  assert_raise_with_message(RegexpError, "premature end of char-class: /a # c\n[/x") do
    Regexp.new("a # c\n[", Regexp::EXTENDED)
  end
  assert_raise_with_message(RegexpError, "end pattern with unmatched parenthesis: /a b(/x") do
    Regexp.new("a b(", Regexp::EXTENDED)
  end

  # The suffix names the flags mrb_re_compile() was entered with, not
  # whatever an inline (?x)/(?-x) leaves c->flags holding by the time the
  # error is raised: turning /x off inline still reports the entry's /x,
  # and turning it on where entry carried none reports no suffix at all.
  # CRuby matches this on both patterns.
  assert_raise_with_message(RegexpError, "premature end of char-class: /(?-x)a # c[/x") do
    Regexp.new("(?-x)a # c[", Regexp::EXTENDED)
  end
  assert_raise_with_message(RegexpError, "premature end of char-class: /(?x)a # c\n[/") do
    Regexp.new("(?x)a # c\n[")
  end

  # Multiple entry flags are named in Regexp#to_s/#inspect's m, i, x order.
  assert_raise_with_message(RegexpError, "premature end of char-class: /[a/ix") do
    Regexp.new("[a", Regexp::EXTENDED | Regexp::IGNORECASE)
  end
end

assert("Regexp extended mode keeps a digit escape apart from what follows it") do
  need_backtracking_stack
  # The same rule as above, where what stands after the whitespace is a
  # backreference's digit or a lookbehind: `\1 0` is group 1 and a `0`.
  assert_equal 0, (Regexp.new('(a)\1 0', Regexp::EXTENDED) =~ "aa0")
  # what keeps them apart is no atom: a quantifier after the digit repeats
  # the digit, and a lookbehind still measures a fixed width
  assert_equal "aa000", Regexp.new('(a)\1 0+', Regexp::EXTENDED).match("aa000")[0]
  assert_equal 2, (Regexp.new('(?<=\x1 2)x', Regexp::EXTENDED) =~ "\x012x")
end

assert("Regexp - free-spacing whitespace stands between tokens only") do
  # Under /x the parser skips whitespace where one token ends and the next
  # begins, as CRuby's tokenizer does, and reads every token with the
  # whitespace inside it in place. So a space cannot split a token that
  # is not one, and taking a space out cannot join two into one.
  x = Regexp::EXTENDED

  # between tokens: literals, groups, alternatives, anchors, quantifiers
  assert_equal ["ab"], Regexp.new("a b", x).match("ab").to_a
  assert_equal ["a", "a"], Regexp.new("( a )", x).match("a").to_a
  assert_equal ["b"], Regexp.new("(?: a | b )", x).match("b").to_a
  assert_equal ["b"], Regexp.new("a | b", x).match("b").to_a
  assert_equal ["a"], Regexp.new("^ a $", x).match("a").to_a
  assert_equal ["axb"], Regexp.new("a . b", x).match("axb").to_a
  assert_equal ["aaa"], Regexp.new("a +", x).match("aaa").to_a
  assert_equal ["aa"], Regexp.new("a {2}", x).match("aa").to_a
  assert_equal ["aa", "a"], Regexp.new("(a) {2}", x).match("aa").to_a
  assert_equal ["b"], Regexp.new("a ?b", x).match("b").to_a
  assert_equal ["A"], Regexp.new("(?i: a )", x).match("A").to_a

  # the five bytes CRuby skips; a vertical tab is a literal
  assert_equal ["ab"], Regexp.new("a \t\n\r\fb", x).match("ab").to_a
  assert_nil Regexp.new("a\vb", x).match("ab")
  assert_equal ["a\vb"], Regexp.new("a\vb", x).match("a\vb").to_a

  # an escaped blank is the blank
  assert_equal ["a b"], Regexp.new("a\\ b", x).match("a b").to_a
  assert_equal ["a\nb"], Regexp.new("a\\\nb", x).match("a\nb").to_a
  assert_equal ["#"], Regexp.new("\\#", x).match("#").to_a

  # inside a token the whitespace is the token's own: `(?` and `{n,m}` are
  # read whole, so a space breaks them rather than being removed from them
  assert_raise_with_message(RegexpError,
                            "target of repeat operator is not specified: /( ?i)A/x") do
    Regexp.new("( ?i)A", x)
  end
  assert_raise(RegexpError) { Regexp.new("(?i )a", x) }
  assert_nil Regexp.new("a{1, 2}", x).match("aa")
  assert_equal ["a{1,2}"], Regexp.new("a{1, 2}", x).match("a{1,2}").to_a
  assert_equal ["a{2}"], Regexp.new("a{ 2}", x).match("a{2}").to_a
  assert_equal ["a{2}"], Regexp.new("a{2 }", x).match("a{2}").to_a
  # the whitespace breaks the interval wherever /x is on, and the inline
  # forms turn it on for their own scope as the flag does for the pattern
  assert_nil Regexp.new("(?x)a{1, 2}").match("aa")
  assert_nil Regexp.new("(?x:a{1, 2})").match("aa")
  assert_equal ["a{1,2}"], Regexp.new("(?x)a{1, 2}").match("a{1,2}").to_a
  # with /x off there is no free-spacing for the braces to lose, and the
  # literal is the whole of `{1, 2}`, its blank included
  assert_equal ["a{1, 2}"], Regexp.new("a{1, 2}").match("a{1, 2}").to_a
  # a comment is gone before the parser reads the interval, so one written
  # inside the braces does not break it the way whitespace does
  assert_equal ["aa"], Regexp.new("a{1,#c\n2}", x).match("aa").to_a
  assert_nil Regexp.new("a{1,#c\n 2}", x).match("aa")
  assert_equal ["aa"], Regexp.new("a{1,(?#c)2}", x).match("aa").to_a
  # a `{` the whitespace makes a literal carries no count at all, so a
  # number too large to repeat by is not one to report
  assert_equal ["a{99999999999}"],
               Regexp.new("a{ 99999999999}", x).match("a{99999999999}").to_a

  # a numeric escape is read whole too: the whitespace that CRuby rejects
  # inside one is rejected here, and the whitespace that ends one short of
  # its full width ends it, so `\x6 1` is `\x06` and `1`
  ["\\u {61 62}", "\\u1 234", "\\u12 34", "\\u00 61", "\\u\t{61 62}",
   "\\u 0061", "[\\u12 34]"].each do |pat|
    assert_raise_with_message(RegexpError, "invalid Unicode escape: /#{pat}/x") do
      Regexp.new(pat, x)
    end
  end
  # nothing after \u is "too short", a wrong byte after it "invalid"; the
  # blank is a byte the parser sees, and reports
  assert_raise_with_message(RegexpError, "invalid Unicode escape: /\\u /x") do
    Regexp.new("\\u ", x)
  end
  assert_raise_with_message(RegexpError, "too short escape sequence: /\\u/x") do
    Regexp.new("\\u", x)
  end
  assert_equal ["ab"], Regexp.new("\\u0061 \\u0062", x).match("ab").to_a
  assert_equal ["ab"], Regexp.new("\\u{ 61  62 }", x).match("ab").to_a
  assert_raise_with_message(RegexpError, "invalid hex escape: /\\x 61/x") do
    Regexp.new("\\x 61", x)
  end
  assert_equal ["\x061"], Regexp.new("\\x6 1", x).match("\x061").to_a
  assert_nil Regexp.new("\\x6 1", x).match("a")
  assert_equal ["\x0061"], Regexp.new("\\0 61", x).match("\x0061").to_a
  assert_equal ["\x061"], Regexp.new("\\06 1", x).match("\x061").to_a
  assert_nil Regexp.new("\\0 61", x).match("1")

  # a group name is the raw bytes up to its terminator, whitespace included,
  # whether it is declared or referenced: the name is "a b"
  assert_equal ["a b"], Regexp.new("(?<a b>x)", x).names
  assert_equal ["a b"], Regexp.new("(?'a b'x)", x).names
  assert_raise_with_message(RegexpError,
                            "undefined name <a b> reference: /(?<ab>x)\\k<a b>/x") do
    Regexp.new("(?<ab>x)\\k<a b>", x)
  end
  assert_raise_with_message(RegexpError,
                            "undefined name <ab> reference: /(?<a b>x)\\k<ab>/x") do
    Regexp.new("(?<a b>x)\\k<ab>", x)
  end
  # a `\k` that whitespace follows is the letter k, with the `<ab>` after
  # the whitespace a literal of its own
  assert_equal "xk<ab>", Regexp.new("(?<ab>x)\\k <ab>", x).match("xk<ab>")[0]
  assert_equal "xk<ab>", Regexp.new("(?<ab>x)\\k\n<ab>", x).match("xk<ab>")[0]
  # a comment inside a name is removed and takes its newline with it
  assert_equal ["ab"], Regexp.new("(?<ab#c\n>x)\\k<ab#c\n>", x).names
  # inside a class the escape is the letter and the space is a member
  assert_equal [" "], Regexp.new("[\\k<a b>]", x).match(" ").to_a
end

assert("Regexp - free-spacing whitespace stands between the tokens the backtracking engine runs") do
  need_backtracking_stack
  # The same rule where the token is a lookaround's opener, an atomic
  # group's, a non-greedy marker or a backreference's name: the whitespace
  # between two tokens goes, and the whitespace inside one breaks it.
  x = Regexp::EXTENDED

  # between tokens
  assert_equal ["a"], Regexp.new("(?= a)a", x).match("a").to_a
  assert_equal ["b"], Regexp.new("(?<= a)b", x).match("ab").to_a
  assert_equal ["b"], Regexp.new("(?! a)b", x).match("b").to_a
  assert_equal ["a"], Regexp.new("(?> a )", x).match("a").to_a
  assert_equal ["aa", "a"], Regexp.new("(?<n> a)\\k<n>", x).match("aa").to_a
  assert_equal ["aa", "a"], Regexp.new("(a) \\1", x).match("aa").to_a

  # the `?` that makes a quantifier non-greedy is read right after it, so a
  # `?` a space away is a repeat of the repeat, `(?:a*)?`, and not the
  # marker, which is read where the quantifier is
  assert_equal [""], Regexp.new("a*?", x).match("aaa").to_a
  assert_equal ["aaa"], Regexp.new("a* ?", x).match("aaa").to_a
  assert_equal ["aa"], Regexp.new("a{2} ?", x).match("aaa").to_a

  # a group name is the raw bytes up to its terminator, whitespace included,
  # so the reference whose name holds the blank is the one that resolves
  assert_equal "xx", Regexp.new("(?<a b>x)\\k<a b>", x).match("xx")[0]
  assert_equal "xx", Regexp.new("(?'a b'x)\\k'a b'", x).match("xx")[0]
  # a comment between `\k` and its name is gone before the parser reads
  # either, as in CRuby, so that `\k` is a reference where whitespace would
  # have left it the letter k
  assert_equal "xx", Regexp.new("(?<ab>x)\\k#c\n<ab>", x).match("xx")[0]
  assert_equal "xx", Regexp.new("(?<ab>x)\\k(?#c)<ab>").match("xx")[0]
  assert_equal "xx", Regexp.new("(?<ab#c\n>x)\\k<ab#c\n>", x).match("xx")[0]
end

assert("Regexp - a removed comment does not reach into an escape") do
  # The pass that removes (?#...) groups and, under /x, `#` comments runs
  # before the parser, so bytes it removes from inside an escape would leave
  # the parser a different escape. CRuby's own pre-pass reads each escape
  # whole before it reaches the comment, and so does this one: `\u12(?#c)34`
  # is rejected as `\u12(?` rather than read as `\u1234`.
  x = Regexp::EXTENDED

  # \uXXXX is exactly four bytes after the u, whatever they are
  ["\\u12(?#c)34", "\\u(?#c){61}", "\\u006(?#c)1"].each do |pat|
    assert_raise_with_message(RegexpError, "invalid Unicode escape: /#{pat}/") do
      Regexp.new(pat)
    end
  end
  ["\\u12#c\n34", "\\u#c\n{61}"].each do |pat|
    assert_raise_with_message(RegexpError, "invalid Unicode escape: /#{pat}/x") do
      Regexp.new(pat, x)
    end
  end
  assert_equal ["ab"], Regexp.new("\\u0061(?#c)\\u0062").match("ab").to_a
  assert_equal ["ab"], Regexp.new("\\u{61}(?#c)\\u{62}").match("ab").to_a
  # a `\u{...}` list is one escape through its brace, and holds no comment
  assert_raise_with_message(RegexpError, "invalid Unicode list: /\\u{61(?#c)62}/") do
    Regexp.new("\\u{61(?#c)62}")
  end
  assert_raise_with_message(RegexpError, "invalid Unicode list: /\\u{61 #c\n62}/x") do
    Regexp.new("\\u{61 #c\n62}", x)
  end

  # \x is one or two hex digits and needs the one; a one-digit escape is
  # written at full width so that a digit the comment kept apart cannot
  # join it: `\x6(?#c)1` is `\x06` and `1`
  assert_raise_with_message(RegexpError, "invalid hex escape: /\\x(?#c)61/") do
    Regexp.new("\\x(?#c)61")
  end
  assert_raise_with_message(RegexpError, "invalid hex escape: /\\x#c\n61/x") do
    Regexp.new("\\x#c\n61", x)
  end
  assert_equal ["\x061"], Regexp.new("\\x6(?#c)1").match("\x061").to_a
  assert_nil Regexp.new("\\x6(?#c)1").match("a")
  assert_equal ["\x061"], Regexp.new("\\x6#c\n1", x).match("\x061").to_a
  assert_nil Regexp.new("\\x6#c\n1", x).match("a")
  assert_equal ["\x06"], Regexp.new("\\x6(?#c)").match("\x06").to_a
  assert_equal ["a1"], Regexp.new("\\x61(?#c)1").match("a1").to_a

  # \0 is up to two more octal digits, written at full width the same way
  assert_equal ["\x0061"], Regexp.new("\\0(?#c)61").match("\x0061").to_a
  assert_nil Regexp.new("\\0(?#c)61").match("1")
  assert_equal ["\x0061"], Regexp.new("\\0#c\n61", x).match("\x0061").to_a
  assert_equal ["\x061"], Regexp.new("\\06(?#c)1").match("\x061").to_a
  assert_equal ["\x061"], Regexp.new("\\06#c\n1", x).match("\x061").to_a
  assert_equal ["\x00"], Regexp.new("\\0(?#c)").match("\x00").to_a
  assert_equal ["\x001"], Regexp.new("\\000(?#c)1").match("\x001").to_a

  # inside a class the pass removes nothing, so `(?#c)` is five members and
  # `#c` two, and the escape is read as written
  assert_equal ["\x06"], Regexp.new("[\\x6(?#c)1]").match("\x06").to_a
  assert_equal ["("], Regexp.new("[\\x6(?#c)1]").match("(").to_a
  assert_equal ["#"], Regexp.new("[\\x6#c\n1]", x).match("#").to_a
end

assert("Regexp - empty pattern") do
  assert_true //.match?("")
  assert_true //.match?("abc")
end

assert("Regexp - nested captures") do
  md = /((a)(b))c/.match("abc")
  assert_equal "abc", md[0]
  assert_equal "ab", md[1]
  assert_equal "a", md[2]
  assert_equal "b", md[3]
end

assert("Regexp - non-greedy quantifiers") do
  need_backtracking_stack

  assert_equal "a", /a+?/.match("aaa")[0]
  assert_equal "", /a*?/.match("aaa")[0]
end

assert("Regexp - word boundary") do
  assert_equal "cat", /\bcat\b/.match("the cat sat")[0]
  assert_nil /\bcat\b/.match("concatenate")
end

assert("Regexp - a group the pattern ends inside says which it was") do
  # A group no ')' closes is `end pattern with unmatched parenthesis` in
  # CRuby, whichever of the (?...) forms opened it, and the plain one as
  # well.
  ["(", "(a", "(?:a", "(?=a", "(?!a", "(?<=a", "(?<!a", "(?>a", "(?i:a",
   "(?<a>x", "(?'a'x"].each do |src|
    assert_raise_with_message(RegexpError,
                              "end pattern with unmatched parenthesis: /#{src}/",
                              src) do
      Regexp.new(src)
    end
  end
end

assert("Regexp - a ')' that closes no group says which it was") do
  # The counterpart of the group that never closes: a ')' with no group
  # open is `unmatched close parenthesis` in CRuby. A comment group does
  # not nest, so the second ')' of (?#a(?#b)) is one of these.
  [")", "a)", "(a))", "(?#a(?#b))"].each do |src|
    assert_raise_with_message(RegexpError,
                              "unmatched close parenthesis: /#{src}/", src) do
      Regexp.new(src)
    end
  end
end

assert("Regexp - a (?...) prefix the pattern ends inside says which it was") do
  # `(?` opens a group the characters after it name. A pattern that ends
  # before they do is `end pattern in group` in CRuby, whether what stands
  # there is nothing at all, an option letter, or a comment group.
  ["(?", "(?i", "(?im", "(?i-", "(?-", "(?#", "(?#note"].each do |src|
    assert_raise_with_message(RegexpError, "end pattern in group: /#{src}/", src) do
      Regexp.new(src)
    end
  end
  # A character that names no group is that failure rather than this one,
  # whether or not the pattern goes on. `(?P<name>x)` is Python's spelling
  # of a named group and no group of Ruby's, so it is one of these too.
  ["(?z", "(?z)", "(?P", "(?P<a>x)"].each do |src|
    assert_raise_with_message(RegexpError, "undefined group option: /#{src}/", src) do
      Regexp.new(src)
    end
  end
  # `(?<` is the prefix CRuby answers for with the group instead: it ends
  # before the character that tells a lookbehind from a named group, and
  # both of those are a group that never closes.
  assert_raise_with_message(RegexpError,
                            "end pattern with unmatched parenthesis: /(?</") do
    Regexp.new("(?<")
  end
end

assert("Regexp - non-capturing group") do
  md = /(?:a)(b)/.match("ab")
  assert_equal "ab", md[0]
  assert_equal "b", md[1]
  assert_nil md[2]
end

assert("Regexp - an empty group takes a quantifier") do
  # `(?:)` emits no code, but it is an atom, and a repeat of what matches
  # empty matches empty, as CRuby compiles it. The scoped-option and demoted
  # plain spellings of an empty group are the same atom.
  assert_equal [""], /(?:)*/.match("").to_a
  assert_equal [""], /(?:)+/.match("").to_a
  assert_equal [""], /(?:){2}/.match("").to_a
  assert_equal [""], /(?:){0}/.match("").to_a
  assert_equal [""], /(?:)*?/.match("").to_a
  assert_equal [""], /(?:)*+/.match("").to_a
  assert_equal [""], /(?:)**/.match("").to_a
  assert_equal [""], /(?:(?:))*/.match("").to_a
  assert_equal [""], /(?:a{0})*/.match("aa").to_a
  assert_equal ["ab"], /a(?:)*b/.match("ab").to_a
  assert_equal ["b", ""], /((?:)*)b/.match("b").to_a
  assert_equal [""], /(?i:)*/.match("").to_a
  assert_equal ["a", "a"], /(?<n>a)()*/.match("a").to_a
  assert_equal [""], /(?:) * /x.match("").to_a
  # A `{` after it that spells no quantifier is a literal, as after any atom.
  assert_equal ["{a}"], /(?:){a}/.match("{a}").to_a
  # A quantifier after `(?i)` has no target: what follows the toggle is its
  # scope, and the quantifier stands at the beginning of it with no atom
  # before it.
  assert_raise_with_message(RegexpError,
                            "target of repeat operator is not specified: /(?i)*/") do
    Regexp.new("(?i)*")
  end
  assert_raise(RegexpError) { Regexp.new("(?:(?i))*(?i)+") }
end

assert("Regexp - atomic group (?>...)") do
  need_backtracking_stack
  # The body's first match is its only one: what follows cannot make it
  # give text back or take another branch, where a plain group can.
  assert_equal 0, /(?>a)+b/ =~ "aab"
  assert_equal 0, /(?:a+)ab/ =~ "aaab"
  assert_nil /(?>a+)ab/ =~ "aaab"
  assert_equal 0, /(?>a+)b/ =~ "aaab"
  assert_equal 0, /(?:a|ab)c/ =~ "abc"
  assert_nil /(?>a|ab)c/ =~ "abc"
  assert_equal 0, /(?>ab|a)c/ =~ "abc"

  # A repeated atomic group still gives back whole iterations: only the
  # inside of each one is closed to backtracking.
  assert_equal 0, /(?>a)+a/ =~ "aa"
  assert_equal 0, /(?>ab)+c/ =~ "ababc"
  assert_nil /(?>ab)+c/ =~ "abbc"
  assert_equal 0, /(?>a){2}b/ =~ "aab"
  assert_equal 1, /(?>ab)*?b/ =~ "abab"
  assert_equal 0, /(?>ab)|x/ =~ "x"

  # Once a group is closed, a failure after it fails the group as a whole,
  # even an alternation at the top of its body.
  assert_nil /(?>a(?>b|bc)|abcd)d/ =~ "abcd"
  # Before it is closed, its body backtracks as any other does, past an
  # inner atomic group that already closed.
  assert_equal "xy", /(?>(x|xy)(?>a)b)/.match("xyab")[1]
  # Sequential atomic groups at the same depth cut independently.
  assert_nil /(?>x(?>a)(?>b)y)/.match("xabz")
  assert_equal 0, /(?>x(?>a)(?>b)y)/ =~ "xaby"

  # A possessive repeat is an atomic group wrapped around what it repeats,
  # and it cuts as a group of its own: a failure after it does not open the
  # repeat to being skipped, whatever groups the repeated code holds.
  assert_nil /(?>a)?+a/.match("a")
  assert_nil /(?>a)*+a/.match("aa")
  assert_nil /(?:(?>a)b?)?+a/.match("a")
  assert_nil /(?:(?>a)?+)?+a/.match("a")
  assert_equal 0, /(?>a)?+b/ =~ "ab"
  assert_equal 0, /(?:(?>a)b?)?+c/ =~ "abc"
  # A failure inside the repeat, before its end, still fails only the inner
  # group, and the repeat is skipped as its `?` allows.
  assert_equal 1, /(?:(?>a)b)?+c/ =~ "ac"

  # A repetition whose body can match empty stops on its empty iteration
  # inside the group as anywhere else, and takes the group's exit; a
  # repetition of the group stops the same way, its lazy body still empty.
  assert_equal 0, /(?>(?:b*)+)/ =~ ""
  assert_equal 0, /(?>(?:a*)*)b/ =~ "aab"
  assert_equal "aab", /(?>(?:a*)*)b?/.match("aab")[0]
  assert_equal "aab", /(?:(?>a*))*b?/.match("aab")[0]
  assert_equal "ca", /ca(?>b??)+/.match("cab")[0]

  # Captures written inside the body stay when the group matches, and are
  # unset again when a cut fails the group.
  assert_equal "a", /(?>(a)+)b/.match("aab")[1]
  assert_nil /(?:(?>(a))x|a)b/.match("ab")[1]
  assert_equal "a", /(?>(a)|ab)b/.match("ab")[1]

  # Inside a lookaround, the cut stays inside it.
  assert_equal 0, /(?=(?>a+)b)a/ =~ "aab"
  assert_nil /(?=(?>a+)ab)a/ =~ "aab"
  assert_equal 0, /(?!(?>a)b)a/ =~ "aac"

  # Options toggled in the body end with it, as in any group.
  assert_equal 0, /(?>a(?i)x)b/ =~ "aXb"
  assert_nil /(?>a(?i)x)B/ =~ "aXb"

  # It reads back through to_s, and free-spacing applies to its body.
  assert_equal 0, Regexp.new(/(?>a)+b/.to_s) =~ "aab"
  assert_equal 0, Regexp.new("(?> a b )c", Regexp::EXTENDED) =~ "abc"
end

assert("Regexp - an atomic group the parser refuses") do
  assert_raise(RegexpError) { Regexp.new("(?>a") }
  assert_raise(RegexpError) { Regexp.new("(?>") }
  # not a fixed-length construct, so not allowed in a lookbehind
  assert_raise(RegexpError) { Regexp.new("(?<=(?>a))b") }
end

assert("Regexp - a lookbehind body of no fixed width says which it was") do
  # This engine rewinds a lookbehind by a width it measures at compile
  # time, so a body that has none is refused. CRuby refuses the same
  # bodies, and what it says of them is `invalid pattern in look-behind`.
  ["(?<=a+)b", "(?<=a*)b", "(?<=a?)b", "(?<=a{1,2})b", "(?<!a+)b",
   "(?<=(?>a))b"].each do |src|
    assert_raise_with_message(RegexpError,
                              "invalid pattern in look-behind: /#{src}/", src) do
      Regexp.new(src)
    end
  end
end

assert("Regexp - a named group makes plain groups non-capturing") do
  # Onigmo's ONIG_OPTION_DONT_CAPTURE_GROUP, which CRuby turns on once the
  # pattern declares a named group: (...) then groups without capturing.
  md = /(?<a>a)(b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "a"], md.to_a
  assert_equal ["a"], md.captures
  assert_nil md[2]
  assert_raise_with_message(IndexError, "index 2 out of matches") { md.begin(2) }
  assert_equal "a", md[:a]

  # a plain group written before the named group is demoted just the same,
  # which is what the pre-scan buys: the parser reaches it before it has seen
  # the declaration that decides the question
  md = /(a)(?<b>b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "b"], md.to_a
  assert_equal ["b"], md.captures
  assert_equal "b", md[1]
  assert_equal "b", md[:b]

  # the shrunken count is what $2, $+ and a \2 in a replacement read
  "ab" =~ /(?<a>a)(b)/
  assert_nil $2
  assert_equal "a", $+
  assert_equal "[]", "ab".sub(/(?<a>a)(b)/, '[\2]')

  # a "(?<" that is escaped or sits inside a character class opens no named
  # group either
  assert_equal ["(<a>b", "b"], /\(?<a>(b)/.match("(<a>b").to_a
  assert_equal ["(?<b", "b"], /[(?<a>]+(b)/.match("(?<b").to_a
  assert_equal ["a(?<b", "b"], /[[:alpha:](?<]+(b)/.match("a(?<b").to_a
  # nor one inside a (?#...) comment group, which is gone before the scan runs
  assert_equal ["b", "b"], /(?# (?<a>x )(b)/.match("b").to_a

  # in /x mode the scan reads the pattern after free-spacing and comments go
  assert_equal ["xy", "x"], /(?<a>x) # (b)
                             (y)/x.match("xy").to_a
  assert_equal ["y", "y"], /# (?<a>x)
                            (y)/x.match("y").to_a

  # a truncated "(?<" is still the parser's error, not a silent named group
  assert_raise(RegexpError) { Regexp.new("(?<") }

  # the scan runs on every pattern, so a truncated POSIX bracket reaches
  # skip_posix_bracket() without /x too, and is still the parser's error
  assert_raise_with_message(RegexpError, "premature end of char-class: /[[:alpha/") do
    Regexp.new("[[:alpha")
  end
end

assert("Regexp - a lookbehind opener is no named group") do
  need_backtracking_stack
  # (?<= and (?<! open a lookbehind, not a named group, so they demote the
  # plain group after them no more than the spellings above do.
  assert_equal ["b", "b"], /(?<=a)(b)/.match("ab").to_a
  assert_equal ["b", "b"], /(?<!x)(b)/.match("ab").to_a
end

assert("Regexp - the comment pass, the named-group scan and the parser skip the same constructs") do
  # Three readers step over the pattern's escapes, character classes and
  # POSIX brackets: the comment pass, which runs when the pattern holds a
  # `#`; the named-group pre-scan; and the parser, which under /x skips
  # whitespace between tokens and none inside those constructs. Every row
  # below is read by the pre-scan and the parser, and the rows with a `#`
  # by the pass as well. A rule lost from the pass takes a `#` inside a
  # class for a comment and eats the rest of the pattern; the same rule
  # lost from the pre-scan turns a bracketed "(?<" into a phantom named
  # group, which demotes the plain (b) that follows and shortens the match;
  # lost from the parser it strips a space that is a member. Either way the
  # row fails.
  x = Regexp::EXTENDED

  # an escape pair hides the '(' from the pre-scan and the parser
  assert_equal ["(<a>b", "b"],
               Regexp.new('\(?<a> (b)', x).match("(<a>b").to_a

  # a character class hides "(?<" and keeps its own spaces and its `#`
  assert_equal ["(?< #b", "b"],
               Regexp.new('[(?< a>#]+ (b)', x).match("(?< #b").to_a

  # a ']' written first is a member, so the class runs past it; no `#` in
  # these two, since CRuby's own pre-pass does not know this rule and takes
  # a `#` after `[]` for a comment, so it cannot settle what the pass makes
  # of one
  assert_equal ["] (?<b", "b"],
               Regexp.new('[] (?<a>]+(b)', x).match("] (?<b").to_a
  assert_equal ["zzb", "b"],
               Regexp.new('[^] (?<a>]+(b)', x).match("zzb").to_a

  # a POSIX bracket's ']' does not close the class either
  assert_equal ["a #(?<b", "b"],
               Regexp.new('[[:alpha:] #(?<a>]+(b)', x).match("a #(?<b").to_a

  # a `\u{...}` list is one escape, so its separating space survives /x and
  # its bytes are not read as pattern syntax
  assert_equal ["ab"], Regexp.new('\u{61 62}', x).match("ab").to_a
  assert_equal ["abcd", "c", "d"],
               Regexp.new('\u{61 62}(c)(d)', x).match("abcd").to_a
  assert_equal ["abcd", "c"],
               Regexp.new('\u{61 62}(?<n>c)(d)', x).match("abcd").to_a
  assert_equal ["abcd", "c", "d"],
               Regexp.new('[\u{61 62}]+(c)(d)', x).match("abcd").to_a

  # With no `#` the pass does not run, and with no whitespace /x gives the
  # parser nothing to skip, so both compiles read the same bytes and must
  # agree: /x by itself changes nothing about where a class or an escape
  # ends.
  [
    ['\(?<a>(b)',            "(<a>b"],
    ['[(?<a>]+(b)',          "(?<b"],
    ['[](?<a>]+(b)',         "](?<b"],
    ['[^](?<a>]+(b)',        "zzb"],
    ['[[:alpha:](?<a>]+(b)', "a(?<b"],
    ['[\]](?<a>x)(y)',       "]xy"],
    ['\\\\(?<a>x)(y)',       "\\xy"],
    ['\u{61}(?<n>b)(c)',     "abc"],
    ['[\u{61}]+(?<n>b)(c)',  "abc"],
    ['(a)(?<b>b)',           "ab"],
    ["\\(?'a'(b)",           "('a'b"],
    ["[(?'a]+(b)",           "(?'b"],
  ].each do |pat, subject|
    assert_equal Regexp.new(pat).match(subject).to_a,
                 Regexp.new(pat, x).match(subject).to_a
  end

  # A `\u{...}` list is not a place a named group can be declared, so the
  # scan must not read "(?<" out of one. The list here is malformed either
  # way and the pattern is rejected either way, but which error comes first
  # depends on the scan: taking the "(?<" for a declaration turns on the
  # demotion that rejects the leading \1 before the parser ever reaches the
  # bad list. CRuby reports the list, and so does the scan that treats
  # `\u{...}` as one escape.
  assert_raise_with_message(RegexpError,
                            "invalid Unicode list: /\\1\\u{(?<a>/") do
    Regexp.new("\\1\\u{(?<a>")
  end
end

assert("Regexp - case in when") do
  result = case "hello123"
           when /\d+/ then "has digits"
           else "no digits"
           end
  assert_equal "has digits", result
end

assert("Regexp - backreference \\1") do
  need_backtracking_stack
  # match repeated word
  md = /(\w+) \1/.match("hello hello world")
  assert_equal "hello hello", md[0]
  assert_equal "hello", md[1]
end

assert("Regexp - backreference no match") do
  need_backtracking_stack
  assert_nil /(\w+) \1/.match("hello world")
end

assert("Regexp - backreference under /i") do
  need_backtracking_stack
  # The comparison against the captured text has to fold case too, otherwise
  # `\1` stays case-sensitive while the rest of the pattern does not.
  assert_equal "aA", /(a)\1/i.match("aA")[0]
  assert_equal "Hello hELLO", /(\w+) \1/i.match("Hello hELLO world")[0]
  assert_nil /(a)\1/i.match("ab")
end

assert("Regexp - named captures") do
  md = /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.match("2026-03-21")
  assert_equal "2026", md[:year]
  assert_equal "03", md[:month]
  assert_equal "21", md[:day]
  assert_equal "2026", md["year"]
end

assert("Regexp - a named group can be written (?'name'...)") do
  # A definition has two spellings, and \k already read both, so the parser
  # used to accept a reference to a name it refused to introduce.
  md = /(?'x'a)/.match("a")
  assert_equal ["a", "a"], md.to_a
  assert_equal "a", md[:x]
  assert_equal ["year", "month"], /(?'year'\d+)-(?'month'\d+)/.names
  assert_equal({"year" => [1], "month" => [2]},
               /(?'year'\d+)-(?'month'\d+)/.named_captures)

  # the two spellings write into one registry: a name given twice is reported
  # once however each of them was spelled
  assert_equal ["t"], /(?<t>\w)(?'t'\w)/.names
  assert_equal ["xy", "x", "y"], /(?<a>x)(?'b'y)/.match("xy").to_a

  # a name runs to its own terminator, so the other spelling's terminator is
  # a member of it rather than the end
  assert_equal ["a>b"], /(?'a>b'x)/.names
  assert_equal ["a'b"], /(?<a'b>x)/.names

  # nesting, quantifiers and /i are the group's own business either way
  assert_equal ["ab", "ab", "b"], /(?'o'a(?'i'b))/.match("ab").to_a
  assert_equal ["abab", "ab"], /(?'a'ab)+/.match("abab").to_a
  assert_equal ["AB", "AB"], /(?'a'ab)/i.match("AB").to_a

  # a name is still required, and still has to be terminated
  assert_raise(RegexpError) { Regexp.new("(?''x)") }
  assert_raise(RegexpError) { Regexp.new("(?'x") }
  assert_raise(RegexpError) { Regexp.new("(?'") }
end

assert("Regexp - either spelling of \\k reaches either spelling of a name") do
  need_backtracking_stack
  # The reference half of the block above, which the declaration alone
  # cannot show: a group written in one spelling is reached by the other.
  assert_equal "aa", "aa".match(/(?'n'\w)\k<n>/)[0]
  assert_equal "aa", "aa".match(/(?<n>\w)\k'n'/)[0]
  assert_equal "aa", "aa".match(/(?'n'\w)\k'n'/)[0]
end

assert("Regexp - a (?'name'...) group demotes plain groups too") do
  # The pre-scan settles the demotion before the parser runs, so it has to
  # know both spellings: reading "(?<" alone left /(a)(?'b'b)/ numbering the
  # plain group that the declaration demotes.
  md = /(?'a'a)(b)/.match("ab")
  assert_equal 2, md.size
  assert_equal ["ab", "a"], md.to_a
  assert_nil md[2]

  md = /(a)(?'b'b)/.match("ab")
  assert_equal ["ab", "b"], md.to_a
  assert_equal "b", md[:b]

  assert_equal "[]", "ab".sub(/(?'a'a)(b)/, '[\2]')

  # and the numbers the declaration took away cannot be referred to
  msg = "numbered backref/call is not allowed. (use name)"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?'b'b)\\1/") do
    Regexp.new("(a)(?'b'b)\\1")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?'b'b)\\k<1>/") do
    Regexp.new("(a)(?'b'b)\\k<1>")
  end

  # an escaped or bracketed "(?'" declares nothing, so the plain group that
  # follows keeps its number
  assert_equal ["('a'b", "b"], /\(?'a'(b)/.match("('a'b").to_a
  assert_equal ["(?'b", "b"], /[(?'a]+(b)/.match("(?'b").to_a
end

assert("Regexp#named_captures") do
  assert_equal({"year" => [1], "month" => [2], "day" => [3]},
               /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.named_captures)
  assert_equal({}, /\d+/.named_captures)

  # the returned Hash is a copy; mutating it must not affect a later call
  re = /(?<a>x)/
  re.named_captures["a"] = 99
  assert_equal({"a" => [1]}, re.named_captures)
end

assert("Regexp#names") do
  assert_equal ["year", "month", "day"],
               /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.names
  assert_equal [], /\d+/.names

  # a name that is registered twice is reported once, as in CRuby
  assert_equal ["tag"], /(?<tag>\w+)-(?<tag>\w+)/.names
end

assert("Regexp - empty group name") do
  # (?<>x) used to compile and answer to "", and in /x mode the stored name
  # pointed into the preprocessing buffer the compiler frees on the way out.
  assert_raise(RegexpError) { Regexp.new("(?<>x)") }
  assert_raise(RegexpError) { Regexp.new("(?<>x) ", Regexp::EXTENDED) }
  assert_raise(RegexpError) { Regexp.new("(?<>x)\\k<>") }
  assert_raise(RegexpError) { Regexp.new("\\k<>") }
  assert_raise(RegexpError) { Regexp.new("\\k''") }
end

assert("Regexp - a lookbehind is no named group with an empty name") do
  need_backtracking_stack
  # `(?<` opens one only where `=` or `!` does not follow, so the refusal
  # above reaches no lookbehind.
  assert_equal "b", Regexp.new("(?<=a)b").match("ab")[0]
  assert_nil Regexp.new("(?<!a)b").match("ab")
end

assert("Regexp - group name longer than a uint16 length") do
  # The name length used to live in a uint16_t and was truncated with a cast,
  # so (uint16_t)65538 == 2 made this group answer to "ab" instead of to the
  # name it was given.
  long = "ab" + "A" * 65536
  re = Regexp.new("(?<#{long}>x)")
  assert_equal [long], re.named_captures.keys
  assert_equal "x", re.match("x")[long]
  assert_raise(IndexError) { re.match("x")["ab"] }

  # two names that shared a truncation stay distinct, and the two APIs that
  # resolve a name agree on which group it names
  re = Regexp.new("(?<ab>x)(?<#{long}>y)")
  md = re.match("xy")
  assert_equal "x", md["ab"]
  assert_equal "y", md[long]
  assert_equal({ "ab" => "x", long => "y" }, md.named_captures)

  # a name of exactly 65536 bytes is not the empty name
  z = "Z" * 65536
  assert_equal [z], Regexp.new("(?<#{z}>x)").named_captures.keys
end

assert("Regexp - \\k binds to the group a long name was written on") do
  need_backtracking_stack
  # The reference half of the truncation above: two names that shared a
  # truncated length must not share a group here either.
  long = "ab" + "A" * 65536
  re = Regexp.new("(?<ab>x)(?<#{long}>y)\\k<#{long}>")
  assert_nil re.match("xyx")
  assert_equal "xyy", re.match("xyy")[0]
end

assert("Regexp - named backreference \\k") do
  need_backtracking_stack
  assert_equal "aa", "aa".match(/(?<n>\w)\k<n>/)[0]
  assert_equal "abba", "abba".match(/(?<a>.)(?<b>.)\k<b>\k<a>/)[0]
  assert_equal "1212", "1212".match(/(?<x>\d+)\k'x'/)[0]
  assert_nil "ab".match(/(?<n>\w)\k<n>/)
  # numeric and relative forms
  assert_equal "aa", "aa".match(/(a)\k<1>/)[0]
  assert_equal "abba", "abba".match(/(.)(.)\k<-1>\k<-2>/)[0]
  # /i folds the comparison against the captured text
  assert_equal "aA", "aA".match(/(?<n>a)\k<n>/i)[0]
  assert_nil "ab".match(/(?<n>a)\k<n>/i)
end

assert("Regexp - a \\k reference names a group the pattern has") do
  # an unknown name is an error
  assert_raise(RegexpError) { Regexp.new("\\k<missing>") }

  # once the pattern has a named group a numbered backreference is rejected,
  # whatever its spelling, because there is no longer a number to reach
  msg = "numbered backref/call is not allowed. (use name)"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\1/") do
    Regexp.new("(a)(?<b>b)\\1")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<1>/") do
    Regexp.new("(a)(?<b>b)\\k<1>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<-1>/") do
    Regexp.new("(a)(?<b>b)\\k<-1>")
  end
  # the relative form resolves against every group the pattern has opened, the
  # plain ones it demotes included, so this one names group 1 and is refused
  # for being numbered rather than for naming no group
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<-2>/") do
    Regexp.new("(a)(?<b>b)\\k<-2>")
  end
  assert_raise(RegexpError) { Regexp.new("(?<b>b)\\k'1'") }
end

assert("Regexp - numeric \\k backreference out of int range") do
  # The digit accumulator is an int with no bound, so 4294967297 used to wrap
  # to 1 and bind this backreference to group 1 instead of raising.
  msg = "too big number"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<4294967297>/") do
    Regexp.new("(a)\\k<4294967297>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<-4294967297>/") do
    Regexp.new("(a)\\k<-4294967297>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(b)\\k<4294967298>/") do
    Regexp.new("(a)(b)\\k<4294967298>")
  end
end

assert("Regexp - \\k group reference errors say which failure it was") do
  # A \k reference fails in four ways and CRuby gives each its own message.
  # They used to collapse into one, so a pattern that misspelled a name and a
  # pattern that named a group it never opened read the same.

  # a name that is neither `-`? digits nor a name any group carries
  assert_raise_with_message(RegexpError, "invalid group name <1x>: /(a)\\k<1x>/") do
    Regexp.new("(a)\\k<1x>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <-x>: /(a)\\k<-x>/") do
    Regexp.new("(a)\\k<-x>")
  end
  # `-` with no digits behind it
  assert_raise_with_message(RegexpError, "invalid group name <->: /(a)\\k<->/") do
    Regexp.new("(a)\\k<->")
  end
  # group 0 is the whole match, which \k cannot name in either spelling.
  # The message quotes the name in <> whichever delimiter wrote it.
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)\\k<0>/") do
    Regexp.new("(a)\\k<0>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <-0>: /(a)\\k<-0>/") do
    Regexp.new("(a)\\k<-0>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)\\k'0'/") do
    Regexp.new("(a)\\k'0'")
  end

  # the name is read whole before it is converted, so digits followed by
  # anything else is a malformed name and never an oversized number
  assert_raise_with_message(RegexpError,
                            "invalid group name <99999999999999999999x>: /(a)\\k<99999999999999999999x>/") do
    Regexp.new("(a)\\k<99999999999999999999x>")
  end

  # a number past the bound, either sign
  assert_raise_with_message(RegexpError, "too big number: /(a)\\k<2147483648>/") do
    Regexp.new("(a)\\k<2147483648>")
  end
  assert_raise_with_message(RegexpError, "too big number: /(a)\\k<-2147483648>/") do
    Regexp.new("(a)\\k<-2147483648>")
  end

  # a number within the bound that names no group: a different message from
  # the one above, and the bound is where they part
  msg = "invalid backref number/name"
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<2147483647>/") do
    Regexp.new("(a)\\k<2147483647>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<5>/") do
    Regexp.new("(a)\\k<5>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k'5'/") do
    Regexp.new("(a)\\k'5'")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)\\k<-5>/") do
    Regexp.new("(a)\\k<-5>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(b)\\k<-3>/") do
    Regexp.new("(a)(b)\\k<-3>")
  end
  # a pattern that has opened no group at all
  assert_raise_with_message(RegexpError, "#{msg}: /\\k<-1>/") do
    Regexp.new("\\k<-1>")
  end

  # a name no group carries
  assert_raise_with_message(RegexpError,
                            "undefined name <_nope> reference: /(a)\\k<_nope>/") do
    Regexp.new("(a)\\k<_nope>")
  end
  assert_raise_with_message(RegexpError,
                            "undefined name <_nope> reference: /(a)\\k'_nope'/") do
    Regexp.new("(a)\\k'_nope'")
  end
  # only `-` leads a number, so `+1` is a name and fails as one
  assert_raise_with_message(RegexpError,
                            "undefined name <+1> reference: /(a)\\k<+1>/") do
    Regexp.new("(a)\\k<+1>")
  end

  # a named pattern refuses a numbered reference, but only once the name is
  # read as a number at all: a malformed one and an oversized one are still
  # reported for what they are
  assert_raise_with_message(RegexpError, "invalid group name <1x>: /(a)(?<b>b)\\k<1x>/") do
    Regexp.new("(a)(?<b>b)\\k<1x>")
  end
  assert_raise_with_message(RegexpError, "invalid group name <0>: /(a)(?<b>b)\\k<0>/") do
    Regexp.new("(a)(?<b>b)\\k<0>")
  end
  assert_raise_with_message(RegexpError,
                            "too big number: /(a)(?<b>b)\\k<99999999999999999999>/") do
    Regexp.new("(a)(?<b>b)\\k<99999999999999999999>")
  end
  assert_raise_with_message(RegexpError,
                            "numbered backref/call is not allowed. (use name): /(a)(?<b>b)\\k<5>/") do
    Regexp.new("(a)(?<b>b)\\k<5>")
  end
  # the relative form is resolved before that refusal, so one past the groups
  # the pattern has is out of range where an absolute one is refused: where an
  # absolute reference points is only settled once the parse is done, and a
  # relative one is settled where it stands
  assert_raise_with_message(RegexpError, "#{msg}: /(?<b>b)\\k<-2>/") do
    Regexp.new("(?<b>b)\\k<-2>")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(a)(?<b>b)\\k<-3>/") do
    Regexp.new("(a)(?<b>b)\\k<-3>")
  end

  # The name is a length-counted slice of the pattern, so a name holding a NUL
  # is quoted whole. CRuby builds these messages through a C string and stops
  # at the NUL, reporting `undefined name <a` for the first of the two.
  assert_raise_with_message(RegexpError,
                            "undefined name <a\0b> reference: /(a)\\k<a\0b>/") do
    Regexp.new("(a)\\k<a\0b>")
  end
  assert_raise_with_message(RegexpError,
                            "invalid group name <1\0>: /(a)\\k<1\0>/") do
    Regexp.new("(a)\\k<1\0>")
  end
end

assert("Regexp - a \\k reference reads leading zeros as digits") do
  need_backtracking_stack
  # Not a malformed name, which is the failure the block above pins: <01> is
  # group 1 and <-01> is the group one back, and the reference matching is
  # what says the name resolved.
  assert_equal "aa", "aa".match(Regexp.new("(a)\\k<01>"))[0]
  assert_equal "aa", "aa".match(Regexp.new("(a)\\k<-01>"))[0]
end

assert("Regexp - a group name may not be a number") do
  # A definition names a group, it never numbers one: the number spelling
  # belongs to a reference, and CRuby refuses a leading digit or `-` where a
  # group is declared. (?<1>x) used to be accepted and left the group
  # unreachable, since \k<1> reads digits as a number and a named pattern
  # refuses a numbered backreference.
  msg = "invalid group name"
  assert_raise_with_message(RegexpError, "#{msg} <1>: /(?<1>c)/") do
    Regexp.new("(?<1>c)")
  end
  assert_raise_with_message(RegexpError, "#{msg} <1a>: /(?<1a>c)/") do
    Regexp.new("(?<1a>c)")
  end
  assert_raise_with_message(RegexpError, "#{msg} <-a>: /(?<-a>c)/") do
    Regexp.new("(?<-a>c)")
  end
  # both spellings declare, so both refuse, and the message quotes in <>
  # whichever delimiter wrote the name
  assert_raise_with_message(RegexpError, "#{msg} <1a>: /(?'1a'c)/") do
    Regexp.new("(?'1a'c)")
  end
  assert_raise_with_message(RegexpError, "#{msg} <-1>: /(?'-1'c)/") do
    Regexp.new("(?'-1'c)")
  end

  # only the first byte carries the number spelling: a digit or a `-` further
  # in is a name character like any other, as a space is
  assert_equal ["a1"], Regexp.new("(?<a1>c)").names
  assert_equal ["a-b"], Regexp.new("(?<a-b>c)").names
  assert_equal ["a b"], Regexp.new("(?<a b>c)").names
end

assert("Regexp - a group name may not hold a ')'") do
  # CRuby's fetch_name() ends the name at a ')' and reports a name no
  # delimiter ended as invalid. mruby took every byte up to the delimiter, so
  # patterns CRuby rejects compiled here, with a ')' held as a name character
  # no other engine reads as one.
  msg = "invalid group name"
  assert_raise_with_message(RegexpError, "#{msg} <a)b>c)>: /(?<a)b>c)/") do
    Regexp.new("(?<a)b>c)")
  end
  assert_raise_with_message(RegexpError, "#{msg} <a)b'c)>: /(?'a)b'c)/") do
    Regexp.new("(?'a)b'c)")
  end
  # the scan stops at the ')', so the name is quoted to the end of the
  # pattern and an unterminated one is refused for the ')' rather than for
  # its missing delimiter
  assert_raise_with_message(RegexpError, "#{msg} <a)b>: /(?<a)b/") do
    Regexp.new("(?<a)b")
  end

  # the reference arm reads a name the same way and stops the same way
  assert_raise_with_message(RegexpError, "#{msg} <a)b>>: /(?<a>c)\\k<a)b>/") do
    Regexp.new("(?<a>c)\\k<a)b>")
  end
  assert_raise_with_message(RegexpError, "#{msg} <a)b'>: /(?<a>c)\\k'a)b'/") do
    Regexp.new("(?<a>c)\\k'a)b'")
  end

  # the first byte is exempt in both arms, as it is in CRuby: a lone ')' is a
  # name a group can carry and a reference can reach
  assert_equal [")"], Regexp.new("(?<)>c)").names
end

assert("Regexp - a name a group carries is a name a reference reaches") do
  need_backtracking_stack
  # What the two blocks above leave to the parser, this one runs: \k resolves
  # the name the group was declared with, whichever delimiter wrote either
  # and whatever byte the name holds past its first.
  assert_equal "cc", "cc".match(Regexp.new("(?<a b>c)\\k<a b>"))[0]
  assert_equal "cc", "cc".match(Regexp.new("(?<)>c)\\k<)>"))[0]
  assert_equal "cc", "cc".match(Regexp.new("(?')'c)\\k')'"))[0]
end

assert("Regexp - named captures survive /x preprocessing") do
  # Regression: with /x, mrb_re_compile freed the rewritten buffer that
  # named_captures[i].name pointed into.
  re = /(?<n>\d+) # comment
       \s* (?<u>\w+) /x
  m = re.match("42 px")
  assert_equal "42", m[:n]
  assert_equal "px", m[:u]
end

assert("Regexp - named captures survive source string mutation") do
  # Regression: name pointer used to alias RSTRING_PTR of the source.
  s = String.new("(?<key>\\d+)")
  re = Regexp.new(s)
  s.replace("X" * 10000)   # force buffer reallocation
  m = re.match("abc 123 def")
  assert_equal "123", m[:key]
end

assert("Regexp - positive lookahead (?=...)") do
  need_backtracking_stack
  md = /\w+(?=@)/.match("user@host")
  assert_equal "user", md[0]
end

assert("Regexp - negative lookahead (?!...)") do
  need_backtracking_stack
  md = /\d+(?!%)/.match("100%")
  assert_equal "10", md[0]
end

assert("Regexp - lookahead does not consume") do
  need_backtracking_stack
  md = /foo(?=bar)/.match("foobar")
  assert_equal "foo", md[0]
  assert_nil /foo(?=baz)/.match("foobar")
end

assert("Regexp - positive lookbehind (?<=...)") do
  need_backtracking_stack
  md = Regexp.new("(?<=@)\\w+").match("user@host")
  assert_equal "host", md[0]
  assert_nil Regexp.new("(?<=@)\\w+").match("user_host")
end

assert("Regexp - negative lookbehind (?<!...)") do
  need_backtracking_stack
  md = Regexp.new("(?<!\\d)px").match("12px auto")
  assert_nil md  # preceded by digit
  md = Regexp.new("(?<!\\d)em").match("12px 1.5em auto")
  assert_nil md  # preceded by digit
  md = Regexp.new("(?<!\\d)px").match("top px")
  assert_equal "px", md[0]
end

assert("Regexp - lookbehind with literal string") do
  need_backtracking_stack
  md = Regexp.new("(?<=foo)bar").match("foobar")
  assert_equal "bar", md[0]
  assert_nil Regexp.new("(?<=foo)bar").match("bazbar")
end

assert("Regexp - lookbehind at string start") do
  need_backtracking_stack
  # lookbehind should fail if not enough text before
  assert_nil Regexp.new("(?<=abc)d").match("d")
  # but should work at correct position
  md = Regexp.new("(?<=abc)d").match("abcd")
  assert_equal "d", md[0]
end

assert("Regexp - negative lookbehind at string start") do
  need_backtracking_stack
  # negative lookbehind succeeds when not enough text before
  md = Regexp.new("(?<!x)a").match("a")
  assert_equal "a", md[0]
end

assert("Regexp - a capture inside a lookaround is undone with the lookaround") do
  need_backtracking_stack
  # A lookaround's sub-pattern used to run in a call of its own, so once it
  # had matched, the frames that could undo what it captured were gone, and
  # backtracking past the lookaround left the capture written. A plain
  # group's frames stay while the text after it runs, and undo.
  assert_nil /(?:(a)b|)/.match("a")[1]
  assert_nil /(?:(?=(a))b|)/.match("a")[1]
  assert_nil /(?=(a))b|/.match("a")[1]
  assert_nil /(?:(?=(a))b)?/.match("a")[1]
  assert_nil /(?:(?=(a))b)*/.match("a")[1]
  assert_nil /(?:(?<=(a))b|)c/.match("ac")[1]
  # A negative lookaround's sub-pattern matching is the assertion failing,
  # and what it captured on the way is undone with it.
  assert_nil /(?!(a))|/.match("a")[1]
  assert_nil /(?!(a))*/.match("a")[1]
  assert_nil /(?!(a))?/.match("a")[1]
  assert_equal ["a", nil], /(?!(a)b)a\1?/.match("ac").to_a
  # A backreference to the leaked group consumed text, and the match itself
  # changed.
  assert_nil /(?:(?=(a))b|)\1/ =~ "aa"
  assert_nil /(?:(?!(a))|a)\1/ =~ "aa"
  assert_equal "ab", /(?:(?!(a))|a)\1?b/.match("aab")[0]
  assert_nil /(?:(?=(a))a|b)\1/ =~ "ba"
  # What a lookaround that holds captured stays, as before.
  assert_equal "a", /(?=(a))a/.match("a")[1]
  assert_equal ["b", "a"], /(?<=(a))b/.match("ab").to_a
  assert_equal ["ab", "a"], /(?=(a|ab))\1b/.match("abb").to_a
  # A positive lookaround's sub-pattern matches once: the text after it
  # failing does not send it back for another branch, as in Onigmo.
  assert_nil /(?=(a|ab))\1c/ =~ "abc"
  assert_equal ["abc", nil], /(?:(?=(a|ab))\1c|abc)/.match("abc").to_a
  # The text after a lookaround runs inside its sub-pattern's frames, so
  # an atomic group's cut from that text passes through the lookaround on
  # its way to the group, and the lookaround does not absorb it as its own.
  assert_nil /(?>(?=a)ab|a)b/ =~ "ab"
  assert_nil /(?>(?!x)ab|a)b/ =~ "ab"
  assert_equal 0, /(?>(?=a)ab|a)c/ =~ "abc"
  # A possessive repeat wrapped around a lookaround is numbered apart from
  # it, as from an atomic group, so its cut is not read as the lookaround's.
  assert_nil /(?:(?=a)a)?+a/.match("a")
  assert_nil /(?:(?=a)a)*+a/.match("aa")
  assert_nil /(?:a(?<=a))?+a/.match("a")
  assert_nil /(?:(?!b)a)?+a/.match("a")
  assert_equal 0, /(?:(?=a)a)?+b/ =~ "ab"
  # A repeat around a positive lookaround re-enters its sub-pattern while
  # the frames of the run before are still up, so a loop inside it starts
  # over with the record of where the run before left off still live; the
  # first iteration of an e+ reads that record without having written it,
  # and must not take it for its own: `(b|)+` on "b" goes round twice, and
  # the empty second time is what it leaves in the group.
  assert_equal ["", ""], /(?=(b|)+)+/.match("b").to_a
  assert_equal ["", ""], /(?=(b|)+a)+/.match("ba").to_a
  assert_equal ["", ""], /(?=(b|)+)+\1/.match("b").to_a
  assert_equal ["", "", ""], /(?=(b|)+(?=(a|)+))+/.match("ba").to_a
  assert_equal ["", ""], /(?=(a|)+)++/.match("abab").to_a
end

assert("Regexp - lookbehind over a class that can match a multibyte character") do
  # A class consumes exactly one character whatever its members are, so the
  # rewind steps back that many characters rather than assuming a byte each.
  # A build that reads its strings by byte has one byte per character, so it
  # has nothing here to tell the two rewinds apart.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "x", "Āx".match(/(?<=[Ā])x/)[0]
  assert_nil "ax".match(/(?<=[Ā])x/)
  assert_equal "x", "Āx".match(/(?<=[Ā-ă])x/)[0]
  assert_equal "x", "ax".match(/(?<=[aĀ])x/)[0]
  assert_equal "x", "Āx".match(/(?<=[aĀ])x/)[0]
  assert_equal "x", "ĀĀx".match(/(?<=[Ā]{2})x/)[0]
  assert_nil "aĀx".match(/(?<=[Ā]{2})x/)
  assert_nil "Āx".match(/(?<=[Ā]{2})x/)
  # a negated class admits non-ASCII, whatever its members are
  assert_nil "あx".match(/(?<=[^あ])x/)
  assert_equal "x", "ax".match(/(?<=[^あ])x/)[0]
  assert_nil "Āb".match(/(?<![Ā])b/)
  assert_equal "b", "ab".match(/(?<![Ā])b/)[0]
  assert_equal "b", "ab".match(/(?<![^a])b/)[0]
  assert_nil "あb".match(/(?<![^a])b/)
  # the uppercase shorthands carry the same catch-all
  assert_equal "x", "aあx".match(/(?<=a\W)x/)[0]
  assert_nil "aax".match(/(?<=a\W)x/)
  assert_equal "x", "ああx".match(/(?<=\W\W)x/)[0]
  assert_equal "x", "Āx".match(/(?<=\D)x/)[0]
  assert_equal "x", "Āx".match(/(?<=\S)x/)[0]
  # dot is one character by the same argument
  assert_equal "x", "ax".match(/(?<=.)x/)[0]
  assert_equal "x", "Āx".match(/(?<=.)x/)[0]
  assert_nil "x".match(/(?<=.)x/)
end

assert("Regexp - lookbehind against a binary subject rewinds by bytes") do
  # A binary subject advances one byte at a time, so the same compiled
  # pattern rewinds by its byte count there: two for the literal Ā, and one
  # for a class, which is handed the raw byte as its codepoint. What this
  # contrasts with is the character rewind, which a build reading its strings
  # by byte does not have.
  skip unless __ENCODING__ == "UTF-8"
  bin = "Āx".b
  assert_equal "x", bin.match(/(?<=Ā)x/)[0]
  assert_nil bin.match(/(?<=[Ā])x/)
  assert_equal "x", bin.match(/(?<=[\x80])x/)[0]
  assert_equal "x", bin.match(/(?<=.)x/)[0]
end

assert("Regexp - lookbehind measures bytes that spell no character") do
  need_backtracking_stack
  # A byte no lead byte reaches is a character of its own, which is what the
  # rewind steps back over, so the width has to count it as one. Counting the
  # lead bytes of the run alone made such a byte part of the character before
  # it, rewound too little, and the lookbehind then failed on text it
  # describes, or succeeded where the negative form describes it.
  #
  # A subject whose bytes spell no character is refused wherever an encoding
  # reads them, so the character rewind is asked of the build that reads none,
  # and the byte rewind below puts the same question to the same bytes in
  # either build.
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { "\x80ab" =~ /(?<=\x80a)b/ }
    assert_raise(ArgumentError) { "\x80ab" =~ /(?<!\x80a)b/ }
    assert_raise(ArgumentError) { "\xE3\x81ab" =~ /(?<=\xE3\x81a)b/ }
    assert_raise(ArgumentError) { "\x80ab" =~ Regexp.new("(?<=\x80a)b") }
  else
    assert_equal 2, ("\x80ab" =~ /(?<=\x80a)b/)
    assert_nil ("\x80ab" =~ /(?<!\x80a)b/)
    # a sequence cut short spells no character either, so each of its bytes is
    # one: E3 leads three bytes and only two follow it here
    assert_equal 3, ("\xE3\x81ab" =~ /(?<=\xE3\x81a)b/)
    # the same bytes written into the pattern rather than escaped
    assert_equal 2, ("\x80ab" =~ Regexp.new("(?<=\x80a)b"))
  end
  # a subject that spells characters throughout is read in either build
  assert_nil ("ab" =~ /(?<=\x80a)b/)
  # a byte-indexed subject counts the same bytes, and rewinds by them
  assert_equal 2, ("\x80ab".b =~ Regexp.new("(?<=\x80a)b"))
  assert_equal 3, ("\xE3\x81ab".b =~ Regexp.new("(?<=\xE3\x81a)b"))
  # a whole character is still one whatever its byte count
  assert_equal "b", "Āab".match(/(?<=Āa)b/)[0]
  assert_nil "aab".match(/(?<=Āa)b/)
end

assert("Regexp - lookbehind over a class holding a byte that spells no character") do
  need_backtracking_stack
  # Two things meet here that were built apart: a character class may hold a
  # byte that starts no character, and the rewind steps back over characters.
  # They agree on the unit already: such a byte is a character of its own,
  # which is the step the forward match takes for it too, so a class holding
  # one is measured as one character wide, the same as any other class.
  #
  # A subject whose bytes spell no character is refused wherever an encoding
  # reads them, so the stray byte is put to a binary subject, which rewinds by
  # bytes in either build.
  assert_equal "x", "\xB5x".b.match(/(?<=[\xB5])x/)[0]
  assert_equal 1, ("\xB5x".b =~ /(?<=[\xB5])x/)
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { "\xB5x" =~ /(?<=[\xB5])x/ }
    # the byte written into a class is still asked about a character: Ā is
    # C4 80, and the rewind steps back over the whole of it, so the class is
    # handed U+0100 rather than either of its bytes
    assert_nil ("Āx" =~ /(?<=[\x80])x/)
    assert_nil ("Āx" =~ /(?<=[\xC4])x/)
    assert_equal 1, ("Āx" =~ /(?<=[Ā])x/)
  else
    # a build that reads its strings by byte has one character per byte, so
    # the same class does see the continuation byte, and only that one
    assert_equal 1, ("\xB5x" =~ /(?<=[\xB5])x/)
    assert_equal 2, ("\xC4\x80x" =~ /(?<=[\x80])x/)
    assert_nil ("\xC4\x80x" =~ /(?<=[\xC4])x/)
  end
end

assert("Regexp - lookbehind measures an ASCII-only class") do
  need_backtracking_stack
  assert_equal "x", "ax".match(/(?<=[a-z])x/)[0]
  assert_nil "1x".match(/(?<=[a-z])x/)
  assert_equal "x", "1x".match(/(?<=\d)x/)[0]
  assert_equal "x", " x".match(/(?<=\s)x/)[0]
  # a multibyte literal compiles to a run of one-byte instructions, so it
  # keeps its exact width and must keep measuring
  assert_equal "x", "Āx".match(/(?<=Ā)x/)[0]
  assert_nil "bx".match(/(?<=Ā)x/)
end

assert("Regexp - consecutive optional quantifiers (#6853)") do
  # insert_inst was over-incrementing jump offsets that pointed *at* the
  # insertion site, sending earlier "skip this atom" SPLITs into the next
  # atom's body. Two adjacent zero-matchable atoms then both failed even
  # when both should match zero characters.
  assert_equal ["a", nil],   /\Aa(b)?c?\z/.match("a").to_a
  assert_equal ["ab", "b"],  /\Aa(b)?c?\z/.match("ab").to_a
  assert_equal ["ac", nil],  /\Aa(b)?c?\z/.match("ac").to_a
  assert_equal ["abc", "b"], /\Aa(b)?c?\z/.match("abc").to_a

  assert_equal [""], /a?b?/.match("").to_a
  assert_equal [""], /a*b*/.match("").to_a
  assert_equal [""], /a?b?c?d?/.match("").to_a
end

assert("Regexp - a relocated lookaround keeps the end of its sub-pattern") do
  need_backtracking_stack
  # A lookaround holds the end of its sub-pattern as an absolute code index,
  # so every relocation has to carry it the way it carries a jump target.
  # Neither relocator did: the stale index landed on the sub-pattern's own
  # RE_MATCH, which ends the outer match early, so the answers below flipped
  # in both directions and the MatchData of an apparent success held nil.
  # Three shapes reach a relocator, one each.

  # insert_inst, via the SPLIT a quantifier puts in front of the group
  assert_nil /(?:(?=a)b)*x/.match("a")
  assert_equal "x", /(?:(?!b)b)*x/.match("ax")[0]

  # emit_atom_copy, via the copies {n,m} makes of the group
  assert_equal "aa", /(?:(?=a)a){2}/.match("aa")[0]
  assert_nil /(?:(?=a)a){2}/.match("ab")

  # insert_inst again, via the SPLIT compile_alt puts in front of branch 0
  # once every branch is compiled
  md = /(?=a)a|z/.match("ax")
  assert_equal 0, md.begin(0)
  assert_equal "a", md[0]

  # the same group without a relocation, which always answered correctly
  assert_equal "ab", /(?:(?=a)ab)+/.match("ab")[0]
end

assert("Regexp - empty-matchable patterns find earliest match position") do
  # When a regex can match zero characters via epsilon transitions, the
  # first-byte skip-ahead optimization is unsafe: skipping past bytes
  # that aren't in the first-byte set would also skip past valid
  # empty-match positions.
  md = /a?/.match("b")
  assert_equal "", md[0]
  assert_equal 0, md.begin(0)

  md = /a?b?/.match("c")
  assert_equal "", md[0]
  assert_equal 0, md.begin(0)
end

assert("Regexp - octal and hex escapes") do
  assert_equal 0, (/\033/ =~ "\e")
  assert_equal 0, (/\x1b/ =~ "\e")
  assert_equal 0, (/[\x41]/ =~ "A")
  assert_equal 0, (/[\101]/ =~ "A")
  assert_equal 0, (/\x7/ =~ "\a")

  # three octal digits can spell more than a byte, which is refused rather
  # than folded to one, inside a class and out
  assert_kind_of Regexp, Regexp.new('\377')
  assert_raise_with_message(RegexpError, "invalid escape code: /\\400/") do
    Regexp.new('\400')
  end
  assert_raise_with_message(RegexpError, "invalid escape code: /[\\400]/") do
    Regexp.new('[\400]')
  end
end

assert("Regexp - a hex escape needs at least one digit") do
  # `\x` followed by no hex digit used to read as `\x00`, so `\x{41}` was a
  # NUL and a quantifier, and matched 41 NUL bytes. A regexp literal never
  # gets this far (the parser refuses it), so the pattern has to be a string.
  assert_raise_with_message(RegexpError, "invalid hex escape: /\\x{41}/") do
    Regexp.new("\\x{41}")
  end
  assert_raise_with_message(RegexpError, "invalid hex escape: /\\x/") do
    Regexp.new("\\x")
  end
  assert_raise(RegexpError) { Regexp.new("\\xZ") }
  assert_raise(RegexpError) { Regexp.new("a\\x") }
  assert_raise(RegexpError) { Regexp.new("\\x{}") }

  # inside a character class the escape reads the same way
  assert_raise_with_message(RegexpError, "invalid hex escape: /[\\x]/") do
    Regexp.new("[\\x]")
  end
  assert_raise(RegexpError) { Regexp.new("[\\xZ]") }
  assert_raise(RegexpError) { Regexp.new("[\\x{41}]") }
  assert_raise(RegexpError) { Regexp.new("[a-\\x]") }
  assert_raise(RegexpError) { Regexp.new("[\\x-z]") }

  # one digit is enough, and a second non-digit ends the escape
  assert_equal 0, (Regexp.new("\\x4") =~ "\x04")
  assert_equal 0, (Regexp.new("\\x4Z") =~ "\x04Z")
  assert_equal 0, (Regexp.new("[\\x4]") =~ "\x04")
end

assert("Regexp - a digit escape no group answers to is an octal escape") do
  # Outside a class the digits after the backslash are read as one decimal
  # number: a backreference when it is at most 9 or at most the number of
  # groups opened before it, as CRuby reads it, and an octal escape of up
  # to three digits otherwise. \0 is always octal. An octal escape is a
  # byte like any other, so none of these leaves the Pike VM; what the
  # count does make a backreference is pinned below.
  assert_equal 0, (/\101/ =~ "A")
  assert_equal 0, (/\12/ =~ "\n")
  assert_equal 0, (/\100/ =~ "@")
  assert_equal 0, (/\1234/ =~ "S4")
  assert_equal 0, (/\18/ =~ "\x018")
  assert_equal 0, (/\101/i =~ "a")
  assert_equal "AA", /\101{2}/.match("AA")[0]
  assert_equal 0, (/\303\244/ =~ "ä")

  # 8 and 9 are no octal digits, so what is not a backreference is the
  # digit itself
  assert_equal 0, (/\81/ =~ "81")
  assert_equal 0, (/\99/ =~ "99")

  # A named pattern counts its plain groups too, since CRuby demotes them
  # only once the parse is done: what the count makes a backreference is
  # then refused by number, and what it makes an octal escape is read.
  msg = "numbered backref/call is not allowed. (use name)"
  assert_equal 0, (/(?<n>a)\101/ =~ "aA")
  assert_equal 0, (/(?<n>a)\10/ =~ "a\b")
  assert_equal 0, (/(?<n>a)(?<m>b)(c)(d)(e)(f)(g)(h)(i)\10/ =~ "abcdefghi\b")
  assert_raise_with_message(RegexpError, "#{msg}: /(?<n>a)\\9/") do
    Regexp.new("(?<n>a)\\9")
  end
  assert_raise_with_message(RegexpError, "#{msg}: /(?<n>a)(?<m>b)(c)(d)(e)(f)(g)(h)(i)(j)\\10/") do
    Regexp.new("(?<n>a)(?<m>b)(c)(d)(e)(f)(g)(h)(i)(j)\\10")
  end

  # Under /x whitespace ends the number, so `\10 1` is octal 010 and a `1`
  assert_equal 0, (Regexp.new("\\10 1", Regexp::EXTENDED) =~ "\b1")
end

assert("Regexp - a digit escape is a backreference or an octal escape by the group count") do
  need_backtracking_stack
  # The count is taken where the escape stands, so the same \10 refers back
  # after ten groups and is octal 010 before them. Each pair here is the
  # same spelling read both ways, which is why the octal half stands under
  # this guard with the backreference half rather than beside the escapes
  # above.
  ten = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)"
  assert_equal 0, (Regexp.new("#{ten}\\10") =~ "abcdefghijj")
  assert_nil Regexp.new("#{ten}\\10") =~ "abcdefghij\b"
  assert_equal 0, (Regexp.new("#{ten}\\10{2}") =~ "abcdefghijjj")
  assert_equal 0, (Regexp.new("#{ten}\\11") =~ "abcdefghij\t")
  assert_equal 0, (Regexp.new("#{ten}(k)\\11") =~ "abcdefghijkk")
  assert_equal 0, (Regexp.new("\\10#{ten}") =~ "\babcdefghij")
  assert_equal 0, (/(a)(b)(c)(d)(e)(f)(g)(h)(i)\10/ =~ "abcdefghi\b")

  # Under /x whitespace ends the number and a comment does not, as in
  # CRuby, whose tokenizer stops at whitespace but never sees a comment: the
  # first is `\1` and a `0`, and the other two are octal 010.
  assert_equal 0, (Regexp.new("(a)\\1 0", Regexp::EXTENDED) =~ "aa0")
  assert_equal 0, (Regexp.new("(a)\\1#c\n0", Regexp::EXTENDED) =~ "a\b")
  assert_equal 0, (/(a)\1(?#c)0/ =~ "a\b")
end

assert("Regexp - \\h and \\H hex-digit shorthands") do
  assert_equal 0, (/\h/ =~ "f")
  assert_nil (/\h/ =~ "g")
  assert_equal 0, (/\H/ =~ "g")
  assert_nil (/\H/ =~ "a")
  assert_equal ["3f"], "3fX".scan(/[\h]+/)
  assert_equal ["XY"], "3fXY".scan(/[\H]+/)
  assert_equal ["deadBEEF"], "deadBEEFzz".scan(/\h+/)
end

assert("Regexp - pattern too large for its jump targets is refused") do
  # Jump targets live in a 16-bit field, so a program that outgrows the field
  # used to wrap them and jump to an unrelated instruction: the pattern then
  # quietly stopped matching text it describes instead of reporting anything.
  # Each (?:abc) unit costs three instructions and the bound is on the whole
  # program, so the two counts below sit either side of it.
  assert_kind_of Regexp, Regexp.new("(?:abc){21844}")
  assert_raise_with_message(RegexpError, "regexp too large: /(?:abc){21845}/") do
    Regexp.new("(?:abc){21845}")
  end

  # the shapes that used to answer wrongly rather than raise: a quantifier
  # whose skip target is patched past the bound, and an alternation whose
  # branch and exit targets both wrap
  assert_raise(RegexpError) { Regexp.new("(?:abc){21844}x*y") }
  assert_raise(RegexpError) { Regexp.new("(?:abc){30000}(?:y|z)") }

  # a quantifier the parser still accepts reaches the bound on its own once
  # the repeated atom costs more than one instruction
  assert_raise(RegexpError) { Regexp.new("(?:ab){32768}") }
end

assert("Regexp - a character property escape is refused, not read as letters") do
  # The engine reads no character property. Left as an unknown escape,
  # `\p{Alpha}` was the letters `p{Alpha}` and the pattern answered a request
  # for a letter with the text of the request; inside a class it was worse,
  # since every letter of the name became a member of the class.
  assert_raise_with_message(RegexpError,
                            "character property is not supported: /\\p{Alpha}/") do
    Regexp.new("\\p{Alpha}")
  end
  ["\\P{Alpha}", "[\\p{Han}]", "[\\P{L}]", "a\\p{Lu}b", "(?x)\\p{Space}",
   "\\p{}", "\\p{"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # It is the braces that name a property. CRuby reads a bare `\p`, and `\pL`
  # as well, as the letter, with only a warning, and so does this.
  assert_equal "p", "p"[/\p/]
  assert_equal "P", "P"[/\P/]
  assert_equal "pL", "pL"[/\pL/]
  assert_equal "p", "p"[/[\p]/]

  # `[[:alpha:]]` is how to ask for a letter, and still is
  assert_equal "a", "1a"[/[[:alpha:]]/]
end

assert("Regexp - a character class intersection is refused, not read as members") do
  # `&&` narrows a class to what both sides hold, which this engine does not
  # do. Read as members it did the opposite: [a&&b] held a, & and b where it
  # names nothing at all, so a class written to narrow one widened it instead.
  assert_raise_with_message(RegexpError,
                            "character class intersection is not supported: /[a&&b]/") do
    Regexp.new("[a&&b]")
  end
  ["[a&&]", "[&&a]", "[&&]", "[a&&b&&c]", "[[:alpha:]&&[:digit:]]",
   "[\\w&&\\d]", "[^a&&b]", "[a-c&&b]"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # A lone `&` is a member, here as in CRuby
  assert_equal "&", "x&y"[/[&]/]
  assert_equal "&", "x&y"[/[a&b]/]
  assert_equal "a", "a"[/[a&b]/]

  # and an escaped one is that member followed by whatever comes next, so the
  # pair it makes with the next `&` is not an intersection
  assert_equal "&", "&"[/[\&]/]
  assert_equal "&", "x&y"[/[\&&]/]
end

assert("Regexp - the escapes this engine does not carry are refused") do
  # Each means something in CRuby that this engine does not do, and as an
  # unknown escape each was simply its own letter: /\R/ matched an R rather
  # than a newline, and /(a)\g<1>/ matched "ag<1>" rather than "aa".
  assert_raise_with_message(RegexpError, "\\G is not supported: /\\G/") { Regexp.new("\\G") }
  assert_raise_with_message(RegexpError, "\\K is not supported: /a\\Kb/") { Regexp.new("a\\Kb") }
  ["\\R", "\\X", "x\\G", "\\K"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end
  assert_raise_with_message(RegexpError,
                            "subexpression call is not supported: /(a)\\g<1>/") do
    Regexp.new("(a)\\g<1>")
  end
  ["(?<n>a)\\g<n>", "(a)\\g'1'", "\\g<0>"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # Inside a character class CRuby reads them as the letter, and so does this
  assert_equal "G", "G"[/[\G]/]
  assert_equal "R", "R"[/[\R]/]
  assert_equal "X", "X"[/[\X]/]
  assert_equal "g", "g"[/[\g]/]

  # and a bare `\g` is the letter outside one too
  assert_equal "g", "g"[/\g/]
end

assert("Regexp - \\k<name> is the group reference this engine does carry") do
  need_backtracking_stack
  # `\g<name>` is refused above; the letter one apart from it is not.
  assert_equal "aa", "aa"[/(?<n>a)\k<n>/]
end

assert("Regexp - a '[' inside a class opens something, and is refused when it cannot") do
  # A '[' inside a class never stands for itself in CRuby: it opens a POSIX
  # bracket, a collating element, an equivalence class, or a class nested in
  # this one. Only the bracket is read here. Taken as a member the rest
  # compiled to a different pattern than the one written: [[a][b]] is the
  # union of two classes in CRuby and was `[` or `a`, then b, then `]` here.
  assert_raise_with_message(RegexpError,
                            "nested character class is not supported: /[[a][b]]/") do
    Regexp.new("[[a][b]]")
  end
  assert_raise_with_message(RegexpError,
                            "POSIX collating element is not supported: /[[.a.]]/") do
    Regexp.new("[[.a.]]")
  end
  assert_raise_with_message(RegexpError,
                            "POSIX equivalence class is not supported: /[[=a=]]/") do
    Regexp.new("[[=a=]]")
  end
  assert_raise_with_message(RegexpError,
                            "premature end of char-class: /[[:alpha]/") do
    Regexp.new("[[:alpha]")
  end
  ["[[]", "[a[]", "[[ab]c]", "[[^a]b]"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # The bracket that is read, in every position it is written in
  assert_equal "a", "1a"[/[[:alpha:]]/]
  assert_equal "x", " x"[/[^[:space:]]/]
  assert_equal "a1", "a1-"[/[a[:digit:]]+/]

  # and the escaped bracket, which is how to hold one, in CRuby too
  assert_equal "[", "x[y"[/[\[]/]

  # a '[' with nothing after it leaves the class unterminated
  assert_raise(RegexpError) { Regexp.new("[a[") }
end

assert("Regexp - a control escape names the same character however it is written") do
  # A regexp literal reaches the engine with `\cA` already turned into its
  # byte by the lexer, so only Regexp.new() with a written-out backslash asks
  # the engine to read one. It used to read the letters instead, and the same
  # pattern then meant two different things depending on how it was spelled.
  assert_equal "\cA", "\cA"[Regexp.new("\\cA")]
  assert_equal "\cA", "\cA"[Regexp.new("\\C-A")]
  assert_equal "\cA", "\cA"[/\cA/]
  assert_equal "\cA", "\cA"[/\C-A/]

  # the mask is the lexer's, so every X agrees with the string of the same name
  assert_equal "\c@", "\c@"[Regexp.new("\\c@")]
  assert_equal "\cz", "\cz"[Regexp.new("\\cz")]
  assert_equal "\c-", "\c-"[Regexp.new("\\c-")]
  # `?` is the one X that is not X & 0x1f, in the lexer and so here
  assert_equal "\c?", "\c?"[Regexp.new("\\c?")]
  assert_equal 127, "\c?".bytes[0]
  # a backslash in the X position opens an escape of its own
  assert_equal "\c\n", "\c\n"[Regexp.new("\\c\\n")]

  # inside a class, as a member and as the end of a range
  assert_equal "\cA", "\cA"[Regexp.new("[\\cA]")]
  assert_equal "\cB", "\cB"[Regexp.new("[\\cA-\\cC]")]
  assert_nil "\cD"[Regexp.new("[\\cA-\\cC]")]

  # what ends early is refused rather than read as the letter
  ["\\c", "\\C", "\\CA", "[\\C]"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # `\M-X` sets the high bit, making a byte that starts no character, and
  # there is no encoding here to read one against
  assert_raise_with_message(RegexpError, "meta escape is not supported: /\\M-a/") do
    Regexp.new("\\M-a")
  end
  ["\\M", "[\\M]", "[\\M-a]", "\\M-\\C-a"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end
end

assert("Regexp - a set cannot be an end of a range in a class") do
  # A shorthand and a POSIX bracket each name a set rather than a character,
  # so neither can open or close a range. Read as characters the class held
  # something else entirely: [a-\d] was [a-d], four letters, and [\d-z] was
  # the digits plus `-` plus z.
  assert_raise_with_message(RegexpError,
                            "char-class value at end of range: /[a-\\d]/") do
    Regexp.new("[a-\\d]")
  end
  assert_raise_with_message(RegexpError,
                            "unmatched range specifier in char-class: /[\\d-z]/") do
    Regexp.new("[\\d-z]")
  end
  ["[a-\\w]", "[a-\\D]", "[\\w-z]", "[\\s-z]", "[\\D-z]", "[\\d-\\w]",
   "[a\\d-z]", "[[:digit:]-z]", "[[:alpha:]-[:digit:]]",
   "[a-[:digit:]]"].each do |src|
    assert_raise(RegexpError, src) { Regexp.new(src) }
  end

  # A '-' at either edge is a member, so these still read as they did.
  # Counted with a plain loop: this gem's tests run without mruby-enum-ext.
  members = lambda do |src|
    re = Regexp.new(src)
    n = 0
    i = 0x20
    while i <= 0x7e
      n += 1 if re.match(i.chr)
      i += 1
    end
    n
  end
  assert_equal 11, members.call("[-\\d]")
  assert_equal 11, members.call("[\\d-]")
  assert_equal 10, members.call("[\\d]")
  assert_equal 26, members.call("[a-z]")
end
