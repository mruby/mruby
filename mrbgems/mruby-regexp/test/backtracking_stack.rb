# What a test that reaches the backtracking engine asks of the build.
#
# That engine holds its state on a heap stack, and MRB_REGEXP_STACK_LIMIT is
# how many entries the build lets it hold at once. The macro takes any value
# from 1 up, a build that would rather bound what one search may ask the
# allocator for than match a complicated pattern being free to set it low, and
# what such a build gets is an engine that refuses more: a backreference, an
# atomic group or a lookaround sends a pattern here, and a handful of entries
# go on the stack whatever the subject is, so below a certain limit a pattern
# these tests take for granted answers RegexpError instead of matching.
#
# The engine is doing what the build asked there, and the assertions have
# nothing to say about it. 48 is the limit at which every pattern in these
# files matches again: with the guard off, a build reading its strings as
# characters leaves one test at 47, two at 40, three at 36, thirty-nine at 4
# and eighty-four at 1. A test that reaches the engine asks for that much and
# skips below it. What stands at the top of that is the search that reaches
# the step limit, which holds some three entries per character of its run.
# A test wanting more than this is a test to size down: a lookbehind body of
# forty branches sits under it at one entry per branch.
#
# What asks for it is a test and not a subject: an assertion that reaches no
# further than the parser, or one whose pattern the Pike VM runs, holds none
# of this state and answers the same whatever the limit is. Such assertions
# stand in an assert of their own rather than under this guard, so that a
# build that sets the limit low still runs them; where a block mixed the two,
# the split is what the two blocks' names say.
RE_TESTS_NEED_STACK = 48

def need_backtracking_stack
  if Regexp::STACK_LIMIT < RE_TESTS_NEED_STACK
    skip "MRB_REGEXP_STACK_LIMIT stands below what these patterns need"
  end
end
