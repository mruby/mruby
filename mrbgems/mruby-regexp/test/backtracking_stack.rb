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
# files matches again (at 44 one test is left, at 36 two, at 1 fifty-one),
# so a test that reaches the engine asks for that much and skips below it.
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
