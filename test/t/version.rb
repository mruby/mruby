##
# MRUBY_REVISION

assert('MRUBY_REVISION') do
  assert_kind_of(String, MRUBY_REVISION)
  # What the build was made from is not a thing a program gets to edit.
  assert_true(MRUBY_REVISION.frozen?)
  # A build with no revision to read says "HEAD"; one that read a revision
  # says the whole commit hash, and nothing else belongs there.
  skip 'built without a revision' if MRUBY_REVISION == 'HEAD'
  assert_equal(40, MRUBY_REVISION.size)
  i = 0
  while i < MRUBY_REVISION.size
    assert_include('0123456789abcdef', MRUBY_REVISION[i])
    i += 1
  end
end
