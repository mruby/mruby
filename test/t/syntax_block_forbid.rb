##
# `&nil` block-forbidding parameters (Ruby Feature #19979).
#
# This is in its own file because the Prism parser does not accept the `&nil`
# syntax yet, so a build using mruby-compiler-prism excludes this file from
# the test set (see mrbgems/mruby-test/mrbgem.rake). The bison parser compiles
# and runs it.

assert('&nil in formal parameters') do
  def m(&nil); end
  m
  m(&nil)
  assert_raise(ArgumentError) { m {} }

  def m2(a, b, &nil); end
  m2(1, 2)
  assert_raise(ArgumentError) { m2(1, 2) {} }

  def m3(a, b=1, *c, &nil); end
  m3(1)
  assert_raise(ArgumentError) { m3(1) {} }

  def m4(a:, &nil); end
  m4(a: 1)
  assert_raise(ArgumentError) { m4(a: 1) {} }

  f = ->(&nil) { :ok }
  assert_equal :ok, f.call
  assert_raise(ArgumentError) { f.call {} }
end
