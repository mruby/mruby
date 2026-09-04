##
# MRUBY_PLATFORM Test

assert('MRUBY_PLATFORM') do
  assert_kind_of String, MRUBY_PLATFORM
  assert_predicate MRUBY_PLATFORM, :frozen?
end

assert('MRUBY_PLATFORM is a cpu-os pair') do
  # The constant is read by matching a pattern against it, so what it has to
  # promise is the shape: a non-empty cpu name, a separator, a non-empty rest.
  sep = MRUBY_PLATFORM.index('-')
  assert_not_nil sep, "MRUBY_PLATFORM #{MRUBY_PLATFORM.inspect} has no separator"
  assert_operator sep, :>, 0
  assert_operator sep, :<, MRUBY_PLATFORM.size - 1
  assert_not_include MRUBY_PLATFORM, ' '
end
