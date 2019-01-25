assert("sleep works") do
  assert_nothing_raised { sleep(1) }
end

assert("sleep would not accept negative value") do
  assert_raise(ArgumentError) { sleep(-1) }
end

assert("usleep works") do
  assert_nothing_raised { usleep(100) }
end

assert("usleep would not accept negative value") do
  assert_raise(ArgumentError) { usleep(-100) }
end
