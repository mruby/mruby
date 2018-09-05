def run_with_catching_error &b
  e = nil
  begin
    b.call
  rescue => _e
    e = _e
  end

  return e
end

assert("sleep works") do
  e = run_with_catching_error { sleep 1 }

  assert_nil e
end

assert("sleep would not accept negative value") do
  e = run_with_catching_error { sleep -1 }

  assert_not_equal e, nil
  assert_equal e.class, ArgumentError
end

assert("usleep works") do
  e = run_with_catching_error { usleep 100 }

  assert_nil e
end

assert("usleep would not accept negative value") do
  e = run_with_catching_error { usleep -100 }

  assert_not_equal e, nil
  assert_equal e.class, ArgumentError
end
