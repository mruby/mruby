assert_equal %q{2012}, %q{
  Time.gm(2012, 12, 23).year
}
assert_equal %q{12}, %q{
  Time.gm(2012, 12, 23).month
}
assert_equal %q{23}, %q{
  Time.gm(2012, 12, 23).mday
}

# TODO
assert_equal %q{ng}, %q{
  $x = :ng
  begin
    Time.new
  rescue
    $x = :ok
  end
  $x
}

assert_equal %q{Sun Mar 13 07:06:40 UTC 2011}, %q{
  Time.at(1300000000.0).utc.asctime
}
assert_equal %q{0}, %q{
  Time.at(1300000000.0).utc.usec
}
assert_equal %q{1300000000}, %q{
  Time.at(1300000000.0).utc.to_i
}
assert_equal %q{1300000000.0}, %q{
  Time.at(1300000000.0).utc.to_f
}
assert_equal %q{true}, %q{
  Time.at(1300000000.0).utc.utc?
}
assert_equal %q{UTC}, %q{
  Time.at(1300000000.0).utc.zone
}
assert_equal %q{0}, %q{
  Time.at(1300000000.0).utc.wday
}
assert_equal %q{2011}, %q{
  Time.at(1300000000.0).utc.year
}

assert_equal %q{true}, %q{
  t = Time.at(7.0e6)
  t.clone == t
}
