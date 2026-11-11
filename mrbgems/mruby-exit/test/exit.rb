assert('Kernel#exit with a status past int') do
  # 2**32 + 3 was cut to 3 and the process exited with it. Computed, not
  # written out, so the file still compiles where mrb_int is 32 bits.
  shift = 32
  big = begin
    (1 << shift) + 3
  rescue RangeError
    nil
  end
  skip "no integer past 32 bits" unless big
  assert_raise(RangeError) { exit(big) }
  assert_raise(RangeError) { exit(-big) }
end

assert('SystemExit.new') do
  e = SystemExit.new
  assert_equal 0, e.status
  assert_equal "SystemExit", e.message
  assert_equal 1, SystemExit.new(false).status
  assert_equal 0, SystemExit.new(true).status
  e = SystemExit.new(3, "bye")
  assert_equal 3, e.status
  assert_equal "bye", e.message
  e = SystemExit.new("bye")
  assert_equal 0, e.status
  assert_equal "bye", e.message
  assert_raise(ArgumentError) { SystemExit.new("a", "b") }
  assert_raise(ArgumentError) { SystemExit.new(1, "b", 3) }
end

assert('SystemExit#success?') do
  assert_true SystemExit.new.success?
  assert_true SystemExit.new(0).success?
  assert_false SystemExit.new(false).success?
  assert_false SystemExit.new(2).success?
  begin
    exit(2)
  rescue SystemExit => e
    assert_equal 2, e.status
    assert_false e.success?
  end
  begin
    exit
  rescue SystemExit => e
    assert_true e.success?
  end
end

assert('Kernel#at_exit') do
  blk = Proc.new { }
  assert_same blk, at_exit(&blk)
  assert_raise(ArgumentError) { at_exit }
end
