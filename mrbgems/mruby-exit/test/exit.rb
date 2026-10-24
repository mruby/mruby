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
