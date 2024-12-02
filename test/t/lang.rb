# The aim of these tests is to detect pitfall for optimized VM.

# Test for or/and
#
# You may think instruction fusion(OP_EQ and OP_JMPIF) for avoiding
# generate intermediate boolean value.
# But and/or is pitfall for this fusioning.
#
# For example, the following mruby code:
#
#   if i > 0 and i < 10
#
# compiles to the following byte code:
#
#    1 000 LOADI_0      R1      (0)             ; R1:i
#    2 002 MOVE         R2      R1              ; R1:i
#    2 005 LOADI_0      R3      (0)
#    2 007 GT     R2      R3
#    2 009 JMPNOT       R2      021
#    2 013 MOVE         R2      R1              ; R1:i
#    2 016 LOADI8       R3      10
#    2 019 LT     R2      R3
#    2 021 JMPNOT       R2      (The address of end of then part)
#
# When the instruction fusion the OP_GT and OP_JMPNOT you fell into the pitfalls.
# The deleted intermediate boolean value is used in OP_JMPNOT (address 008).

assert('and', '11.2.3') do
  a = 1
  if a > 0 and a < 10
    b = 1
  else
    b = 0
  end
  assert_equal 1, b

  if a < 0 and a < 10
    b = 1
  else
    b = 0
  end
  assert_equal 0, b

  if a < 0 and a > 10
    b = 1
  else
    b = 0
  end
  assert_equal 0, b
end

assert('or','11.2.4') do
  a = 1
  if a > 0 or a < 10
    b = 1
  else
    b = 0
  end
  assert_equal 1, b

  if a < 0 or a < 10
    b = 1
  else
    b = 0
  end
  assert_equal 1, b

  if a < 0 or a > 10
    b = 1
  else
    b = 0
  end
  assert_equal 0, b
end
