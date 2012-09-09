##
# Array#pack, String#unpack Test

if Array.new.methods.include?(:pack) and String.new.methods.include?(:unpack)


  # pack & unpack 'm' (base64)
  assert('[""].pack("m")') do
    ary = ""
    str = ""
    [ary].pack("m") == str and
    str.unpack("m") == [ary]
  end

  assert('["\0"].pack("m")') do
    ary = "\0"
    str = "AA==\n"
    [ary].pack("m") == str and
    str.unpack("m") == [ary]
  end

  assert('["\0\0"].pack("m")') do
    ary = "\0\0"
    str = "AAA=\n"
    [ary].pack("m") == str and
    str.unpack("m") == [ary]
  end

  assert('["\0\0\0"].pack("m")') do
    ary = "\0\0\0"
    str = "AAAA\n"
    [ary].pack("m") == str and
    str.unpack("m") == [ary]
  end

  assert('["abc..xyzABC..XYZ"].pack("m")') do
    ["abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"].pack("m") == "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJT\nVFVWV1hZWg==\n"
  end

  assert('"YWJ...".unpack("m") should "abc..xyzABC..XYZ"') do
    str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJT\nVFVWV1hZWg==\n".unpack("m") == [str] and
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg==\n".unpack("m") == [str]
  end

end

