##
# Issue 214
# Broken mrb_str_concat for shared strings

assert('Issue 214') do
  a = "A" * 32
  "#{a}:" == "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA:"
end
