assert('mrb_format(): string substitution') do
  "foo" == CAPITest_mrb_format::test1
end

assert('mrb_format(): fixnum substitution') do
  "10" == CAPITest_mrb_format::test2
end

assert('mrb_format(): string substitution after a space') do
  " foo" == CAPITest_mrb_format::test3
end

assert('mrb_format(): multiple substitution') do
  "foo bar"  == CAPITest_mrb_format::test4
end

assert('mrb_format(): backslash escape') do
  "%S"  == CAPITest_mrb_format::test5
end
