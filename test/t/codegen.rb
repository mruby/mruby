##
# Codegen tests

assert('nested empty heredoc') do
  _, a = nil, <<B
#{<<A}
A
B
  assert_equal "\n", a
end
