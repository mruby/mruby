##
# Pattern-matching syntax accepted by the Prism parser but rejected by the
# legacy lrama parser.
#
# This is in its own file because the lrama parser raises a syntax error on
# patterns such as `self`, so a build using mruby-compiler-lrama excludes this
# file from the test set (see mrbgems/mruby-test/mrbgem.rake).

assert('pattern matching - unimplemented pattern in alternation does not crash') do
  # `self` is an unsupported pattern; on the left of `|` it used to corrupt the
  # bytecode through the JMPNOT->JMPIF peephole and crash the VM. It must just
  # fail to match and fall through to the right alternative.
  result = case 42
  in self | _
    :matched
  else
    :no
  end
  assert_equal :matched, result
end
