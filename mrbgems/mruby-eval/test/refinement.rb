assert('Refinement: string instance_eval and class_eval see the refinements') do
  skip "requires MRB_USE_REFINEMENTS" unless Object.const_defined?(:Refinement)

  klass = Class.new { def foo; "C"; end }
  m = Module.new do
    refine(klass) { def foo; "R"; end }
  end
  c = Class.new do
    using m
    define_method(:run) { [klass.new.instance_eval("foo"), klass.class_eval("new.foo")] }
  end
  assert_equal ["R", "R"], c.new.run
end
