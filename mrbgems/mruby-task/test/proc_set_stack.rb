# Regression: a task's stack is sized for the proc it is created with. When a
# larger proc is installed on the same context afterwards (mrb_task_proc_set,
# as picoruby-sandbox's Sandbox#execute does), the stack must grow to cover the
# new proc's nregs. Otherwise mrb_vm_exec()/OP_ENTER clears registers past the
# stack allocation (heap-buffer-overflow; the crash surfaces as a bogus realloc
# in stack_extend_alloc on 32-bit targets).
if Object.const_defined?(:TaskTest) && TaskTest.respond_to?(:proc_set_stack)
  assert('mruby-task: proc_set grows the task stack for a larger proc') do
    small = Proc.new { }
    # ~80 locals -> nregs well past TASK_STACK_INIT_SIZE (64).
    big = Proc.new {
      a0=0;a1=0;a2=0;a3=0;a4=0;a5=0;a6=0;a7=0;a8=0;a9=0;a10=0;a11=0;a12=0;a13=0;a14=0;a15=0;a16=0;a17=0;a18=0;a19=0;
      a20=0;a21=0;a22=0;a23=0;a24=0;a25=0;a26=0;a27=0;a28=0;a29=0;a30=0;a31=0;a32=0;a33=0;a34=0;a35=0;a36=0;a37=0;a38=0;a39=0;
      a40=0;a41=0;a42=0;a43=0;a44=0;a45=0;a46=0;a47=0;a48=0;a49=0;a50=0;a51=0;a52=0;a53=0;a54=0;a55=0;a56=0;a57=0;a58=0;a59=0;
      a60=0;a61=0;a62=0;a63=0;a64=0;a65=0;a66=0;a67=0;a68=0;a69=0;a70=0;a71=0;a72=0;a73=0;a74=0;a75=0;a76=0;a77=0;a78=0;a79=0
    }
    slots, nregs = TaskTest.proc_set_stack(small, big)
    assert_true nregs > 64, "test proc should exceed TASK_STACK_INIT_SIZE (got nregs=#{nregs})"
    assert_true slots >= nregs, "task stack (#{slots}) must cover replacement proc nregs (#{nregs})"
  end
end

# Regression: an undersized context puts ci->stack past stend, which is the
# state above before #7279 and the one an embedder reaches by entering the VM
# the same way. stack_extend_alloc() reads its size floor as the frame's
# offset from the bottom of the stack; read as the room above the frame the
# unsigned subtraction wrapped, the growth math asked for a stack no allocator
# could give, and the extend raised NoMemoryError rather than growing.
if Object.const_defined?(:TaskTest) && TaskTest.respond_to?(:extend_past_stend)
  assert('mruby-task: the VM stack grows for a frame that sits past its top') do
    over = 8
    size = TaskTest.extend_past_stend(Proc.new { }, over, 4)
    # `assert_not_nil` records a failure without leaving the block, so the
    # size is only compared when there is one; otherwise the miss reads as a
    # NoMethodError on nil rather than as the assertion that failed.
    assert_not_nil size, "the extend raised instead of growing the stack"
    if size
      assert_true size > over,
                  "new stack (#{size}) must reach the frame at #{over} past the old top"
    end
  end
end
