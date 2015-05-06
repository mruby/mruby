	.text
	.globl	op_jmpnot
	.align	16, 0x90
	.type	op_jmpnot,@function
op_jmpnot:                               # @op_jmpnot
  mov    0x18(%rdi),%rax
  cmpl   $0x0,0xab1008(%rax)
