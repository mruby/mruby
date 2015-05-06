	.text
	.globl	op_jmpif
	.align	16, 0x90
	.type	op_jmpif,@function
op_jmpif:                               # @op_jmpif
  mov    0x18(%rdi),%rax
  cmpl   $0x0,0xab1008(%rax)
