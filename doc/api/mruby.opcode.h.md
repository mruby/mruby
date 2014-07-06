# mruby/opode.h

## MAXARG_Bx
Maximum value of `Bx` parameter.

## MAXARG_sBx
Maximum value of `sBx`(signed `Bx`) parameter.

## OP_L_* flags
2-bit flags used in 3rd parameter of `OP_LAMBDA`.

Name | Value
=====|======
`OP_L_STRICT` | 1
`OP_L_CAPTURE` | 2
`OP_L_METHOD` | `OP_L_STRICT`
`OP_L_LAMBDA` | `OP_L_STRICT | OP_L_CAPTURE`
`OP_L_BLOCK` | `OP_L_CAPTURE`

## OP_RETURN type
Used in 2nd parameter of `OP_RETURN`.

Name | Description
=====|============
`OP_R_NORMAL` | return implicitly
`OP_R_BREAK` | break loop
`OP_R_RETURN` | return explicitly

## Instruction packing.
Every instruction is packed to 32bit integer(`mrb_code`).
Opcode always take last 7-bit of instruction.

Parameter names | Bits per each parameter
============:|============:
`A:B:C:OP`   | `9: 9: 7: 7`
`A:Bx:OP`    |    `9:16: 7`
`Ax:OP`      |      `25: 7`
`A:Bz:Cz:OP` | `9:14: 2: 7`

## Instruction parameter getter.
Macro to get parameter of instruction.
Usually returns unsigned integer except `GETARG_sBx`.

Macro | Description
======|============
`GET_OPCODE(i)` | Gets opcode of instruction.
`GETARG_A(i)` | Gets `A` parameter.
`GETARG_B(i)` | Gets `B` parameter.
`GETARG_C(i)` | Gets `C` parameter.
`GETARG_Bx(i)` | Gets `Bx` parameter.
`GETARG_sBx(i)` | Gets signed `Bx` parameter.
`GETARG_Ax(i)` | Gets `Ax` parameter.
`GETARG_b(i)` | Gets `Bz` parameter.
`GETARG_b(i)` | Gets `Cz` parameter.

## Instruction mask maker.
Macro to make mask of instruction from specific parameter.
To make a complete instruction combine all masks withn `|` operator.

Macro | Description
======|============
`MKOPCODE(op)` | Make opcode mask.
`MKARG_A(p)` | Make `A` parameter mask.
`MKARG_B(p)` | Make `B` parameter mask.
`MKARG_C(p)` | Make `C` parameter mask.
`MKARG_Bx(p)` | Make `Bx` parameter mask.
`MKARG_Ax(p)` | Make `Ax` parameter mask.
`MKARG_bc(b, c)` | Make `Bz` and `Cz` parameter mask.

## Instruction maker.
Macro to make instruction from parameters.

Macro | Description
======|============
`MKOP_A(op, a)` | Make instruction of `OP`, `A`.
`MKOP_AB(op, a, b)` | Make instruction of `OP`, `A`, `B`.
`MKOP_ABC(op, a, b, c)` | Make instruction of `OP`, `A`, `B`, `C`
`MKOP_ABx(op, a, bx)` | Make instruction of `OP`, `A`, `Bx`.
`MKOP_AsBx(op, a, sbx)` | Make instruction of `OP`, `A`, signed `Bx`.
`MKOP_Bx(op, bx)` | Make instruction of `OP`, `Bx`.
`MKOP_sBx(op, sbx)` | Make instruction of `OP`, signed `Bx`.
`MKOP_Ax(op, ax)` | Make instruction of `OP`, `Ax`.
`MKOP_Abc(op, a, b, c)` | Make instruction of `OP`, `A`, `Bz`, `Cz`.

## OPCODEs
* `R(idx)` references register of `idx`.
* `Syms[idx]` gets symbol of `idx` in IREP symbol table.

Name | Format | Psuedo code
=====|========|============
`OP_NOP` | | Do nothing.
`OP_MOVE` | `A B` | R(A) := R(B)
`OP_LOADL` | `A Bx` | R(A) := Pool(Bx)
`OP_LOADI` | `A sBx` | R(A) := sBx
`OP_LOADSYM` | `A Bx` | R(A) := Syms(Bx)
`OP_LOADNIL` | `A` | R(A) := nil
`OP_LOADSELF` | `A` | R(A) := self
`OP_LOADT` | `A` | R(A) := true
`OP_LOADF` | `A` | R(A) := false
`OP_GETGLOBAL` | `A Bx` | R(A) := getglobal(Syms(Bx))
`OP_SETGLOBAL` | `A Bx` | setglobal(Syms(Bx), R(A))
`OP_GETSPECIAL` | `A Bx` | R(A) := Special[Bx]
`OP_SETSPECIAL` | `A Bx` | Special[Bx] := R(A)
`OP_GETIV` | `A Bx` | R(A) := ivget(Syms(Bx))
`OP_SETIV` | `A Bx` | ivset(Syms(Bx),R(A))
`OP_GETCV` | `A Bx` | R(A) := cvget(Syms(Bx))
`OP_SETCV` | `A Bx` | cvset(Syms(Bx),R(A))
`OP_GETCONST` | `A Bx` | R(A) := constget(Syms(Bx))
`OP_SETCONST` | `A Bx` | constset(Syms(Bx),R(A))
`OP_GETMCNST` | `A Bx` | R(A) := R(A)::Syms(Bx)
`OP_SETMCNST` | `A Bx` | R(A+1)::Syms(Bx) := R(A)
`OP_GETUPVAR` | `A B C` | R(A) := uvget(B,C)
`OP_SETUPVAR` | `A B C` | uvset(B,C,R(A))
`OP_JMP` | `sBx` | pc+=sBx
`OP_JMPIF` | `A sBx` | if R(A) pc+=sBx
`OP_JMPNOT` | `A sBx` | if !R(A) pc+=sBx
`OP_ONERR` | `sBx` | rescue_push(pc+sBx)
`OP_RESCUE` | `A` | clear(exc); R(A) := exception (ignore when A=0)
`OP_POPERR` | `A` | A.times{rescue_pop()}
`OP_RAISE` | `A` | raise(R(A))
`OP_EPUSH` | `Bx` | ensure_push(SEQ[Bx])
`OP_EPOP` | `A` | A.times{ensure_pop().call}
`OP_SEND` | `A B C` | R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C))
`OP_SENDB` | `A B C` | R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C),&R(A+C+1))
`OP_FSEND` | `A B C` | R(A) := fcall(R(A),Syms(B),R(A+1),...,R(A+C-1))
`OP_CALL` | `A` | R(A) := self.call(frame.argc, frame.argv)
`OP_SUPER` | `A C` | R(A) := super(R(A+1),... ,R(A+C+1))
`OP_ARGARY` | `A Bx` | R(A) := argument array (16=6:1:5:4)
`OP_ENTER` | `Ax` | arg setup according to flags (23=5:5:1:5:5:1:1)
`OP_KARG` | `A B C` | R(A) := kdict[Syms(B)]; if C kdict.rm(Syms(B))
`OP_KDICT` | `A C` | R(A) := kdict
`OP_RETURN` | `A B` | return R(A) (B=normal,in-block return/break)
`OP_TAILCALL` | `A B C` | return call(R(A),Syms(B),*R(C))
`OP_BLKPUSH` | `A Bx` | R(A) := block (16=6:1:5:4)
`OP_ADD` | `A B C` | R(A) := R(A)+R(A+1) (Syms[B]=:+,C=1)
`OP_ADDI` | `A B C` | R(A) := R(A)+C (Syms[B]=:+)
`OP_SUB` | `A B C` | R(A) := R(A)-R(A+1) (Syms[B]=:-,C=1)
`OP_SUBI` | `A B C` | R(A) := R(A)-C (Syms[B]=:-)
`OP_MUL` | `A B C` | R(A) := R(A)*R(A+1) (Syms[B]=:*,C=1)
`OP_DIV` | `A B C` | R(A) := R(A)/R(A+1) (Syms[B]=:/,C=1)
`OP_EQ` | `A B C` | R(A) := R(A)==R(A+1) (Syms[B]=:==,C=1)
`OP_LT` | `A B C` | R(A) := R(A)<R(A+1) (Syms[B]=:<,C=1)
`OP_LE` | `A B C` | R(A) := R(A)<=R(A+1) (Syms[B]=:<=,C=1)
`OP_GT` | `A B C` | R(A) := R(A)>R(A+1) (Syms[B]=:>,C=1)
`OP_GE` | `A B C` | R(A) := R(A)>=R(A+1) (Syms[B]=:>=,C=1)
`OP_ARRAY` | `A B C` | R(A) := ary_new(R(B),R(B+1)..R(B+C))
`OP_ARYCAT` | `A B` | ary_cat(R(A),R(B))
`OP_ARYPUSH` | `A B` | ary_push(R(A),R(B))
`OP_AREF` | `A B C` | R(A) := R(B)[C]
`OP_ASET` | `A B C` | R(B)[C] := R(A)
`OP_APOST` | `A B C` | *R(A),R(A+1)..R(A+C) := R(A)
`OP_STRING` | `A Bx` | R(A) := str_dup(Lit(Bx))
`OP_STRCAT` | `A B` | str_cat(R(A),R(B))
`OP_HASH` | `A B C` | R(A) := hash_new(R(B),R(B+1)..R(B+C))
`OP_LAMBDA` | `A Bz Cz` | R(A) := lambda(SEQ[Bz],Cz)
`OP_RANGE` | `A B C` | R(A) := range_new(R(B),R(B+1),C)
`OP_OCLASS` | `A` | R(A) := ::Object
`OP_CLASS` | `A B` | R(A) := newclass(R(A),Syms(B),R(A+1))
`OP_MODULE` | `A B` | R(A) := newmodule(R(A),Syms(B))
`OP_EXEC` | `A Bx` | R(A) := blockexec(R(A),SEQ[Bx])
`OP_METHOD` | `A B` | R(A).newmethod(Syms(B),R(A+1))
`OP_SCLASS` | `A B` | R(A) := R(B).singleton_class
`OP_TCLASS` | `A` | R(A) := target_class
`OP_DEBUG` | `A B C` | print R(A),R(B),R(C)
`OP_STOP` | | stop VM
`OP_ERR` | `Bx` | raise RuntimeError with message Lit(Bx)
`OP_RSVD1` | | reserved instruction #1
`OP_RSVD2` | | reserved instruction #2
`OP_RSVD3` | | reserved instruction #3
`OP_RSVD4` | | reserved instruction #4
`OP_RSVD5` | | reserved instruction #5
