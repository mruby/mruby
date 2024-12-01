<!-- summary: About mruby Virtual Machine Instructions -->

# The new bytecode

We will reimplement the VM to use 8bit instruction code. By
bytecode, we mean real byte code. The whole purpose is
reducing the memory consumption of mruby VM.

# Instructions

Instructions are bytes. There can be 256 instructions. Currently, we
have 106 instructions. Instructions can take 0 to 3 operands.

## operands

The size of operands can be either 8bits, 16bits or 24bits.
In the table.1 below, the third field describes the size
of operands.

- B: 8bit
- S: 16bit
- W: 24bit

If the first and second operands are of type `B` (8bits), they may be
extended to 16bits by the operand extension instruction immediately
preceding them.
See also `OP_EXT1`, `OP_EXT2` and `OP_EXT3`.

## table.1 Instruction Table

| No. | Instruction Name | Operand type | Semantics
| --: | ---------------- | ------------ | ---------------
|   0 | `OP_NOP`         | `-`          | `no operation`
|   1 | `OP_MOVE`        | `BB`         | `R(a) = R(b)`
|   2 | `OP_LOADL`       | `BB`         | `R(a) = Pool(b)`
|   3 | `OP_LOADI8`      | `BB`         | `R(a) = mrb_int(b)`
|   4 | `OP_LOADINEG`    | `BB`         | `R(a) = mrb_int(-b)`
|   5 | `OP_LOADI__1`    | `B`          | `R(a) = mrb_int(-1)`
|   6 | `OP_LOADI_0`     | `B`          | `R(a) = mrb_int(0)`
|   7 | `OP_LOADI_1`     | `B`          | `R(a) = mrb_int(1)`
|   8 | `OP_LOADI_2`     | `B`          | `R(a) = mrb_int(2)`
|   9 | `OP_LOADI_3`     | `B`          | `R(a) = mrb_int(3)`
|  10 | `OP_LOADI_4`     | `B`          | `R(a) = mrb_int(4)`
|  11 | `OP_LOADI_5`     | `B`          | `R(a) = mrb_int(5)`
|  12 | `OP_LOADI_6`     | `B`          | `R(a) = mrb_int(6)`
|  13 | `OP_LOADI_7`     | `B`          | `R(a) = mrb_int(7)`
|  14 | `OP_LOADI16`     | `BS`         | `R(a) = mrb_int(b)`
|  15 | `OP_LOADI32`     | `BSS`        | `R(a) = mrb_int((b<<16)+c)`
|  16 | `OP_LOADSYM`     | `BB`         | `R(a) = Syms(b)`
|  17 | `OP_LOADNIL`     | `B`          | `R(a) = nil`
|  18 | `OP_LOADSELF`    | `B`          | `R(a) = self`
|  19 | `OP_LOADT`       | `B`          | `R(a) = true`
|  20 | `OP_LOADF`       | `B`          | `R(a) = false`
|  21 | `OP_GETGV`       | `BB`         | `R(a) = getglobal(Syms(b))`
|  22 | `OP_SETGV`       | `BB`         | `setglobal(Syms(b), R(a))`
|  23 | `OP_GETSV`       | `BB`         | `R(a) = Special[Syms(b)]`
|  24 | `OP_SETSV`       | `BB`         | `Special[Syms(b)] = R(a)`
|  25 | `OP_GETIV`       | `BB`         | `R(a) = ivget(Syms(b))`
|  26 | `OP_SETIV`       | `BB`         | `ivset(Syms(b),R(a))`
|  27 | `OP_GETCV`       | `BB`         | `R(a) = cvget(Syms(b))`
|  28 | `OP_SETCV`       | `BB`         | `cvset(Syms(b),R(a))`
|  29 | `OP_GETCONST`    | `BB`         | `R(a) = constget(Syms(b))`
|  30 | `OP_SETCONST`    | `BB`         | `constset(Syms(b),R(a))`
|  31 | `OP_GETMCNST`    | `BB`         | `R(a) = R(a)::Syms(b)`
|  32 | `OP_SETMCNST`    | `BB`         | `R(a+1)::Syms(b) = R(a)`
|  33 | `OP_GETUPVAR`    | `BBB`        | `R(a) = uvget(b,c)`
|  34 | `OP_SETUPVAR`    | `BBB`        | `uvset(b,c,R(a))`
|  35 | `OP_GETIDX`      | `B`          | `R(a) = R(a)[R(a+1)]`
|  36 | `OP_SETIDX`      | `B`          | `R(a)[R(a+1)] = R(a+2)`
|  37 | `OP_JMP`         | `S`          | `pc+=a`
|  38 | `OP_JMPIF`       | `BS`         | `if R(a) pc+=b`
|  39 | `OP_JMPNOT`      | `BS`         | `if !R(a) pc+=b`
|  40 | `OP_JMPNIL`      | `BS`         | `if R(a)==nil pc+=b`
|  41 | `OP_JMPUW`       | `S`          | `unwind_and_jump_to(a)`
|  42 | `OP_EXCEPT`      | `B`          | `R(a) = exc`
|  43 | `OP_RESCUE`      | `BB`         | `R(b) = R(a).isa?(R(b))`
|  44 | `OP_RAISEIF`     | `B`          | `raise(R(a)) if R(a)`
|  45 | `OP_SSEND`       | `BBB`        | `R(a) = self.send(Syms(b),R(a+1)..,R(a+n+1):R(a+n+2)..) (c=n\|k<<4)`
|  46 | `OP_SSENDB`      | `BBB`        | `R(a) = self.send(Syms(b),R(a+1)..,R(a+n+1):R(a+n+2)..,&R(a+n+2k+1))`
|  47 | `OP_SEND`        | `BBB`        | `R(a) = R(a).send(Syms(b),R(a+1)..,R(a+n+1):R(a+n+2)..) (c=n\|k<<4)`
|  48 | `OP_SENDB`       | `BBB`        | `R(a) = R(a).send(Syms(b),R(a+1)..,R(a+n+1):R(a+n+2)..,&R(a+n+2k+1))`
|  49 | `OP_CALL`        | `-`          | `self.call(*, **, &) (But overlay the current call frame; tailcall)`
|  50 | `OP_SUPER`       | `BB`         | `R(a) = super(R(a+1),... ,R(a+b+1))`
|  51 | `OP_ARGARY`      | `BS`         | `R(a) = argument array (16=m5:r1:m5:d1:lv4)`
|  52 | `OP_ENTER`       | `W`          | `arg setup according to flags (23=m5:o5:r1:m5:k5:d1:b1)`
|  53 | `OP_KEY_P`       | `BB`         | `R(a) = kdict.key?(Syms(b))`
|  54 | `OP_KEYEND`      | `-`          | `raise unless kdict.empty?`
|  55 | `OP_KARG`        | `BB`         | `R(a) = kdict[Syms(b)]; kdict.delete(Syms(b))`
|  56 | `OP_RETURN`      | `B`          | `return R(a) (normal)`
|  57 | `OP_RETURN_BLK`  | `B`          | `return R(a) (in-block return)`
|  58 | `OP_BREAK`       | `B`          | `break R(a)`
|  59 | `OP_BLKPUSH`     | `BS`         | `R(a) = block (16=m5:r1:m5:d1:lv4)`
|  60 | `OP_ADD`         | `B`          | `R(a) = R(a)+R(a+1)`
|  61 | `OP_ADDI`        | `BB`         | `R(a) = R(a)+mrb_int(b)`
|  62 | `OP_SUB`         | `B`          | `R(a) = R(a)-R(a+1)`
|  63 | `OP_SUBI`        | `BB`         | `R(a) = R(a)-mrb_int(b)`
|  64 | `OP_MUL`         | `B`          | `R(a) = R(a)*R(a+1)`
|  65 | `OP_DIV`         | `B`          | `R(a) = R(a)/R(a+1)`
|  66 | `OP_EQ`          | `B`          | `R(a) = R(a)==R(a+1)`
|  67 | `OP_LT`          | `B`          | `R(a) = R(a)<R(a+1)`
|  68 | `OP_LE`          | `B`          | `R(a) = R(a)<=R(a+1)`
|  69 | `OP_GT`          | `B`          | `R(a) = R(a)>R(a+1)`
|  70 | `OP_GE`          | `B`          | `R(a) = R(a)>=R(a+1)`
|  71 | `OP_ARRAY`       | `BB`         | `R(a) = ary_new(R(a),R(a+1)..R(a+b))`
|  72 | `OP_ARRAY2`      | `BBB`        | `R(a) = ary_new(R(b),R(b+1)..R(b+c))`
|  73 | `OP_ARYCAT`      | `B`          | `ary_cat(R(a),R(a+1))`
|  74 | `OP_ARYPUSH`     | `BB`         | `ary_push(R(a),R(a+1)..R(a+b))`
|  75 | `OP_ARYSPLAT`    | `B`          | `R(a) = ary_splat(R(a))`
|  76 | `OP_AREF`        | `BBB`        | `R(a) = R(b)[c]`
|  77 | `OP_ASET`        | `BBB`        | `R(b)[c] = R(a)`
|  78 | `OP_APOST`       | `BBB`        | `*R(a),R(a+1)..R(a+c) = R(a)[b..]`
|  79 | `OP_INTERN`      | `B`          | `R(a) = intern(R(a))`
|  80 | `OP_SYMBOL`      | `BB`         | `R(a) = intern(Pool(b))`
|  81 | `OP_STRING`      | `BB`         | `R(a) = str_dup(Pool(b))`
|  82 | `OP_STRCAT`      | `B`          | `str_cat(R(a),R(a+1))`
|  83 | `OP_HASH`        | `BB`         | `R(a) = hash_new(R(a),R(a+1)..R(a+b*2-1))`
|  84 | `OP_HASHADD`     | `BB`         | `hash_push(R(a),R(a+1)..R(a+b*2))`
|  85 | `OP_HASHCAT`     | `B`          | `R(a) = hash_cat(R(a),R(a+1))`
|  86 | `OP_LAMBDA`      | `BB`         | `R(a) = lambda(Irep(b),OP_L_LAMBDA)`
|  87 | `OP_BLOCK`       | `BB`         | `R(a) = lambda(Irep(b),OP_L_BLOCK)`
|  88 | `OP_METHOD`      | `BB`         | `R(a) = lambda(Irep(b),OP_L_METHOD)`
|  89 | `OP_RANGE_INC`   | `B`          | `R(a) = range_new(R(a),R(a+1),FALSE)`
|  90 | `OP_RANGE_EXC`   | `B`          | `R(a) = range_new(R(a),R(a+1),TRUE)`
|  91 | `OP_OCLASS`      | `B`          | `R(a) = ::Object`
|  92 | `OP_CLASS`       | `BB`         | `R(a) = newclass(R(a),Syms(b),R(a+1))`
|  93 | `OP_MODULE`      | `BB`         | `R(a) = newmodule(R(a),Syms(b))`
|  94 | `OP_EXEC`        | `BB`         | `R(a) = blockexec(R(a),Irep(b))`
|  95 | `OP_DEF`         | `BB`         | `R(a).newmethod(Syms(b),R(a+1)); R(a) = Syms(b)`
|  96 | `OP_ALIAS`       | `BB`         | `alias_method(target_class,Syms(a),Syms(b))`
|  97 | `OP_UNDEF`       | `B`          | `undef_method(target_class,Syms(a))`
|  98 | `OP_SCLASS`      | `B`          | `R(a) = R(a).singleton_class`
|  99 | `OP_TCLASS`      | `B`          | `R(a) = target_class`
| 100 | `OP_DEBUG`       | `BBB`        | `print a,b,c`
| 101 | `OP_ERR`         | `B`          | `raise(LocalJumpError, Pool(a))`
| 102 | `OP_EXT1`        | `-`          | `make 1st operand (a) 16bit`
| 103 | `OP_EXT2`        | `-`          | `make 2nd operand (b) 16bit`
| 104 | `OP_EXT3`        | `-`          | `make 1st and 2nd operands 16bit`
| 105 | `OP_STOP`        | `-`          | `stop VM`
