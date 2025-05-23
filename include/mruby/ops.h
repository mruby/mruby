/* operand types:
   + Z: no operand
   + B: 8bit
   + BB: 8+8bit
   + BBB: 8+8+8bit
   + BS: 8+16bit
   + BSS: 8+16+16bit
   + S: 16bit
   + W: 24bit
*/

/*-----------------------------------------------------------------------
operation code    operands      semantics
------------------------------------------------------------------------*/
OPCODE(NOP,        Z)        /* no operation */
OPCODE(MOVE,       BB)       /* R[a] = R[b] */
OPCODE(LOADL,      BB)       /* R[a] = Pool[b] */
OPCODE(LOADI8,     BB)       /* R[a] = mrb_int(b) */
OPCODE(LOADINEG,   BB)       /* R[a] = mrb_int(-b) */
OPCODE(LOADI__1,   B)        /* R[a] = mrb_int(-1) */
OPCODE(LOADI_0,    B)        /* R[a] = mrb_int(0) */
OPCODE(LOADI_1,    B)        /* R[a] = mrb_int(1) */
OPCODE(LOADI_2,    B)        /* R[a] = mrb_int(2) */
OPCODE(LOADI_3,    B)        /* R[a] = mrb_int(3) */
OPCODE(LOADI_4,    B)        /* R[a] = mrb_int(4) */
OPCODE(LOADI_5,    B)        /* R[a] = mrb_int(5) */
OPCODE(LOADI_6,    B)        /* R[a] = mrb_int(6) */
OPCODE(LOADI_7,    B)        /* R[a] = mrb_int(7) */
OPCODE(LOADI16,    BS)       /* R[a] = mrb_int(b) */
OPCODE(LOADI32,    BSS)      /* R[a] = mrb_int((b<<16)+c) */
OPCODE(LOADSYM,    BB)       /* R[a] = Syms[b] */
OPCODE(LOADNIL,    B)        /* R[a] = nil */
OPCODE(LOADSELF,   B)        /* R[a] = self */
OPCODE(LOADT,      B)        /* R[a] = true */
OPCODE(LOADF,      B)        /* R[a] = false */
OPCODE(GETGV,      BB)       /* R[a] = getglobal(Syms[b]) */
OPCODE(SETGV,      BB)       /* setglobal(Syms[b], R[a]) */
OPCODE(GETSV,      BB)       /* R[a] = Special[Syms[b]] */
OPCODE(SETSV,      BB)       /* Special[Syms[b]] = R[a] */
OPCODE(GETIV,      BB)       /* R[a] = ivget(Syms[b]) */
OPCODE(SETIV,      BB)       /* ivset(Syms[b],R[a]) */
OPCODE(GETCV,      BB)       /* R[a] = cvget(Syms[b]) */
OPCODE(SETCV,      BB)       /* cvset(Syms[b],R[a]) */
OPCODE(GETCONST,   BB)       /* R[a] = constget(Syms[b]) */
OPCODE(SETCONST,   BB)       /* constset(Syms[b],R[a]) */
OPCODE(GETMCNST,   BB)       /* R[a] = R[a]::Syms[b] */
OPCODE(SETMCNST,   BB)       /* R[a+1]::Syms[b] = R[a] */
OPCODE(GETUPVAR,   BBB)      /* R[a] = uvget(b,c) */
OPCODE(SETUPVAR,   BBB)      /* uvset(b,c,R[a]) */
OPCODE(GETIDX,     B)        /* R[a] = R[a][R[a+1]] */
OPCODE(SETIDX,     B)        /* R[a][R[a+1]] = R[a+2] */
OPCODE(JMP,        S)        /* pc+=a */
OPCODE(JMPIF,      BS)       /* if R[a] pc+=b */
OPCODE(JMPNOT,     BS)       /* if !R[a] pc+=b */
OPCODE(JMPNIL,     BS)       /* if R[a]==nil pc+=b */
OPCODE(JMPUW,      S)        /* unwind_and_jump_to(a) */
OPCODE(EXCEPT,     B)        /* R[a] = exc */
OPCODE(RESCUE,     BB)       /* R[b] = R[a].isa?(R[b]) */
OPCODE(RAISEIF,    B)        /* raise(R[a]) if R[a] */
OPCODE(SSEND,      BBB)      /* R[a] = self.send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..) (c=n|k<<4) */
OPCODE(SSENDB,     BBB)      /* R[a] = self.send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..,&R[a+n+2k+1]) */
OPCODE(SEND,       BBB)      /* R[a] = R[a].send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..) (c=n|k<<4) */
OPCODE(SENDB,      BBB)      /* R[a] = R[a].send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..,&R[a+n+2k+1]) */
OPCODE(CALL,       Z)        /* self.call(*, **, &) (But overlay the current call frame; tailcall) */
OPCODE(SUPER,      BB)       /* R[a] = super(R[a+1],... ,R[a+b+1]) */
OPCODE(ARGARY,     BS)       /* R[a] = argument array (16=m5:r1:m5:d1:lv4) */
OPCODE(ENTER,      W)        /* arg setup according to flags (23=m5:o5:r1:m5:k5:d1:b1) */
OPCODE(KEY_P,      BB)       /* R[a] = kdict.key?(Syms[b]) */
OPCODE(KEYEND,     Z)        /* raise unless kdict.empty? */
OPCODE(KARG,       BB)       /* R[a] = kdict[Syms[b]]; kdict.delete(Syms[b]) */
OPCODE(RETURN,     B)        /* return R[a] (normal) */
OPCODE(RETURN_BLK, B)        /* return R[a] (in-block return) */
OPCODE(BREAK,      B)        /* break R[a] */
OPCODE(BLKPUSH,    BS)       /* R[a] = block (16=m5:r1:m5:d1:lv4) */
OPCODE(ADD,        B)        /* R[a] = R[a]+R[a+1] */
OPCODE(ADDI,       BB)       /* R[a] = R[a]+mrb_int(b) */
OPCODE(SUB,        B)        /* R[a] = R[a]-R[a+1] */
OPCODE(SUBI,       BB)       /* R[a] = R[a]-mrb_int(b) */
OPCODE(MUL,        B)        /* R[a] = R[a]*R[a+1] */
OPCODE(DIV,        B)        /* R[a] = R[a]/R[a+1] */
OPCODE(EQ,         B)        /* R[a] = R[a]==R[a+1] */
OPCODE(LT,         B)        /* R[a] = R[a]<R[a+1] */
OPCODE(LE,         B)        /* R[a] = R[a]<=R[a+1] */
OPCODE(GT,         B)        /* R[a] = R[a]>R[a+1] */
OPCODE(GE,         B)        /* R[a] = R[a]>=R[a+1] */
OPCODE(ARRAY,      BB)       /* R[a] = ary_new(R[a],R[a+1]..R[a+b]) */
OPCODE(ARRAY2,     BBB)      /* R[a] = ary_new(R[b],R[b+1]..R[b+c]) */
OPCODE(ARYCAT,     B)        /* ary_cat(R[a],R[a+1]) */
OPCODE(ARYPUSH,    BB)       /* ary_push(R[a],R[a+1]..R[a+b]) */
OPCODE(ARYSPLAT,   B)        /* R[a] = ary_splat(R[a]) */
OPCODE(AREF,       BBB)      /* R[a] = R[b][c] */
OPCODE(ASET,       BBB)      /* R[b][c] = R[a] */
OPCODE(APOST,      BBB)      /* *R[a],R[a+1]..R[a+c] = R[a][b..] */
OPCODE(INTERN,     B)        /* R[a] = intern(R[a]) */
OPCODE(SYMBOL,     BB)       /* R[a] = intern(Pool[b]) */
OPCODE(STRING,     BB)       /* R[a] = str_dup(Pool[b]) */
OPCODE(STRCAT,     B)        /* str_cat(R[a],R[a+1]) */
OPCODE(HASH,       BB)       /* R[a] = hash_new(R[a],R[a+1]..R[a+b*2-1]) */
OPCODE(HASHADD,    BB)       /* hash_push(R[a],R[a+1]..R[a+b*2]) */
OPCODE(HASHCAT,    B)        /* R[a] = hash_cat(R[a],R[a+1]) */
OPCODE(LAMBDA,     BB)       /* R[a] = lambda(Irep[b],L_LAMBDA) */
OPCODE(BLOCK,      BB)       /* R[a] = lambda(Irep[b],L_BLOCK) */
OPCODE(METHOD,     BB)       /* R[a] = lambda(Irep[b],L_METHOD) */
OPCODE(RANGE_INC,  B)        /* R[a] = range_new(R[a],R[a+1],FALSE) */
OPCODE(RANGE_EXC,  B)        /* R[a] = range_new(R[a],R[a+1],TRUE) */
OPCODE(OCLASS,     B)        /* R[a] = ::Object */
OPCODE(CLASS,      BB)       /* R[a] = newclass(R[a],Syms[b],R[a+1]) */
OPCODE(MODULE,     BB)       /* R[a] = newmodule(R[a],Syms[b]) */
OPCODE(EXEC,       BB)       /* R[a] = blockexec(R[a],Irep[b]) */
OPCODE(DEF,        BB)       /* R[a].newmethod(Syms[b],R[a+1]); R[a] = Syms[b] */
OPCODE(ALIAS,      BB)       /* alias_method(target_class,Syms[a],Syms[b]) */
OPCODE(UNDEF,      B)        /* undef_method(target_class,Syms[a]) */
OPCODE(SCLASS,     B)        /* R[a] = R[a].singleton_class */
OPCODE(TCLASS,     B)        /* R[a] = target_class */
OPCODE(DEBUG,      BBB)      /* print a,b,c */
OPCODE(ERR,        B)        /* raise(LocalJumpError, Pool[a]) */
OPCODE(EXT1,       Z)        /* make 1st operand (a) 16bit */
OPCODE(EXT2,       Z)        /* make 2nd operand (b) 16bit */
OPCODE(EXT3,       Z)        /* make 1st and 2nd operands 16bit */
OPCODE(STOP,       Z)        /* stop VM */
