mrbdump は mrbc コマンドが出力する .mrb ファイルの内容を可読形式で出力するツールです。

```
% cat a.rb
p true

% bin/mrbc a.rb

% ./app/mrbdump/mrbdump.rb a.mrb
Rite Binary File Identify: RITE
Rite Binary File Format Version: "00090000"
Rite Instruction Specification Version: "00090000"
Rite Compiler Type: "MATZ    "
Rite Compiler Version: "00090000"
Rite Binary Data Size: 91
Number of ireps: 1
Start index: 0
Reserved: "        "
Header CRC: 0x8914
irep0:
  len=91
  record identifier="S"
  class or module="C"
  number of local variable=1
  number of register variable=3
  offset of isec block=4
  header CRC=0x469f
  class or module name(?)=""
  ireq lenngth=4
    OP_LOADSELF R1
    OP_LOADT    R2
    OP_SEND     R1      :0      1
    OP_STOP
  ireq CRC=0x36e1
  pool length=0
  pool CRC=0x0000
  syms length=1
    000: p
  syms CRC=0x3241
dummy record len=0 (must be 0)
```
