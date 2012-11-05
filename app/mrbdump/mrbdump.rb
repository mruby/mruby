#!/usr/bin/env ruby

class MRB
  attr_accessor :rbds
  attr_accessor :nirep
  attr_accessor :sirep
end

class Irep
end

class BinaryBuffer
  def initialize(ary)
    @ary = ary
    @pos = 0
  end
  
  attr_reader :pos

  def readstr(n)
    if n == 0
      ""
    else
      x = @ary[@pos, n]
      @pos += n
      x
    end
  end

  def readn(n)
    if @pos + n > @ary.size
      raise ArgumentError, "try to read #{n} bytes from offset #{@pos} buf we have only #{@ary.size} bytes"
    end
    x = @ary[@pos, n].to_i(16)
    @pos += n
    x
  end

  def read8
    readn(2)
  end

  def read16
    readn(4)
  end

  def read32
    readn(8)
  end
end

class MDB
  @@op = []
  @@op << lambda { |v| "OP_NOP" }
  @@op << lambda { |v| "OP_MOVE\tR%d\tR%d" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_LOADL\tR%d\tL(%d)" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_LOADI\tR%d\t%d" % [ v[:A], v[:sBx] ] }
  @@op << lambda { |v| "OP_LOADSYM\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_LOADNIL\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_LOADSELF\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_LOADT\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_LOADF\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_GETGLOBAL\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETGLOBAL\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETSPECIAL\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETSPECIAL\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETIV\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETIV\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETCV\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETCV\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETCONST\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETCONST\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETMCNST\tR%d\t:%s" % [ v[:A], v[:Bx] ] }
  @@op << lambda { |v| "OP_SETMCNST\t:%s\tR%d" % [ v[:Bx], v[:A] ] }
  @@op << lambda { |v| "OP_GETUPVAR\tR%d\t%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_SETUPVAR\tR%d\t%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_JMP\t\t%03d" % [ v[:i]+v[:sBx] ] }
  @@op << lambda { |v| "OP_JMPIF\tR%d\t%03d" % [ v[:A], v[:i]+v[:sBx] ] }
  @@op << lambda { |v| "OP_JMPNOT\tR%d\t%03d" % [ v[:A], v[:i]+v[:sBx] ] }
  @@op << lambda { |v| "OP_ONERR\t%03d" % [ v[:i]+v[:sBx] ] }
  @@op << lambda { |v| "OP_RESCUE\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_POPERR\t%s" % v[:A] }
  @@op << lambda { |v| "OP_RAISE\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_EPUSH\t:I(%d)" % [ v[:n]+v[:Bx] ] }
  @@op << lambda { |v| "OP_EPOP\t%s" % v[:A] }
  @@op << lambda { |v| "OP_SEND\tR%d\t:%s\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_SENDB\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_FSEND\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_CALL\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_SUPER\tR%d\t%d" % [ v[:A], v[:C] ] }
  @@op << lambda { |v| "OP_ARGARY\tR%d\t%s" % [ v[:A], v[:Ba] ] }
  @@op << lambda { |v| "OP_ENTER\t%s" % v[:Ax] }
  @@op << lambda { |v| "OP_KARG\tR%d\t%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_KDICT\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_RETURN\tR%d (%d)" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_TAILCALL\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_BLKPUSH\tR%d\t%s" % [ v[:A], v[:Ba] ] }
  @@op << lambda { |v| "OP_ADD\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_ADDI\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_SUB\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_SUBI\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_MUL\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_DIV\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_EQ\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_LT\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_LE\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_GT\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_GE\tR%d\t:%s\t%d" % [ v[:A], v[:Bs], v[:C] ] }
  @@op << lambda { |v| "OP_ARRAY\tR%d\tR%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_ARYCAT\tR%d\tR%d" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_ARYPUSH\tR%d\tR%d" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_AREF\tR%d\tR%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_ASET\tR%d\tR%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_APOST\tR%d\t%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_STRING\tR%d\t%d" % [ v[:A], v[:n]+v[:Bx] ] }
  @@op << lambda { |v| "OP_STRCAT\tR%d\tR%d" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_HASH\tR%d\tR%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_LAMBDA\tR%d\tI(%d)\t%d" % [ v[:A], v[:n]+v[:b], v[:c]]}
  @@op << lambda { |v| "OP_RANGE\tR%d\tR%d\t%d" % [ v[:A], v[:B], v[:C] ] }
  @@op << lambda { |v| "OP_OCLASS\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_CLASS\tR%d\t:%s" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_MODULE\tR%d\t:%s" % [ v[:A], v[:Bs] ] }
  @@op << lambda { |v| "OP_EXEC\tR%d\tI(%d)" % [ v[:A], v[:n]+v[:Bx] ] }
  @@op << lambda { |v| "OP_METHOD\tR%d\t:%s" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_SCLASS\tR%d\tR%d" % [ v[:A], v[:B] ] }
  @@op << lambda { |v| "OP_TCLASS\tR%d" % v[:A] }
  @@op << lambda { |v| "OP_DEBUG" }
  @@op << lambda { |v| "OP_STOP" }
  @@op << lambda { |v| "OP_ERR\tL(%d)" % [ v[:Bx] ] }
  @@op_unknown = lambda { |v| "OP_unknown %d\t%d\t%d\t%d" % [ v[:op], v[:A], v[:B], v[:C]] }

  def getarg_a(x)
    (x >> 23) & 0x1ff
  end

  def getarg_b(x)
    (x >> 14) & 0x1ff
  end

  def disasm(x, n, i)
    c = x & 0x7f
    if c < @@op.size
      insn = @@op[c]
    else
      insn = @@op_unknown
    end

    v = { :op => c, 
          :i => i, 
          :A => (x >> 23) & 0x1ff,
    	  :B => (x >> 14) & 0x1ff,
    	  :C => (x >> 7)  & 0x7f,
    	  :b => (x >> 9)  & 0x3fff,
    	  :c => (x >> 7)  & 0x3,
    	  :n => n,
	  :Ax => format("%d:%d:%d:%d:%d:%d:%d",
                        (x>>25)&0x1f, (x>>20)&0x1f, (x>>19)&0x1, (x>>14)&0x1f,
		        (x>>9)&0x1f, (x>>8)&0x1, (x>>7)&0x1),
	  :Ba => format("%d:%d:%d:%d",
                         (x>>17)&0x3f, (x>>16)&0x1, (x>>11)&0x1f, (x>>7)&0xf),
	  :Bs => (x >> 14) & 0x1ff,	# XXX: lookup symbol
	  :Bx => (x >> 7) & 0xffff,
	  :sBx => ((x >> 7) & 0xffff) - 0x7fff
	  }
    puts "    " + insn.call(v)
  end

  def self.disasm(*args)
    self.new.disasm(*args)
  end
end

filename = ARGV[0]
data = File.open(filename).read
mrb = MRB.new

# load_rite_header()
bbuf = BinaryBuffer.new(data)
printf "Rite Binary File Identify: %s\n", bbuf.readstr(4)
printf "Rite Binary File Format Version: \"%s\"\n", bbuf.readstr(8)
printf "Rite Instruction Specification Version: \"%s\"\n", bbuf.readstr(8)
printf "Rite Compiler Type: \"%s\"\n", bbuf.readstr(8)
printf "Rite Compiler Version: \"%s\"\n", bbuf.readstr(8)
mrb.rbds = bbuf.read32
printf "Rite Binary Data Size: %u\n", mrb.rbds
mrb.nirep = bbuf.read16
printf "Number of ireps: %u\n", mrb.nirep
mrb.sirep = bbuf.read16
printf "Start index: %u\n", mrb.sirep
printf "Reserved: \"%s\"\n", bbuf.readstr(8)
printf "Header CRC: 0x%x\n", bbuf.read16

# Read binary data section
(0...mrb.nirep).each do |i|
  # load_rite_irep_record()
  len = bbuf.read32
  next if len == 0

  printf "irep%u:\n", i
  printf "  len=%d\n", len
  printf "  record identifier=\"%s\"\n", bbuf.readstr(1)
  printf "  class or module=\"%s\"\n", bbuf.readstr(1)
  printf "  number of local variable=%u\n", bbuf.read16
  printf "  number of register variable=%u\n", bbuf.read16
  offset = bbuf.read16
  printf "  offset of isec block=%u\n", offset
  printf "  header CRC=0x%x\n", bbuf.read16
  printf "  class or module name(?)=\"%s\"\n", bbuf.readstr(offset - 4)

  # ISEQ BLOCK
  iseqlen = bbuf.read32
  printf "  ireq lenngth=%d\n", iseqlen
  (0...iseqlen).each { |j|
    x = bbuf.read32
    MDB.disasm(x, i, j)
  }
  printf "  ireq CRC=0x%04x\n", bbuf.read16

  # POOL BLOCK
  poollen = bbuf.read32
  printf "  pool length=%d\n", poollen
  (0...poollen).each { |i|
    bbuf.read8
    pdl = bbuf.read16
    str = bbuf.readstr(pdl)
    printf "    %03d: %s\n", i, str
  }
  printf "  pool CRC=0x%04x\n", bbuf.read16
  
  # SYMS BLOCK
  symslen = bbuf.read32
  printf "  syms length=%d\n", symslen
  (0...symslen).each { |i|
    snl = bbuf.read16
    name = bbuf.readstr(snl)
    printf "    %03d: %s\n", i, name
  }
  printf "  syms CRC=0x%04x\n", bbuf.read16
end

drl = bbuf.read32
printf "dummy record len=%d (must be 0)\n", drl
