#  The Computer Language Benchmarks Game
#  http://shootout.alioth.debian.org/
#
#  contributed by Karl von Laudermann
#  modified by Jeremy Echols
#  optimized: while loops instead of for..in to avoid closure overhead

size = 600 # ARGV[0].to_i

puts "P4\n#{size} #{size}"

# Cache constants in local variables to avoid repeated constant lookup
iter = 49
limit_squared = 4.0

byte_acc = 0
bit_num = 0

count_size = size - 1

# Use while loops instead of for..in to avoid closure/upvalue overhead
y = 0
while y <= count_size
  x = 0
  while x <= count_size
    zr = 0.0
    zi = 0.0
    cr = (2.0*x/size)-1.5
    ci = (2.0*y/size)-1.0
    escape = false

    # Use while instead of for..in to avoid closure overhead
    i = 0
    while i <= iter
      tr = zr*zr - zi*zi + cr
      ti = 2*zr*zi + ci
      zr = tr
      zi = ti

      if (zr*zr+zi*zi) > limit_squared
        escape = true
        break
      end
      i += 1
    end

    byte_acc = (byte_acc << 1) | (escape ? 0b0 : 0b1)
    bit_num += 1

    # Code is very similar for these cases, but using separate blocks
    # ensures we skip the shifting when it's unnecessary, which is most cases.
    if (bit_num == 8)
      print byte_acc.chr
      byte_acc = 0
      bit_num = 0
    elsif (x == count_size)
      byte_acc <<= (8 - bit_num)
      print byte_acc.chr
      byte_acc = 0
      bit_num = 0
    end
    x += 1
  end
  y += 1
end
