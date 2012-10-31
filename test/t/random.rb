##
# Random Test

if self.methods.include?(:rand) and self.methods.include?(:srand)

  assert('Kernel#rand') do
    r = rand

    r.class == Float
  end

  assert('Kernel#rand(num)') do
    r1 = rand(100)
    r2 = rand(100)

    r1.class == Fixnum and r1 != r2
  end

  assert('Kernel#rand entropy check') do
    ary = []
    size = 1000
    size.times do
      ary << rand(size * size)
    end

    ary.uniq.size > ary.size * 0.9
  end


  assert('Kernel#srand') do
    num = 12345678
    seeds = []
    ary1 = []
    ary2 = []
    ary3 = []

    srand(num)
    ary1 << rand      << rand      << rand      << rand      << rand
    ary1 << rand(100) << rand(100) << rand(100) << rand(100) << rand(100)

    seeds << srand
    ary2 << rand      << rand      << rand      << rand      << rand
    ary2 << rand(100) << rand(100) << rand(100) << rand(100) << rand(100)

    seeds << srand(num)
    ary3 << rand      << rand      << rand      << rand      << rand
    ary3 << rand(100) << rand(100) << rand(100) << rand(100) << rand(100)

    seeds << srand

    ary1 == ary3 and ary1 != ary2 and seeds[0] == seeds[2] and seeds.all?{|i| i > 0 }
  end
end
