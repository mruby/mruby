class Rational < Numeric
  #
  # call-seq:
  #   rat.inspect -> string
  #
  # Returns the value as a string for inspection.
  #
  #   Rational(2).inspect      #=> "(2/1)"
  #   Rational(-8, 6).inspect  #=> "(-4/3)"
  #   Rational(1, 2).inspect   #=> "(1/2)"
  #
  def inspect
    "(#{to_s})"
  end

  #
  # call-seq:
  #   rat.to_s -> string
  #
  # Returns the value as a string.
  #
  #   Rational(2).to_s      #=> "2/1"
  #   Rational(-8, 6).to_s  #=> "-4/3"
  #   Rational(1, 2).to_s   #=> "1/2"
  #
  def to_s
    "#{numerator}/#{denominator}"
  end

  #
  # call-seq:
  #   rat <=> numeric -> -1, 0, +1, or nil
  #
  # Returns -1, 0, or +1 depending on whether rat is less than, equal to,
  # or greater than numeric. This is the basis for the tests in the Comparable module.
  # Returns nil if the two values are incomparable.
  #
  #   Rational(2, 3) <=> Rational(2, 3)  #=> 0
  #   Rational(5) <=> 5                  #=> 0
  #   Rational(2, 3) <=> Rational(1, 3)  #=> 1
  #   Rational(1, 3) <=> 1               #=> -1
  #   Rational(1, 3) <=> 0.3             #=> 1
  #
  def <=>(other)
    # An Integer or another Rational is compared exactly, by __cmp, which is
    # what lets a numerator too large for a Float still be told apart and what
    # answers at all in a build carrying no Float. Anything else is compared
    # the way CRuby compares it, through Float.
    c = __cmp(other)
    return c unless false == c
    return nil unless Numeric === other
    self.to_f <=> other.to_f
  rescue
    nil
  end

  #
  # call-seq:
  #   rat.floor(ndigits = 0) -> integer or rational
  #
  # Returns the largest number less than or equal to rat, with ndigits
  # decimal digits (an Integer when ndigits is zero or negative).
  #
  #   Rational(7, 2).floor            #=> 3
  #   Rational(-7, 2).floor           #=> -4
  #   Rational(12345, 100).floor(1)   #=> (617/5)
  #   Rational(12345, 100).floor(-1)  #=> 120
  #
  def floor(ndigits = 0)
    __rounding(ndigits, :floor)
  end

  #
  # call-seq:
  #   rat.ceil(ndigits = 0) -> integer or rational
  #
  # Returns the smallest number greater than or equal to rat, with
  # ndigits decimal digits (an Integer when ndigits is zero or negative).
  #
  #   Rational(7, 2).ceil             #=> 4
  #   Rational(-7, 2).ceil            #=> -3
  #   Rational(12345, 100).ceil(-1)   #=> 130
  #
  def ceil(ndigits = 0)
    __rounding(ndigits, :ceil)
  end

  #
  # call-seq:
  #   rat.truncate(ndigits = 0) -> integer or rational
  #
  # Returns rat truncated toward zero, with ndigits decimal digits (an
  # Integer when ndigits is zero or negative).
  #
  #   Rational(7, 2).truncate         #=> 3
  #   Rational(-7, 2).truncate        #=> -3
  #
  def truncate(ndigits = 0)
    __rounding(ndigits, :truncate)
  end

  #
  # call-seq:
  #   rat.round(ndigits = 0, half: :up) -> integer or rational
  #
  # Returns rat rounded to the nearest number with ndigits decimal digits
  # (an Integer when ndigits is zero or negative). A tie is rounded away
  # from zero (half: :up), to the even neighbour (half: :even), or toward
  # zero (half: :down).
  #
  #   Rational(5, 2).round                #=> 3
  #   Rational(-5, 2).round               #=> -3
  #   Rational(5, 2).round(half: :even)   #=> 2
  #   Rational(5, 2).round(half: :down)   #=> 2
  #   Rational(12345, 100).round(1)       #=> (247/2)
  #
  def round(ndigits = 0, half: :up)
    half = half.to_sym if String === half
    case half
    when nil, :up
      mode = :up
    when :even, :down
      mode = half
    else
      raise ArgumentError, "invalid rounding mode: #{half}"
    end
    __rounding(ndigits, mode)
  end

  private

  # Scales rat by 10**ndigits and rounds it to an Integer by mode, in
  # Integer arithmetic only, so that the result is exact.
  def __rounding(ndigits, mode)
    raise TypeError, "not an integer" unless Integer === ndigits
    n = numerator
    d = denominator
    if ndigits > 0
      s = 10 ** ndigits
      n *= s
    elsif ndigits < 0
      s = 10 ** -ndigits
      d *= s
    end
    case mode
    when :floor
      q = n / d
    when :ceil
      q = -(-n / d)
    when :truncate
      q = n < 0 ? -(-n / d) : n / d
    else
      a = n < 0 ? -n : n
      t = a * 2 + d
      q = t / (d * 2)
      # a tie, where a/d is k + 1/2 exactly and q is k + 1
      if mode != :up && t % (d * 2) == 0
        q -= 1 if mode == :down || q % 2 == 1
      end
      q = -q if n < 0
    end
    if ndigits > 0
      Rational(q, s)
    elsif ndigits < 0
      q * s
    else
      q
    end
  end
end

class Numeric
  #
  # call-seq:
  #   num.to_r -> rational
  #
  # Returns the value as a rational.
  #
  #   1.to_r        #=> (1/1)
  #   (1+2i).to_r   #=> (1+2i)/1)
  #   nil.to_r      #=> TypeError
  #
  def to_r
    Rational(self, 1)
  end
end
