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
    return nil unless other.kind_of?(Numeric)
    self.to_f <=> other.to_f
  rescue
    nil
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
