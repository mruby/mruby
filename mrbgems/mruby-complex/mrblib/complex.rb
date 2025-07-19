class Complex < Numeric
  #
  # call-seq:
  #   Complex.polar(abs [, arg]) -> complex
  #
  # Returns a complex number in terms of its polar coordinates.
  # abs is the absolute value (magnitude) and arg is the argument (angle).
  #
  #   Complex.polar(3, 0)            #=> (3+0i)
  #   Complex.polar(3, Math::PI/2)   #=> (1.836909530733566e-16+3.0i)
  #   Complex.polar(3, Math::PI)     #=> (-3.0+3.673819061467132e-16i)
  #
  def self.polar(abs, arg = 0)
    Complex(abs * Math.cos(arg), abs * Math.sin(arg))
  end

  #
  # call-seq:
  #   cmp.inspect -> string
  #
  # Returns the value as a string for inspection.
  #
  #   Complex(2).inspect      #=> "(2+0i)"
  #   Complex(-8, 6).inspect  #=> "(-8+6i)"
  #   Complex(1, 2).inspect   #=> "(1+2i)"
  #
  def inspect
    "(#{to_s})"
  end

  #
  # call-seq:
  #   cmp.to_s -> string
  #
  # Returns the value as a string.
  #
  #   Complex(2).to_s      #=> "2+0i"
  #   Complex(-8, 6).to_s  #=> "-8+6i"
  #   Complex(1, -2).to_s  #=> "1-2i"
  #
  def to_s
    "#{real}#{'+' unless imaginary < 0}#{imaginary}#{'*' unless imaginary.finite?}i"
  end

  #
  # call-seq:
  #   +cmp -> cmp
  #
  # Returns self.
  #
  #   +Complex(1, 2)  #=> (1+2i)
  #
  def +@
    self
  end

  #
  # call-seq:
  #   -cmp -> complex
  #
  # Returns the negation of self.
  #
  #   -Complex(1, 2)   #=> (-1-2i)
  #   -Complex(-1, 2)  #=> (1-2i)
  #
  def -@
    Complex(-real, -imaginary)
  end

  #
  # call-seq:
  #   cmp <=> numeric -> -1, 0, +1, or nil
  #
  # Returns -1, 0, or +1 depending on whether cmp is less than, equal to,
  # or greater than numeric. This is the basis for the tests in the Comparable module.
  # Returns nil if the two values are incomparable.
  #
  #   Complex(2, 3) <=> Complex(2, 3)  #=> 0
  #   Complex(5) <=> 5                 #=> 0
  #   Complex(2, 3) <=> 1              #=> 1
  #
  def <=>(other)
    return nil unless other.kind_of?(Numeric)
    self.to_f <=> other.to_f
  rescue
    nil
  end

  #
  # call-seq:
  #   cmp.abs        -> real
  #   cmp.magnitude  -> real
  #
  # Returns the absolute part of its polar form.
  #
  #   Complex(-1).abs         #=> 1.0
  #   Complex(3.0, -4.0).abs  #=> 5.0
  #
  def abs
    Math.hypot imaginary, real
  end
  alias_method :magnitude, :abs

  #
  # call-seq:
  #   cmp.abs2 -> real
  #
  # Returns square of the absolute value.
  #
  #   Complex(-1).abs2         #=> 1
  #   Complex(3.0, -4.0).abs2  #=> 25.0
  #
  def abs2
    real * real + imaginary * imaginary
  end

  #
  # call-seq:
  #   cmp.arg    -> float
  #   cmp.angle  -> float
  #   cmp.phase  -> float
  #
  # Returns the angle part of its polar form.
  #
  #   Complex.polar(3, Math::PI/2).arg  #=> 1.5707963267948966
  #
  def arg
    Math.atan2 imaginary, real
  end
  alias_method :angle, :arg
  alias_method :phase, :arg

  #
  # call-seq:
  #   cmp.conjugate -> complex
  #   cmp.conj      -> complex
  #
  # Returns the complex conjugate.
  #
  #   Complex(1, 2).conjugate  #=> (1-2i)
  #
  def conjugate
    Complex(real, -imaginary)
  end
  alias_method :conj, :conjugate

  #
  # call-seq:
  #   cmp.fdiv(numeric) -> complex
  #
  # Performs division as each part is a float, even if the parts are not floats.
  #
  #   Complex(11, 22).fdiv(3)  #=> (3.6666666666666665+7.333333333333333i)
  #
  def fdiv(numeric)
    Complex(real / numeric, imaginary / numeric)
  end

  #
  # call-seq:
  #   cmp.polar -> array
  #
  # Returns an array; [cmp.abs, cmp.arg].
  #
  #   Complex(1, 2).polar  #=> [2.23606797749979, 1.1071487177940904]
  #
  def polar
    [abs, arg]
  end

  #
  # call-seq:
  #   cmp.real? -> false
  #
  # Returns false.
  #
  #   Complex(1).real?  #=> false
  #
  def real?
    false
  end

  #
  # call-seq:
  #   cmp.rectangular -> array
  #   cmp.rect        -> array
  #
  # Returns an array; [cmp.real, cmp.imag].
  #
  #   Complex(1, 2).rectangular  #=> [1, 2]
  #
  def rectangular
    [real, imaginary]
  end
  alias_method :rect, :rectangular

  #
  # call-seq:
  #   cmp.to_c -> cmp
  #
  # Returns self.
  #
  #   Complex(2).to_c      #=> (2+0i)
  #   Complex(-8, 6).to_c  #=> (-8+6i)
  #
  def to_c
    self
  end

  #
  # call-seq:
  #   cmp.to_r -> rational
  #
  # Returns the value as a rational if possible (the imaginary part should be exactly zero).
  #
  #   Complex(1, 0).to_r    #=> (1/1)
  #   Complex(1, 0.0).to_r  #=> (1/1)
  #   Complex(1, 2).to_r    #=> RangeError
  #
  def to_r
    raise RangeError.new "can't convert #{to_s} into Rational" unless imaginary.zero?
    Rational(real, 1)
  end

  alias_method :imag, :imaginary

  Numeric.class_eval do
    #
    # call-seq:
    #   num.i -> complex
    #
    # Returns the Complex object created from this number and i (0+num*i).
    #
    #   -42.i  #=> (0-42i)
    #   2.0.i  #=> (0+2.0i)
    #
    def i
      Complex(0, self)
    end
  end
  undef i
end

class Numeric
  #
  # call-seq:
  #   num.to_c -> complex
  #
  # Returns the value as a complex.
  #
  #   1.to_c       #=> (1+0i)
  #   -1.to_c      #=> (-1+0i)
  #   1.0.to_c     #=> (1.0+0i)
  #   3.14159.to_c #=> (3.14159+0i)
  #
  def to_c
    Complex(self, 0)
  end
end
