class Complex < Numeric
  def self.polar(abs, arg = 0)
    Complex(abs * Math.cos(arg), abs * Math.sin(arg))
  end

  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{real}#{'+' unless imaginary < 0}#{imaginary}i"
  end

  def +@
    Complex(real, imaginary)
  end

  def -@
    Complex(-real, -imaginary)
  end

  def +(rhs)
    if rhs.is_a? Complex
      Complex(real + rhs.real, imaginary + rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex(real + rhs, imaginary)
    end
  end

  def -(rhs)
    if rhs.is_a? Complex
      Complex(real - rhs.real, imaginary - rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex(real - rhs, imaginary)
    end
  end

  def *(rhs)
    if rhs.is_a? Complex
      Complex(real * rhs.real - imaginary * rhs.imaginary, real * rhs.imaginary + rhs.real * imaginary)
    elsif rhs.is_a? Numeric
      Complex(real * rhs, imaginary * rhs)
    end
  end

  def /(rhs)
    if rhs.is_a? Complex
      __div__(rhs)
    elsif rhs.is_a? Numeric
      Complex(real / rhs, imaginary / rhs)
    end
  end
  alias_method :quo, :/

  def abs
    Math.hypot imaginary, real
  end
  alias_method :magnitude, :abs

  def abs2
    real * real + imaginary * imaginary
  end

  def arg
    Math.atan2 imaginary, real
  end
  alias_method :angle, :arg
  alias_method :phase, :arg

  def conjugate
    Complex(real, -imaginary)
  end
  alias_method :conj, :conjugate

  def fdiv(numeric)
    Complex(real / numeric, imaginary / numeric)
  end

  def polar
    [abs, arg]
  end

  def real?
    false
  end

  def rectangular
    [real, imaginary]
  end
  alias_method :rect, :rectangular

  def to_c
    self
  end

  def to_r
    raise RangeError.new "can't convert #{to_s} into Rational" unless imaginary.zero?
    Rational(real, 1)
  end

  alias_method :imag, :imaginary

  Numeric.class_eval do
    def i
      Complex(0, self)
    end
  end
  undef i
end
