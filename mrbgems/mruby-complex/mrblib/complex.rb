class Complex < Numeric
  def initialize(real, imaginary)
    real = real.to_f unless real.is_a? Numeric
    imaginary = imaginary.to_f unless imaginary.is_a? Numeric
    @real = real
    @imaginary = imaginary
  end

  def self.polar(abs, arg = 0)
    Complex(abs * Math.cos(arg), abs * Math.sin(arg))
  end

  def self.rectangular(real, imaginary = 0)
    _new(real, imaginary)
  end

  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{real}#{'+' unless imaginary.negative?}#{imaginary}i"
  end

  def +@
    Complex._new(real, imaginary)
  end

  def -@
    Complex._new(-real, -imaginary)
  end

  def +(rhs)
    if rhs.is_a? Complex
      Complex._new(real + rhs.real, imaginary + rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex._new(real + rhs, imaginary)
    end
  end

  def -(rhs)
    if rhs.is_a? Complex
      Complex._new(real - rhs.real, imaginary - rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex._new(real - rhs, imaginary)
    end
  end

  def *(rhs)
    if rhs.is_a? Complex
      Complex._new(real * rhs.real - imaginary * rhs.imaginary, real * rhs.imaginary + rhs.real * imaginary)
    elsif rhs.is_a? Numeric
      Complex._new(real * rhs, imaginary * rhs)
    end
  end

  def /(rhs)
    if rhs.is_a? Complex
      div = rhs.real * rhs.real + rhs.imaginary * rhs.imaginary
      Complex._new((real * rhs.real + imaginary * rhs.imaginary) / div, (rhs.real * imaginary - real * rhs.imaginary) / div)
    elsif rhs.is_a? Numeric
      Complex._new(real / rhs, imaginary / rhs)
    end
  end
  alias_method :quo, :/

  def ==(rhs)
    if rhs.is_a? Complex
      real == rhs.real && imaginary == rhs.imaginary
    elsif rhs.is_a? Numeric
      imaginary.zero? && real == rhs
    end
  end

  def abs
    Math.sqrt(abs2)
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
    Complex(real.to_f / numeric, imaginary.to_f / numeric)
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

  def to_r
    raise RangeError.new "can't convert #{to_s} into Rational" unless imaginary.zero?
    Rational(real, 1)
  end

  alias_method :imag, :imaginary
end

module Kernel
  def Complex(real, imaginary = 0)
    Complex.rectangular(real, imaginary)
  end
end

[Fixnum, Float].each do |cls|
  [:+, :-, :*, :/, :==].each do |op|
    cls.instance_exec do
      original_operator_name = "__original_operator_#{op}_complex"
      alias_method original_operator_name, op
      define_method op do |rhs|
        if rhs.is_a? Complex
          Complex(self).send(op, rhs)
        else
          send(original_operator_name, rhs)
        end
      end
    end
  end
end
