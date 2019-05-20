class Rational < Numeric
  # Override #<, #<=, #>, #>= in Numeric
  prepend Comparable

  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{numerator}/#{denominator}"
  end

  def *(rhs)
    if rhs.is_a? Rational
      Rational(numerator * rhs.numerator, denominator * rhs.denominator)
    elsif rhs.is_a? Integer
      Rational(numerator * rhs, denominator)
    elsif rhs.is_a? Numeric
      numerator * rhs / denominator
    end
  end

  def +(rhs)
    if rhs.is_a? Rational
      Rational(numerator * rhs.denominator + rhs.numerator * denominator, denominator * rhs.denominator)
    elsif rhs.is_a? Integer
      Rational(numerator + rhs * denominator, denominator)
    elsif rhs.is_a? Numeric
      (numerator + rhs * denominator) / denominator
    end
  end

  def -(rhs)
    if rhs.is_a? Rational
      Rational(numerator * rhs.denominator - rhs.numerator * denominator, denominator * rhs.denominator)
    elsif rhs.is_a? Integer
      Rational(numerator - rhs * denominator, denominator)
    elsif rhs.is_a? Numeric
      (numerator - rhs * denominator) / denominator
    end
  end

  def /(rhs)
    if rhs.is_a? Rational
      Rational(numerator * rhs.denominator, denominator * rhs.numerator)
    elsif rhs.is_a? Integer
      Rational(numerator, denominator * rhs)
    elsif rhs.is_a? Numeric
      numerator / rhs / denominator
    end
  end

  def <=>(rhs)
    case rhs
    when Fixnum
      return numerator <=> rhs if denominator == 1
      rhs = Rational(rhs)
    when Float
      return to_f <=> rhs
    end

    case rhs
    when Rational
      (numerator * rhs.denominator - denominator * rhs.numerator) <=> 0
    when Numeric
      (rhs <=> self)&.-@
    else
      nil
    end
  end
end

class Numeric
  def to_r
    Rational(self, 1)
  end
end

module Kernel
  def Rational(numerator = 0, denominator = 1)
    a = numerator
    b = denominator
    a, b = b, a % b until b == 0
    Rational._new(numerator.div(a), denominator.div(a))
  end
end

[:+, :-, :*, :/, :<=>, :==, :<, :<=, :>, :>=].each do |op|
  Fixnum.instance_eval do
    original_operator_name = "__original_operator_#{op}_rational"
    alias_method original_operator_name, op
    define_method op do |rhs|
      if rhs.is_a? Rational
        Rational(self).__send__(op, rhs)
      else
        __send__(original_operator_name, rhs)
      end
    end
  end
  Float.instance_eval do
    original_operator_name = "__original_operator_#{op}_rational"
    alias_method original_operator_name, op
    define_method op do |rhs|
      if rhs.is_a? Rational
        rhs = rhs.to_f
      end
      __send__(original_operator_name, rhs)
    end
  end
end
