class Rational < Numeric
  # Override #<, #<=, #>, #>= in Numeric
  prepend Comparable

  def initialize(numerator = 0, denominator = 1)
    @numerator = numerator
    @denominator = denominator

    _simplify
  end

  def inspect
    "(#{to_s})"
  end

  def to_f
    @numerator.to_f / @denominator.to_f
  end

  def to_i
    to_f.to_i
  end

  def to_r
    self
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
      return @numerator <=> rhs if @denominator == 1
      rhs = Rational(rhs)
    when Float
      return to_f <=> rhs
    end
    case rhs
    when Rational
      (@numerator * rhs.denominator - @denominator * rhs.numerator) <=> 0
    when Numeric
      return rhs <=> self
    else
      nil
    end
  end

  def negative?
    numerator.negative?
  end

  def _simplify
    a = numerator
    b = denominator
    a, b = b, a % b until b.zero?
    @numerator = @numerator.div(a)
    @denominator = @denominator.div(a)
  end

  attr_reader :numerator, :denominator
end

def Rational(numerator = 0, denominator = 1)
  Rational.new(numerator, denominator)
end

[:+, :-, :*, :/, :<=>, :==, :<, :<=, :>, :>=].each do |op|
  Fixnum.instance_exec do
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
  Float.instance_exec do
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
