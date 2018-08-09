class Complex < Numeric
  def initialize(real = 0, imaginary = 0)
    @real = real
    @imaginary = imaginary
  end

  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{real}#{'+'}#{imaginary}i"
  end

  def +@
    Complex.new(real, imaginary)
  end

  def -@
    Complex.new(-real, -imaginary)
  end

  def +(rhs)
    if rhs.is_a? Complex
      Complex.new(real + rhs.real, imaginary + rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex.new(real + rhs, imaginary)
    end
  end

  def -(rhs)
    if rhs.is_a? Complex
      Complex.new(real - rhs.real, imaginary - rhs.imaginary)
    elsif rhs.is_a? Numeric
      Complex.new(real - rhs, imaginary)
    end
  end

  def *(rhs)
    if rhs.is_a? Complex
      Complex.new(real * rhs.real - imaginary * rhs.imaginary, real * rhs.imaginary + rhs.real * imaginary)
    elsif rhs.is_a? Numeric
      Complex.new(real * rhs, imaginary * rhs)
    end
  end

  def /(rhs)
    if rhs.is_a? Complex
      div = rhs.real * rhs.real + rhs.imaginary * rhs.imaginary
      Complex.new((real * rhs.real + imaginary * rhs.imaginary) / div, (rhs.real * imaginary - real * rhs.imaginary) / div)
    elsif rhs.is_a? Numeric
      Complex.new(real / rhs, imaginary / rhs)
    end
  end

  attr_reader :real, :imaginary
end

def Complex(real = 0, imaginary = 0)
  Complex.new(real, imaginary)
end

module ForwardOperatorToComplex
  def __forward_operator_to_complex(op, &b)
    original_operator_name = "__original_operator_#{op}_complex"
    alias_method original_operator_name, op
    define_method op do |rhs|
      if rhs.is_a? Complex
        Complex.new(self).send(op, rhs)
      else
        send(original_operator_name, rhs)
      end
    end
  end

  def __forward_operators_to_complex
    __forward_operator_to_complex :+
    __forward_operator_to_complex :-
    __forward_operator_to_complex :*
    __forward_operator_to_complex :/

    singleton_class.undef_method :__forward_operator_to_complex
    singleton_class.undef_method :__forward_operators_to_complex
  end
end

class Fixnum
  extend ForwardOperatorToComplex
  __forward_operators_to_complex
end

class Float
  extend ForwardOperatorToComplex
  __forward_operators_to_complex
end