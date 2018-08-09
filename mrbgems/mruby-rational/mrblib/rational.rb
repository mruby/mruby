class Rational < Numeric
  def initialize(numerator = 0, denominator = 1)
    @numerator = numerator
    @denominator = denominator
  end

  attr_reader :numerator, :denominator
end

def Rational(numerator = 0, denominator = 1)
  Rational.new(numerator, denominator)
end

module ForwardOperatorToRational
  def __forward_operator_to_rational(op, &b)
    original_operator_name = "__original_operator_#{op}_rational"
    alias_method original_operator_name, op
    define_method op do |rhs|
      if rhs.is_a? Rational
        Rational.new(self).send(op, rhs)
      else
        send(original_operator_name, rhs)
      end
    end
  end

  def __forward_operators_to_rational
    __forward_operator_to_rational :+
    __forward_operator_to_rational :-
    __forward_operator_to_rational :*
    __forward_operator_to_rational :/

    singleton_class.undef_method :__forward_operator_to_rational
    singleton_class.undef_method :__forward_operators_to_rational
  end
end

class Fixnum
  extend ForwardOperatorToRational
  __forward_operators_to_rational
end

class Float
  extend ForwardOperatorToRational
  __forward_operators_to_rational
end