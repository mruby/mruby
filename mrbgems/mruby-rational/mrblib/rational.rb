class Rational < Numeric
  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{numerator}/#{denominator}"
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

  alias quo /

  def <=>(rhs)
    case rhs
    when Integer, Float
      return numerator <=> rhs if denominator == 1
      rhs = Rational(rhs)
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
