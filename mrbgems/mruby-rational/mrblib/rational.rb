class Rational < Numeric
  def inspect
    "(#{to_s})"
  end

  def to_s
    "#{numerator}/#{denominator}"
  end

  def <=>(other)
    return nil unless other.kind_of?(Numeric)
    self.to_f <=> other.to_f
  rescue
    nil
  end
end

class Numeric
  def to_r
    Rational(self, 1)
  end
end
