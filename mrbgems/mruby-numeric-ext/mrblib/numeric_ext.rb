class Numeric
  def zero?
    self == 0
  end

  def nonzero?
    if self == 0
      nil
    else
      self
    end
  end

  def positive?
    self > 0
  end

  def negative?
    self < 0
  end

  ##
  #  call-seq:
  #    int.allbits?(mask)  ->  true or false
  #
  #  Returns +true+ if all bits of <code>+int+ & +mask+</code> are 1.
  #
  def allbits?(mask)
    (self & mask) == mask
  end

  ##
  #  call-seq:
  #    int.anybits?(mask)  ->  true or false
  #
  #  Returns +true+ if any bits of <code>+int+ & +mask+</code> are 1.
  #
  def anybits?(mask)
    (self & mask) != 0
  end

  ##
  #  call-seq:
  #    int.nobits?(mask)  ->  true or false
  #
  #  Returns +true+ if no bits of <code>+int+ & +mask+</code> are 1.
  #
  def nobits?(mask)
    (self & mask) == 0
  end
end
