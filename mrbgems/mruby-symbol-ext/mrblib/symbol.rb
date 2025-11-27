class Symbol
  include Comparable

  alias intern to_sym

  ##
  # call-seq:
  #   sym.capitalize  -> symbol
  #
  # Same as `sym.to_s.capitalize.intern`.

  def capitalize
    (self.to_s.capitalize! || self).to_sym
  end

  ##
  # call-seq:
  #   sym.downcase  -> symbol
  #
  # Same as `sym.to_s.downcase.intern`.

  def downcase
    (self.to_s.downcase! || self).to_sym
  end

  ##
  # call-seq:
  #   sym.upcase    -> symbol
  #
  # Same as `sym.to_s.upcase.intern`.

  def upcase
    (self.to_s.upcase! || self).to_sym
  end

  ##
  # call-seq:
  #   sym.casecmp(other)  -> -1, 0, +1 or nil
  #
  # Case-insensitive version of `Symbol#<=>`.

  def casecmp(other)
    return nil unless other.kind_of?(Symbol)
    lhs =  self.to_s; lhs.upcase!
    rhs = other.to_s.upcase
    lhs <=> rhs
  end

  ##
  # call-seq:
  #   sym.casecmp?(other)  -> true, false, or nil
  #
  # Returns true if sym and other_sym are equal after case folding,
  # false if they are not equal, and nil if other_sym is not a string.

  def casecmp?(sym)
    c = self.casecmp(sym)
    return nil if c.nil?
    return c == 0
  end

  #
  # call-seq:
  #   sym.empty?   -> true or false
  #
  # Returns that _sym_ is :"" or not.

  def empty?
    self.length == 0
  end

end
