class Symbol
  include Comparable

  alias intern to_sym

  # The case methods read the name with Symbol#name, as CRuby's do with
  # rb_sym2str(): #to_s may be redefined, and the String it returns may be
  # frozen (Ruby 4.1's Symbol#to_s returns the interned frozen name), so it
  # is neither read nor modified in place here.

  ##
  # call-seq:
  #   sym.capitalize  -> symbol
  #
  # Same as `sym.to_s.capitalize.intern`.

  def capitalize
    name.capitalize.to_sym
  end

  ##
  # call-seq:
  #   sym.downcase  -> symbol
  #
  # Same as `sym.to_s.downcase.intern`.

  def downcase
    name.downcase.to_sym
  end

  ##
  # call-seq:
  #   sym.upcase    -> symbol
  #
  # Same as `sym.to_s.upcase.intern`.

  def upcase
    name.upcase.to_sym
  end

  ##
  # call-seq:
  #   sym.casecmp(other)  -> -1, 0, +1 or nil
  #
  # Case-insensitive version of `Symbol#<=>`.

  def casecmp(other)
    return nil unless Symbol === other
    name.upcase <=> other.name.upcase
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

  ##
  # call-seq:
  #   sym.succ  -> symbol
  #   sym.next  -> symbol
  #
  # Same as `sym.to_s.succ.intern`.
  #
  #   :a.succ     #=> :b
  #   :az.succ    #=> :ba
  #   :a9.next    #=> :b0

  def succ
    self.to_s.succ.to_sym
  end
  alias next succ

  #
  # call-seq:
  #   sym.empty?   -> true or false
  #
  # Returns that _sym_ is :"" or not.

  def empty?
    self.length == 0
  end

end
