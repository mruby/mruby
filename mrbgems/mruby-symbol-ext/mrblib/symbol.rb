class Symbol
  include Comparable

  def to_proc
    ->(obj,*args,&block) do
      obj.__send__(self, *args, &block)
    end
  end

  ##
  # call-seq:
  #   sym.length    -> integer
  #
  # Same as <code>sym.to_s.length</code>.

  def length
    self.to_s.length
  end
  alias :size :length

  ##
  # call-seq:
  #   sym.capitalize  -> symbol
  #
  # Same as <code>sym.to_s.capitalize.intern</code>.

  def capitalize
    self.to_s.capitalize.intern
  end

  ##
  # call-seq:
  #   sym.downcase  -> symbol
  #
  # Same as <code>sym.to_s.downcase.intern</code>.

  def downcase
    self.to_s.downcase.intern
  end

  ##
  # call-seq:
  #   sym.upcase    -> symbol
  #
  # Same as <code>sym.to_s.upcase.intern</code>.

  def upcase
    self.to_s.upcase.intern
  end

  ##
  # call-seq:
  #   sym.casecmp(other)  -> -1, 0, +1 or nil
  #
  # Case-insensitive version of <code>Symbol#<=></code>.

  def casecmp(other)
    return nil unless other.kind_of?(Symbol)
    self.to_s.upcase <=> other.to_s.upcase
  end

  #
  # call-seq:
  #   sym.empty?   -> true or false
  #
  # Returns that _sym_ is :"" or not.

  def empty?
    self.to_s.empty?
  end

end
