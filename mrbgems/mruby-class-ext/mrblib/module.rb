class Module

  ##
  # call-seq:
  #   mod < other   ->  true, false, or nil
  #
  # Returns true if <i>mod</i> is a subclass of <i>other</i>. Returns
  # <code>nil</code> if there's no relationship between the two.
  # (Think of the relationship in terms of the class definition:
  # "class A < B" implies "A < B".)
  #
  def <(other)
    raise TypeError, 'compared with non class/module' unless other.is_a?(Module)
    if self.equal?(other)
      false
    else
      self <= other
    end
  end

  ##
  # call-seq:
  #   mod <= other   ->  true, false, or nil
  #
  # Returns true if <i>mod</i> is a subclass of <i>other</i> or
  # is the same as <i>other</i>. Returns
  # <code>nil</code> if there's no relationship between the two.
  # (Think of the relationship in terms of the class definition:
  # "class A < B" implies "A < B".)
  def <=(other)
    raise TypeError, 'compared with non class/module' unless other.is_a?(Module)
    if self.ancestors.include?(other)
      return true
    elsif other.ancestors.include?(self)
      return false
    end
  end

end
