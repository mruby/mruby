##
# Comparable
#
# ISO 15.3.3
module Comparable

  ##
  # call-seq:
  #   obj < other    -> true or false
  #
  # Return true if `self` is less
  # than `other`. Otherwise return
  # false.
  #
  # ISO 15.3.3.2.1
  def < other
    cmp = self <=> other
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
    cmp < 0
  end

  ##
  # call-seq:
  #   obj <= other   -> true or false
  #
  # Return true if `self` is less
  # than or equal to `other`.
  # Otherwise return false.
  #
  # ISO 15.3.3.2.2
  def <= other
    cmp = self <=> other
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
    cmp <= 0
  end

  ##
  # call-seq:
  #   obj == other   -> true or false
  #
  # Return true if `self` is equal
  # to `other`. Otherwise return
  # false.
  #
  # ISO 15.3.3.2.3
  def == other
    cmp = self <=> other
    cmp.equal?(0)
  end

  ##
  # call-seq:
  #   obj > other    -> true or false
  #
  # Return true if `self` is greater
  # than `other`. Otherwise return
  # false.
  #
  # ISO 15.3.3.2.4
  def > other
    cmp = self <=> other
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
    cmp > 0
  end

  ##
  # call-seq:
  #   obj >= other   -> true or false
  #
  # Return true if `self` is greater
  # than or equal to `other`.
  # Otherwise return false.
  #
  # ISO 15.3.3.2.5
  def >= other
    cmp = self <=> other
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
    cmp >= 0
  end

  ##
  # call-seq:
  #   obj.between?(min,max) -> true or false
  #
  # Return true if `self` is greater
  # than or equal to `min` and
  # less than or equal to `max`.
  # Otherwise return false.
  #
  # ISO 15.3.3.2.6
  def between?(min, max)
    # Asked of `<=>` rather than through `>=` and `<=`: an operator answers
    # false for a pair that stands in no order, which is right written out but
    # wrong as the whole of a method whose job is to place the receiver. The
    # operators above refuse such a pair, yet a Float receiver never reaches
    # them, because Float defines its own in C.
    cmp = self <=> min
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{min.class} failed"
    end
    # The max bound is left unasked once the receiver is below the min, as it
    # is when `>=` short-circuits the `and`.
    return false if cmp < 0
    cmp = self <=> max
    if cmp.nil?
      raise ArgumentError, "comparison of #{self.class} with #{max.class} failed"
    end
    cmp <= 0
  end
end
