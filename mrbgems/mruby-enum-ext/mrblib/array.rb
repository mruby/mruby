class Array
  ##
  # call-seq:
  #   array.minmax -> [min, max]
  #   array.minmax {|a, b| ... } -> [min, max]
  #
  # Returns the least and the greatest element of `self`.
  #
  # This is an optimized version of Enumerable#minmax for arrays: without a
  # block the walk is made in C, where an Integer, a Float and a String are
  # compared without a `<=>` send, as they are by Array#sort.
  #
  #    [3, 1, 2].minmax                    #=> [1, 3]
  #    [3, 1, 2].minmax {|a, b| b <=> a }  #=> [3, 1]
  #
  def minmax(&block)
    block ? super : __minmax
  end
end
