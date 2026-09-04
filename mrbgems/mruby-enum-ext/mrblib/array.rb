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

  ##
  # call-seq:
  #   array.count                 -> int
  #   array.count(obj)            -> int
  #   array.count {|element| ... } -> int
  #
  # Returns the number of elements, of those equal to `obj`, or of those the
  # block answers true for.
  #
  # This is an optimized version of Enumerable#count for arrays: the length is
  # read off the array, and an argument is counted by a walk made in C, where
  # the pair is compared with `mrb_equal()` as Array#index compares one. An
  # argument decides even where a block came with it, as in CRuby, where
  # Enumerable#count took the block.
  #
  #    [1, 2, 4, 2].count       #=> 4
  #    [1, 2, 4, 2].count(2)    #=> 2
  #    [1, 2, 4, 2].count {|x| x.even? }  #=> 3
  #
  def count(v = Enumerable::NONE, &block)
    if Enumerable::NONE.equal?(v)
      block ? super : size
    else
      __count(v)
    end
  end
end
