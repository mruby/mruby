##
# Enumerable
#
module Enumerable
  ##
  # call-seq:
  #    enum.drop(n)               -> array
  #
  # Drops first n elements from <i>enum</i>, and returns rest elements
  # in an array.
  #
  #    a = [1, 2, 3, 4, 5, 0]
  #    a.drop(3)             #=> [4, 5, 0]

  def drop(n)
    raise TypeError, "expected Integer for 1st argument" unless n.kind_of? Integer
    raise ArgumentError, "attempt to drop negative size" if n < 0

    ary = []
    self.each {|e| n == 0 ? ary << e : n -= 1 }
    ary
  end

  ##
  # call-seq:
  #    enum.drop_while {|arr| block }   -> array
  #
  # Drops elements up to, but not including, the first element for
  # which the block returns +nil+ or +false+ and returns an array
  # containing the remaining elements.
  #
  #    a = [1, 2, 3, 4, 5, 0]
  #    a.drop_while {|i| i < 3 }   #=> [3, 4, 5, 0]

  def drop_while(&block)
    ary, state = [], false
    self.each do |e|
      state = true if !state and !block.call(e)
      ary << e if state
    end
    ary
  end
  
end
