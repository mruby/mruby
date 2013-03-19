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
  
end
