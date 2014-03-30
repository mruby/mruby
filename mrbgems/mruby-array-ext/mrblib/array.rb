class Array
  ##
  # call-seq:
  #    ary.uniq! -> ary or nil
  #
  # Removes duplicate elements from +self+.
  # Returns <code>nil</code> if no changes are made (that is, no
  # duplicates are found).
  #
  #    a = [ "a", "a", "b", "b", "c" ]
  #    a.uniq!   #=> ["a", "b", "c"]
  #    b = [ "a", "b", "c" ]
  #    b.uniq!   #=> nil
  #
  def uniq!
    ary = self.dup
    result = []
    while ary.size > 0
      result << ary.shift
      ary.delete(result.last)
    end
    if result.size == self.size
      nil
    else
      self.replace(result)
    end
  end

  ##
  # call-seq:
  #    ary.uniq   -> new_ary
  #
  # Returns a new array by removing duplicate values in +self+.
  #
  #    a = [ "a", "a", "b", "b", "c" ]
  #    a.uniq   #=> ["a", "b", "c"]
  #
  def uniq
    ary = self.dup
    ary.uniq!
    ary
  end

  ##
  # call-seq:
  #    ary - other_ary    -> new_ary
  #
  # Array Difference---Returns a new array that is a copy of
  # the original array, removing any items that also appear in
  # <i>other_ary</i>. (If you need set-like behavior, see the
  # library class Set.)
  #
  #    [ 1, 1, 2, 2, 3, 3, 4, 5 ] - [ 1, 2, 4 ]  #=>  [ 3, 3, 5 ]
  #
  def -(elem)
    raise TypeError, "can't convert #{elem.class} into Array" unless elem.class == Array

    hash = {}
    array = []
    elem.each { |x| hash[x] = true }
    self.each { |x| array << x unless hash[x] }
    array
  end

  ##
  # call-seq:
  #    ary | other_ary     -> new_ary
  #
  # Set Union---Returns a new array by joining this array with
  # <i>other_ary</i>, removing duplicates.
  #
  #    [ "a", "b", "c" ] | [ "c", "d", "a" ]
  #           #=> [ "a", "b", "c", "d" ]
  #
  def |(elem)
    raise TypeError, "can't convert #{elem.class} into Array" unless elem.class == Array

    ary = self + elem
    ary.uniq! or ary
  end

  ##
  # call-seq:
  #    ary & other_ary      -> new_ary
  #
  # Set Intersection---Returns a new array
  # containing elements common to the two arrays, with no duplicates.
  #
  #    [ 1, 1, 3, 5 ] & [ 1, 2, 3 ]   #=> [ 1, 3 ]
  #
  def &(elem)
    raise TypeError, "can't convert #{elem.class} into Array" unless elem.class == Array

    hash = {}
    array = []
    elem.each{|v| hash[v] = true }
    self.each do |v|
      if hash[v]
        array << v
        hash.delete v
      end
    end
    array
  end

  ##
  # call-seq:
  #    ary.flatten -> new_ary
  #    ary.flatten(level) -> new_ary
  #
  # Returns a new array that is a one-dimensional flattening of this
  # array (recursively). That is, for every element that is an array,
  # extract its elements into the new array.  If the optional
  # <i>level</i> argument determines the level of recursion to flatten.
  #
  #    s = [ 1, 2, 3 ]           #=> [1, 2, 3]
  #    t = [ 4, 5, 6, [7, 8] ]   #=> [4, 5, 6, [7, 8]]
  #    a = [ s, t, 9, 10 ]       #=> [[1, 2, 3], [4, 5, 6, [7, 8]], 9, 10]
  #    a.flatten                 #=> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  #    a = [ 1, 2, [3, [4, 5] ] ]
  #    a.flatten(1)              #=> [1, 2, 3, [4, 5]]
  #
  def flatten(depth=nil)
    ar = []
    self.each do |e|
      if e.is_a?(Array) && (depth.nil? || depth > 0)
        ar += e.flatten(depth.nil? ? nil : depth - 1)
      else
        ar << e
      end
    end
    ar
  end

  ##
  # call-seq:
  #    ary.flatten!        -> ary or nil
  #    ary.flatten!(level) -> array or nil
  #
  # Flattens +self+ in place.
  # Returns <code>nil</code> if no modifications were made (i.e.,
  # <i>ary</i> contains no subarrays.)  If the optional <i>level</i>
  # argument determines the level of recursion to flatten.
  #
  #    a = [ 1, 2, [3, [4, 5] ] ]
  #    a.flatten!   #=> [1, 2, 3, 4, 5]
  #    a.flatten!   #=> nil
  #    a            #=> [1, 2, 3, 4, 5]
  #    a = [ 1, 2, [3, [4, 5] ] ]
  #    a.flatten!(1) #=> [1, 2, 3, [4, 5]]
  #
  def flatten!(depth=nil)
    modified = false
    ar = []
    self.each do |e|
      if e.is_a?(Array) && (depth.nil? || depth > 0)
        ar += e.flatten(depth.nil? ? nil : depth - 1)
        modified = true
      else
        ar << e
      end
    end
    if modified
      self.replace(ar)
    else
      nil
    end
  end

  ##
  # call-seq:
  #    ary.compact     -> new_ary
  #
  # Returns a copy of +self+ with all +nil+ elements removed.
  #
  #    [ "a", nil, "b", nil, "c", nil ].compact
  #                      #=> [ "a", "b", "c" ]
  #
  def compact
    result = self.dup
    result.compact!
    result
  end

  ##
  # call-seq:
  #    ary.compact!    -> ary  or  nil
  #
  # Removes +nil+ elements from the array.
  # Returns +nil+ if no changes were made, otherwise returns
  # <i>ary</i>.
  #
  #    [ "a", nil, "b", nil, "c" ].compact! #=> [ "a", "b", "c" ]
  #    [ "a", "b", "c" ].compact!           #=> nil
  #
  def compact!
    result = self.select { |e| e != nil }
    if result.size == self.size
      nil
    else
      self.replace(result)
    end
  end

  # for efficiency
  def reverse_each(&block)
    return to_enum :reverse_each unless block_given?

    i = self.size - 1
    while i>=0
      block.call(self[i])
      i -= 1
    end
    self
  end

  NONE=Object.new
  ##
  #  call-seq:
  #     ary.fetch(index)                    -> obj
  #     ary.fetch(index, default)           -> obj
  #     ary.fetch(index) { |index| block }  -> obj
  #
  #  Tries to return the element at position +index+, but throws an IndexError
  #  exception if the referenced +index+ lies outside of the array bounds.  This
  #  error can be prevented by supplying a second argument, which will act as a
  #  +default+ value.
  #
  #  Alternatively, if a block is given it will only be executed when an
  #  invalid +index+ is referenced.  Negative values of +index+ count from the
  #  end of the array.
  #
  #     a = [ 11, 22, 33, 44 ]
  #     a.fetch(1)               #=> 22
  #     a.fetch(-1)              #=> 44
  #     a.fetch(4, 'cat')        #=> "cat"
  #     a.fetch(100) { |i| puts "#{i} is out of bounds" }
  #                              #=> "100 is out of bounds"
  #

  def fetch(n=nil, ifnone=NONE, &block)
    warn "block supersedes default value argument" if n != nil && ifnone != NONE && block

    idx = n
    if idx < 0
      idx += size
    end
    if idx < 0 || size <= idx
      return block.call(n) if block
      if ifnone == NONE
        raise IndexError, "index #{n} outside of array bounds: #{-size}...#{size}"
      end
      return ifnone
    end
    self[idx]
  end

  ##
  #  call-seq:
  #     ary.fill(obj)                                 -> ary
  #     ary.fill(obj, start [, length])               -> ary
  #     ary.fill(obj, range )                         -> ary
  #     ary.fill { |index| block }                    -> ary
  #     ary.fill(start [, length] ) { |index| block } -> ary
  #     ary.fill(range) { |index| block }             -> ary
  #
  #  The first three forms set the selected elements of +self+ (which
  #  may be the entire array) to +obj+.
  #
  #  A +start+ of +nil+ is equivalent to zero.
  #
  #  A +length+ of +nil+ is equivalent to the length of the array.
  #
  #  The last three forms fill the array with the value of the given block,
  #  which is passed the absolute index of each element to be filled.
  #
  #  Negative values of +start+ count from the end of the array, where +-1+ is
  #  the last element.
  #
  #     a = [ "a", "b", "c", "d" ]
  #     a.fill("x")              #=> ["x", "x", "x", "x"]
  #     a.fill("w", -1)          #=> ["x", "x", "x", "w"]
  #     a.fill("z", 2, 2)        #=> ["x", "x", "z", "z"]
  #     a.fill("y", 0..1)        #=> ["y", "y", "z", "z"]
  #     a.fill { |i| i*i }       #=> [0, 1, 4, 9]
  #     a.fill(-2) { |i| i*i*i } #=> [0, 1, 8, 27]
  #     a.fill(1, 2) { |i| i+1 } #=> [0, 2, 3, 27]
  #     a.fill(0..1) { |i| i+1 } #=> [1, 2, 3, 27]
  #

  def fill(arg0=nil, arg1=nil, arg2=nil, &block)
    if arg0 == nil && arg1 == nil && arg2 == nil && !block
      raise ArgumentError, "wrong number of arguments (0 for 1..3)" 
    end

    beg = len = 0
    ary = []
    if block
      if arg0 == nil && arg1 == nil && arg2 == nil
        # ary.fill { |index| block }                    -> ary
        beg = 0
        len = self.size
      elsif arg0 != nil && arg0.respond_to?(:begin) && arg0.respond_to?(:end)
        # ary.fill(range) { |index| block }             -> ary
        beg = arg0.begin
        beg += self.size if beg < 0
        len = arg0.end - beg + 1
      elsif arg0 != nil
        # ary.fill(start [, length] ) { |index| block } -> ary
        beg = arg0
        beg += self.size if beg < 0
        if arg1 == nil
          len = self.size
        else
          len = arg0 + arg1
        end
      end
    else
      if arg0 != nil && arg1 == nil && arg2 == nil
        # ary.fill(obj)                                 -> ary
        beg = 0
        len = self.size      
      elsif arg0 != nil && arg1 != nil && arg1.respond_to?(:begin) && arg1.respond_to?(:end)
        # ary.fill(obj, range )                         -> ary
        len = self.size
        beg = arg1.begin
        len = arg1.end - beg + 1
      elsif arg0 != nil && arg1 != nil
        # ary.fill(obj, start [, length])               -> ary
        beg = arg1
        beg += self.size if beg < 0
       if arg2 == nil
          len = self.size
        else
          len = arg1 + arg2
        end
      end
    end

    i = beg
    if block
      while i < len
        self[i] = block.call(i)
        i += 1
      end
    else 
      while i < len
        self[i] = arg0
        i += 1
      end
    end
    self
  end
end
