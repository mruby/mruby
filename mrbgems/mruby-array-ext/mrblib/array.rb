class Array
  ##
  # call-seq:
  #    ary.uniq!                -> ary or nil
  #    ary.uniq! { |item| ... } -> ary or nil
  #
  # Removes duplicate elements from +self+.
  # Returns <code>nil</code> if no changes are made (that is, no
  # duplicates are found).
  #
  #    a = [ "a", "a", "b", "b", "c" ]
  #    a.uniq!   #=> ["a", "b", "c"]
  #    b = [ "a", "b", "c" ]
  #    b.uniq!   #=> nil
  #    c = [["student","sam"], ["student","george"], ["teacher","matz"]]
  #    c.uniq! { |s| s.first } # => [["student", "sam"], ["teacher", "matz"]]
  #
  def uniq!(&block)
    if block
      hash = {}
      result = []
      self.each do |val|
        key = block.call(val)
        unless hash.key?(key)
          hash[key] = true
          result << val
        end
      end

      if result.size == self.size
        nil
      else
        self.replace(result)
      end
    else
      __uniq!
    end
  end

  ##
  # call-seq:
  #    ary.uniq                -> new_ary
  #    ary.uniq { |item| ... } -> new_ary
  #
  # Returns a new array by removing duplicate values in +self+.
  #
  #    a = [ "a", "a", "b", "b", "c" ]
  #    a.uniq   #=> ["a", "b", "c"]
  #
  #    b = [["student","sam"], ["student","george"], ["teacher","matz"]]
  #    b.uniq { |s| s.first } # => [["student", "sam"], ["teacher", "matz"]]
  #
  def uniq(&block)
    if block
      ary = self.dup
      ary.uniq!(&block)
      ary
    else
      __uniq
    end
  end

  # for efficiency
  def reverse_each(&block)
    return to_enum :reverse_each unless block

    i = self.size - 1
    while i>=0
      block.call(self[i])
      i -= 1
    end
    self
  end

  ##
  #  call-seq:
  #     ary.fetch(index)                    -> obj
  #     ary.fetch(index, default)           -> obj
  #     ary.fetch(index) { |index| block }  -> obj
  #
  #  Tries to return the element at position +index+, but throws an IndexError
  #  exception if the referenced +index+ lies outside of the array bounds. This
  #  error can be prevented by supplying a second argument, which will act as a
  #  +default+ value.
  #
  #  Alternatively, if a block is given it will only be executed when an
  #  invalid +index+ is referenced.
  #
  #  Negative values of +index+ count from the end of the array.
  #
  #     a = [ 11, 22, 33, 44 ]
  #     a.fetch(1)               #=> 22
  #     a.fetch(-1)              #=> 44
  #     a.fetch(4, 'cat')        #=> "cat"
  #     a.fetch(100) { |i| puts "#{i} is out of bounds" }
  #                              #=> "100 is out of bounds"
  #

  def fetch(n, ifnone=NONE, &block)
    #warn "block supersedes default value argument" if !n.nil? && ifnone != NONE && block

    if block
      # Block case: use shared index helper + Ruby block handling
      normalized_index = __normalize_index(n)
      if normalized_index
        self[normalized_index]
      else
        block.call(n)
      end
    else
      # Fast C implementation for non-block cases
      __fetch(n, ifnone, NONE)
    end
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
    if arg0.nil? && arg1.nil? && arg2.nil? && !block
      raise ArgumentError, "wrong number of arguments (given 0, expected 1..3)"
    end

    # Use shared C argument parser for all cases
    start, length = __fill_parse_arg(arg0, arg1, arg2, &block)

    if block
      # Block-based filling in Ruby
      i = start
      while i < start + length
        self[i] = block.call(i)
        i += 1
      end
    else
      # Use fast C implementation for value filling
      __fill_exec(start, length, arg0)
    end

    self
  end

  ##
  #  call-seq:
  #     ary.delete_if { |item| block }  -> ary
  #     ary.delete_if                   -> Enumerator
  #
  #  Deletes every element of +self+ for which block evaluates to +true+.
  #
  #  The array is changed instantly every time the block is called, not after
  #  the iteration is over.
  #
  #  See also Array#reject!
  #
  #  If no block is given, an Enumerator is returned instead.
  #
  #     scores = [ 97, 42, 75 ]
  #     scores.delete_if {|score| score < 80 }   #=> [97]

  def delete_if(&block)
    return to_enum :delete_if unless block

    result = []
    idx = 0
    len = size
    while idx < len
      elem = self[idx]
      result << elem unless block.call(elem)
      idx += 1
    end

    self.replace(result)
  end

  ##
  #  call-seq:
  #     ary.reject! { |item| block }  -> ary or nil
  #     ary.reject!                   -> Enumerator
  #
  #  Equivalent to Array#delete_if, deleting elements from +self+ for which the
  #  block evaluates to +true+, but returns +nil+ if no changes were made.
  #
  #  The array is changed instantly every time the block is called, not after
  #  the iteration is over.
  #
  #  See also Enumerable#reject and Array#delete_if.
  #
  #  If no block is given, an Enumerator is returned instead.

  def reject!(&block)
    return to_enum :reject! unless block

    result = []
    idx = 0
    len = size
    while idx < len
      elem = self[idx]
      result << elem unless block.call(elem)
      idx += 1
    end

    return nil if len == result.size

    self.replace(result)
  end



  ##
  #  call-seq:
  #     ary.bsearch {|x| block }  -> elem
  #
  #  By using binary search, finds a value from this array which meets
  #  the given condition in O(log n) where n is the size of the array.
  #
  #  You can use this method in two use cases: a find-minimum mode and
  #  a find-any mode. In either case, the elements of the array must be
  #  monotone (or sorted) with respect to the block.
  #
  #  In find-minimum mode (this is a good choice for typical use case),
  #  the block must return true or false, and there must be an index i
  #  (0 <= i <= ary.size) so that:
  #
  #  - the block returns false for any element whose index is less than
  #    i, and
  #  - the block returns true for any element whose index is greater
  #    than or equal to i.
  #
  #  This method returns the i-th element. If i is equal to ary.size,
  #  it returns nil.
  #
  #     ary = [0, 4, 7, 10, 12]
  #     ary.bsearch {|x| x >=   4 } #=> 4
  #     ary.bsearch {|x| x >=   6 } #=> 7
  #     ary.bsearch {|x| x >=  -1 } #=> 0
  #     ary.bsearch {|x| x >= 100 } #=> nil
  #
  #  In find-any mode (this behaves like libc's bsearch(3)), the block
  #  must return a number, and there must be two indices i and j
  #  (0 <= i <= j <= ary.size) so that:
  #
  #  - the block returns a positive number for ary[k] if 0 <= k < i,
  #  - the block returns zero for ary[k] if i <= k < j, and
  #  - the block returns a negative number for ary[k] if
  #    j <= k < ary.size.
  #
  #  Under this condition, this method returns any element whose index
  #  is within i...j. If i is equal to j (i.e., there is no element
  #  that satisfies the block), this method returns nil.
  #
  #     ary = [0, 4, 7, 10, 12]
  #     # try to find v such that 4 <= v < 8
  #     ary.bsearch {|x| 1 - (x / 4).truncate } #=> 4 or 7
  #     # try to find v such that 8 <= v < 10
  #     ary.bsearch {|x| 4 - (x / 2).truncate } #=> nil
  #
  #  You must not mix the two modes at a time; the block must always
  #  return either true/false, or always return a number. It is
  #  undefined which value is actually picked up at each iteration.

  def bsearch(&block)
    return to_enum :bsearch unless block

    if idx = bsearch_index(&block)
      self[idx]
    else
      nil
    end
  end

  ##
  #  call-seq:
  #     ary.bsearch_index {|x| block }  -> int or nil
  #
  #  By using binary search, finds an index of a value from this array which
  #  meets the given condition in O(log n) where n is the size of the array.
  #
  #  It supports two modes, depending on the nature of the block and they are
  #  exactly the same as in the case of #bsearch method with the only difference
  #  being that this method returns the index of the element instead of the
  #  element itself. For more details consult the documentation for #bsearch.

  def bsearch_index(&block)
    return to_enum :bsearch_index unless block

    low = 0
    high = size
    satisfied = false

    while low < high
      mid = ((low+high)/2).truncate
      res = block.call self[mid]

      case res
      when 0 # find-any mode: Found!
        return mid
      when Numeric # find-any mode: Continue...
        in_lower_half = res < 0
      when true # find-min mode
        in_lower_half = true
        satisfied = true
      when false, nil # find-min mode
        in_lower_half = false
      else
        raise TypeError, 'invalid block result (must be numeric, true, false or nil)'
      end

      if in_lower_half
        high = mid
      else
        low = mid + 1
      end
    end

    satisfied ? low : nil
  end

  ##
  #  call-seq:
  #     ary.keep_if { |item| block } -> ary
  #     ary.keep_if                  -> Enumerator
  #
  #  Deletes every element of +self+ for which the given block evaluates to
  #  +false+.
  #
  #  See also Array#select!
  #
  #  If no block is given, an Enumerator is returned instead.
  #
  #     a = [1, 2, 3, 4, 5]
  #     a.keep_if { |val| val > 3 } #=> [4, 5]

  def keep_if(&block)
    return to_enum :keep_if unless block

    result = []
    idx = 0
    len = size
    while idx < len
      elem = self[idx]
      result << elem if block.call(elem)
      idx += 1
    end

    self.replace(result)
  end

  ##
  #  call-seq:
  #     ary.select!  {|item| block } -> ary or nil
  #     ary.select!                  -> Enumerator
  #
  #  Invokes the given block passing in successive elements from +self+,
  #  deleting elements for which the block returns a +false+ value.
  #
  #  If changes were made, it will return +self+, otherwise it returns +nil+.
  #
  #  See also Array#keep_if
  #
  #  If no block is given, an Enumerator is returned instead.

  def select!(&block)
    return to_enum :select! unless block

    result = []
    idx = 0
    len = size
    while idx < len
      elem = self[idx]
      result << elem if block.call(elem)
      idx += 1
    end

    return nil if len == result.size

    self.replace(result)
  end

  ##
  # call-seq:
  #   ary.dig(idx, ...)                 -> object
  #
  # Extracts the nested value specified by the sequence of <i>idx</i>
  # objects by calling +dig+ at each step, returning +nil+ if any
  # intermediate step is +nil+.
  #
  def dig(idx,*args)
    idx = idx.__to_int
    n = self[idx]
    if args.size > 0
      n&.dig(*args)
    else
      n
    end
  end

  ##
  # call-seq:
  #    ary.permutation { |p| block }          -> ary
  #    ary.permutation                        -> Enumerator
  #    ary.permutation(n) { |p| block }       -> ary
  #    ary.permutation(n)                     -> Enumerator
  #
  # When invoked with a block, yield all permutations of length +n+ of the
  # elements of the array, then return the array itself.
  #
  # If +n+ is not specified, yield all permutations of all elements.
  #
  # The implementation makes no guarantees about the order in which the
  # permutations are yielded.
  #
  # If no block is given, an Enumerator is returned instead.
  #
  # Examples:
  #
  #  a = [1, 2, 3]
  #  a.permutation.to_a    #=> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  #  a.permutation(1).to_a #=> [[1],[2],[3]]
  #  a.permutation(2).to_a #=> [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
  #  a.permutation(3).to_a #=> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  #  a.permutation(0).to_a #=> [[]] # one permutation of length 0
  #  a.permutation(4).to_a #=> []   # no permutations of length 4
  def permutation(n=self.size, &block)
    n = n.__to_int
    return to_enum(:permutation, n) unless block
    size = self.size
    if n == 0
      yield []
    elsif 0 < n && n <= size
      i = 0
      while i<size
        result = [self[i]]
        if n-1 > 0
          ary = self[0...i] + self[i+1..-1]
          ary.permutation(n-1) do |c|
            yield result + c
          end
        else
          yield result
        end
        i += 1
      end
    end
    self
  end

  ##
  # call-seq:
  #    ary.combination(n) { |c| block }    -> ary
  #    ary.combination(n)                  -> Enumerator
  #
  # When invoked with a block, yields all combinations of length +n+ of elements
  # from the array and then returns the array itself.
  #
  # The implementation makes no guarantees about the order in which the
  # combinations are yielded.
  #
  # If no block is given, an Enumerator is returned instead.
  #
  # Examples:
  #
  #    a = [1, 2, 3, 4]
  #    a.combination(1).to_a  #=> [[1],[2],[3],[4]]
  #    a.combination(2).to_a  #=> [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
  #    a.combination(3).to_a  #=> [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
  #    a.combination(4).to_a  #=> [[1,2,3,4]]
  #    a.combination(0).to_a  #=> [[]] # one combination of length 0
  #    a.combination(5).to_a  #=> []   # no combinations of length 5

  def combination(n, &block)
    n = n.__to_int
    return to_enum(:combination, n) unless block
    size = self.size
    if n == 0
      yield []
    elsif n == 1
      i = 0
      while i<size
        yield [self[i]]
        i += 1
      end
    elsif n <= size
      i = 0
      while i<size
        result = [self[i]]
        self[i+1..-1].combination(n-1) do |c|
          yield result + c
        end
        i += 1
      end
    end
    self
  end

  ##
  # call-seq:
  #    ary.transpose -> new_ary
  #
  # Assumes that self is an array of arrays and transposes the rows and columns.
  #
  # If the length of the subarrays don't match, an IndexError is raised.
  #
  # Examples:
  #
  #    a = [[1,2], [3,4], [5,6]]
  #    a.transpose   #=> [[1, 3, 5], [2, 4, 6]]

  def transpose
    return [] if empty?

    column_count = nil
    self.each do |row|
      raise TypeError unless row.is_a?(Array)
      column_count ||= row.size
      raise IndexError, 'element size differs' unless column_count == row.size
    end

    Array.new(column_count) do |column_index|
      self.map { |row| row[column_index] }
    end
  end

  ##
  #  call-seq:
  #    ary.to_h                ->   Hash
  #    ary.to_h{|item| ... }   ->   Hash
  #
  # Returns the result of interpreting <i>array</i> as an array of
  # <tt>[key, value]</tt> pairs. If a block is given, it should
  # return <tt>[key, value]</tt> pairs to construct a hash.
  #
  #     [[:foo, :bar], [1, 2]].to_h
  #       # => {:foo => :bar, 1 => 2}
  #     [1, 2].to_h{|x| [x, x*2]}
  #       # => {1 => 2, 2 => 4}
  #
  def to_h(&blk)
    h = {}
    self.each do |v|
      v = blk.call(v) if blk
      raise TypeError, "wrong element type #{v.class}" unless Array === v
      raise ArgumentError, "wrong array length (expected 2, was #{v.length})" unless v.length == 2
      h[v[0]] = v[1]
    end
    h
  end

  alias append push
  alias prepend unshift
  alias filter! select!

  ##
  # call-seq:
  #   ary.fetch_values(idx, ...)                 -> array
  #   ary.fetch_values(idx, ...) { |i| block } -> array
  #
  # Returns an array containing the values associated with the given indexes.
  # but also raises <code>IndexError</code> when one of indexes can't be found.
  # Also see <code>Array#values_at</code> and <code>Array#fetch</code>.
  #
  #   a = ["cat", "dog", "cow"]
  #
  #   a.fetch_values(2, 0)                #=> ["cow", "cat"]
  #   a.fetch_values(2, 5)                # raises KeyError
  #   a.fetch_values(2, 5) {|i| "BIRD" }  #=> ["cow", "BIRD"]
  #
  def fetch_values(*idx, &block)
    if block
      idx.map do |i|
        self.fetch(i, &block)
      end
    else
      # Fast path: use C implementation for non-block cases
      idx.map do |i|
        __fetch(i, NONE, NONE)
      end
    end
  end

  ##
  # call-seq:
  #   ary.product(*arys)                  ->   array
  #   ary.product(*arys) { |item| ... }   ->   self
  def product(*arys, &block)
    size = arys.size
    i = size
    while i > 0
      i -= 1
      unless arys[i].kind_of?(Array)
        raise TypeError, "no implicit conversion into Array"
      end
    end

    i = size
    total = self.size
    total *= arys[i -= 1].size while i > 0

    if block
      result = self
      list = ->(*, e) { block.call e }
      class << list; alias []= call; end
    else
      result = [nil] * total
      list = result
    end

    i = 0
    while i < total
      group = [nil] * (size + 1)
      j = size
      n = i
      while j > 0
        j -= 1
        a = arys[j]
        b = a.size
        group[j + 1] = a[n % b]
        n /= b
      end
      group[0] = self[n]
      list[i] = group
      i += 1
    end

    result
  end

  ##
  # call-seq:
  #   ary.repeated_combination(n) { |combination| ... }   ->   self
  #   ary.repeated_combination(n)                         ->   enumerator
  #
  # A +combination+ method that contains the same elements.
  def repeated_combination(n, &block)
    raise TypeError, "no implicit conversion into Integer" unless 0 <=> n
    return to_enum(:repeated_combination, n) unless block
    __repeated_combination(n, false, &block)
  end

  ##
  # call-seq:
  #   ary.repeated_permutation(n) { |permutation| ... }   ->   self
  #   ary.repeated_permutation(n)                         ->   enumerator
  #
  # A +permutation+ method that contains the same elements.
  def repeated_permutation(n, &block)
    n = n.__to_int
    raise TypeError, "no implicit conversion into Integer" unless 0 <=> n
    return to_enum(:repeated_permutation, n) unless block
    __repeated_combination(n, true, &block)
  end

  def __repeated_combination(n, permutation, &block)
    n = n.__to_int
    case n
    when 0
      yield []
    when 1
      i = 0
      while i < self.size
        yield [self[i]]
        i += 1
      end
    else
      if n > 0
        v = [0] * n
        while true
          tmp = [nil] * n
          i = 0
          while i < n
            tmp[i] = self[v[i]]
            i += 1
          end

          yield tmp

          tmp = self.size
          i = n - 1
          while i >= 0
            v[i] += 1
            break if v[i] < tmp
            i -= 1
          end
          break unless v[0] < tmp
          i = 1
          while i < n
            unless v[i] < tmp
              if permutation
                v[i] = 0
              else
                v[i] = v[i - 1]
              end
            end
            i += 1
          end
        end
      end
    end

    self
  end
end
