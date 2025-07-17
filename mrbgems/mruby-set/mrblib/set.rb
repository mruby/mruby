class Set
  #
  # call-seq:
  #   Set.new(enum = nil) -> set
  #   Set.new(enum = nil) { |obj| block } -> set
  #
  # Creates a new set containing the elements of the given enumerable object.
  # If a block is given, the elements are preprocessed by the given block.
  #
  #   Set.new([1, 2, 3])           #=> #<Set: {1, 2, 3}>
  #   Set.new([1, 2, 2, 3])        #=> #<Set: {1, 2, 3}>
  #   Set.new([1, 2, 3]) { |x| x * 2 }  #=> #<Set: {2, 4, 6}>
  #
  def initialize(enum = nil, &block)
    __init
    return self if enum.nil?

    if block
      __do_with_enum(enum) { add(block.call(_1)) }
    else
      merge(enum)
    end
    self
  end

  # internal method
  def __do_with_enum(enum, &block)
    if enum.respond_to?(:each)
      enum.each(&block)
    else
      raise ArgumentError, "value must be enumerable"
    end
  end

  #
  # call-seq:
  #   set.merge(enum) -> self
  #
  # Merges the elements of the given enumerable object to the set and returns self.
  #
  #   set = Set.new([1, 2])
  #   set.merge([2, 3, 4])  #=> #<Set: {1, 2, 3, 4}>
  #   set                   #=> #<Set: {1, 2, 3, 4}>
  #
  def merge(enum)
    __merge(enum) || __do_with_enum(enum) { |o| add(o) }
    self
  end

  #
  # call-seq:
  #   set.replace(enum) -> self
  #
  # Replaces the contents of the set with the contents of the given enumerable
  # object and returns self.
  #
  #   set = Set.new([1, 2, 3])
  #   set.replace([4, 5, 6])  #=> #<Set: {4, 5, 6}>
  #   set                     #=> #<Set: {4, 5, 6}>
  #
  def replace(enum)
    clear
    merge(enum)
  end

  #
  # call-seq:
  #   set.subtract(enum) -> self
  #
  # Deletes every element that appears in the given enumerable object and
  # returns self.
  #
  #   set = Set.new([1, 2, 3, 4])
  #   set.subtract([2, 4])  #=> #<Set: {1, 3}>
  #   set                   #=> #<Set: {1, 3}>
  #
  def subtract(enum)
    __subtract(enum) || __do_with_enum(enum) { |o| delete(o) }
    self
  end

  #
  # call-seq:
  #   set.intersection(enum) -> new_set
  #   set & enum -> new_set
  #
  # Returns a new set containing elements common to the set and the given
  # enumerable object.
  #
  #   Set.new([1, 2, 3]).intersection([2, 3, 4])  #=> #<Set: {2, 3}>
  #   Set.new([1, 2, 3]) & [2, 3, 4]              #=> #<Set: {2, 3}>
  #
  def intersection(enum)
    __intersection(enum) || begin
      n = Set.new
      __do_with_enum(enum) { |o| n.add(o) if include?(o) }
      n
    end
  end

  # Alias for #intersection
  alias & intersection

  #
  # call-seq:
  #   set.union(enum) -> new_set
  #   set | enum -> new_set
  #   set + enum -> new_set
  #
  # Returns a new set built by merging the set and the elements of the given
  # enumerable object.
  #
  #   Set.new([1, 2]).union([2, 3, 4])  #=> #<Set: {1, 2, 3, 4}>
  #   Set.new([1, 2]) | [2, 3, 4]       #=> #<Set: {1, 2, 3, 4}>
  #   Set.new([1, 2]) + [2, 3, 4]       #=> #<Set: {1, 2, 3, 4}>
  #
  def union(enum)
    __union(enum) || dup.merge(enum)
  end

  # Aliases for #union
  alias | union
  alias + union

  #
  # call-seq:
  #   set.difference(enum) -> new_set
  #   set - enum -> new_set
  #
  # Returns a new set built by duplicating the set, removing every element that
  # appears in the given enumerable object.
  #
  #   Set.new([1, 2, 3, 4]).difference([2, 4])  #=> #<Set: {1, 3}>
  #   Set.new([1, 2, 3, 4]) - [2, 4]            #=> #<Set: {1, 3}>
  #
  def difference(enum)
    __difference(enum) || begin
      result = dup
      __do_with_enum(enum) { |o| result.delete(o) }
      result
    end
  end

  # Alias for #difference
  alias - difference

  #
  # call-seq:
  #   set ^ enum -> new_set
  #
  # Returns a new set containing elements exclusive between the set and the given
  # enumerable object. (set ^ enum) is equivalent to ((set | enum) - (set & enum)).
  #
  #   Set.new([1, 2, 3]) ^ [2, 3, 4]  #=> #<Set: {1, 4}>
  #   Set.new([1, 2]) ^ [2, 3]        #=> #<Set: {1, 3}>
  #
  def ^(enum)
    __xor(enum) || begin
      s2 = Set.new(enum)
      (self | s2) - (self & s2)
    end
  end

  #
  # call-seq:
  #   set.each { |obj| block } -> set
  #   set.each -> enumerator
  #
  # Calls the given block once for each element in the set, passing the element
  # as parameter. Returns an enumerator if no block is given.
  #
  #   Set.new([1, 2, 3]).each { |x| puts x }
  #   # prints: 1, 2, 3
  #   #=> #<Set: {1, 2, 3}>
  #
  def each(&block)
    return to_enum :each unless block_given?
    # Use C implementation's to_a method and iterate
    to_a.each(&block)
    self
  end

  #
  # call-seq:
  #   set.delete_if { |obj| block } -> set
  #   set.delete_if -> enumerator
  #
  # Deletes every element of the set for which block evaluates to true, and
  # returns self. Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5])
  #   set.delete_if { |x| x.even? }  #=> #<Set: {1, 3, 5}>
  #   set                            #=> #<Set: {1, 3, 5}>
  #
  def delete_if
    return to_enum :delete_if unless block_given?
    select { yield _1 }.each { delete(_1) }
    self
  end

  #
  # call-seq:
  #   set.keep_if { |obj| block } -> set
  #   set.keep_if -> enumerator
  #
  # Deletes every element of the set for which block evaluates to false, and
  # returns self. Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5])
  #   set.keep_if { |x| x.even? }  #=> #<Set: {2, 4}>
  #   set                          #=> #<Set: {2, 4}>
  #
  def keep_if
    return to_enum :keep_if unless block_given?
    reject { yield _1 }.each { delete(_1) }
    self
  end

  #
  # call-seq:
  #   set.collect! { |obj| block } -> set
  #   set.map! { |obj| block } -> set
  #   set.collect! -> enumerator
  #   set.map! -> enumerator
  #
  # Replaces the elements with ones returned by collect().
  # Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3])
  #   set.collect! { |x| x * 2 }  #=> #<Set: {2, 4, 6}>
  #   set                         #=> #<Set: {2, 4, 6}>
  #
  def collect!
    return to_enum :collect! unless block_given?
    set = self.class.new
    each { set << yield(_1) }
    replace(set)
  end
  alias map! collect!

  #
  # call-seq:
  #   set.reject! { |obj| block } -> set or nil
  #   set.reject! -> enumerator
  #
  # Equivalent to Set#delete_if, but returns nil if no changes were made.
  # Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5])
  #   set.reject! { |x| x.even? }  #=> #<Set: {1, 3, 5}>
  #   set.reject! { |x| x > 10 }   #=> nil
  #
  def reject!(&block)
    return to_enum :reject! unless block_given?
    n = size
    delete_if(&block)
    size == n ? nil : self
  end

  #
  # call-seq:
  #   set.select! { |obj| block } -> set or nil
  #   set.filter! { |obj| block } -> set or nil
  #   set.select! -> enumerator
  #   set.filter! -> enumerator
  #
  # Equivalent to Set#keep_if, but returns nil if no changes were made.
  # Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5])
  #   set.select! { |x| x.even? }  #=> #<Set: {2, 4}>
  #   set.select! { |x| x.even? }  #=> nil
  #
  def select!(&block)
    return to_enum :select! unless block_given?
    n = size
    keep_if(&block)
    size == n ? nil : self
  end
  alias filter! select!

  #
  # call-seq:
  #   set.classify { |obj| block } -> hash
  #   set.classify -> enumerator
  #
  # Classifies the set by the return value of the given block and returns a
  # hash of {value => set of elements} pairs. Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5, 6])
  #   set.classify { |x| x % 3 }
  #   #=> {1=>#<Set: {1, 4}>, 2=>#<Set: {2, 5}>, 0=>#<Set: {3, 6}>}
  #
  def classify
    return to_enum :classify unless block_given?
    h = {}
    each { |i|
      x = yield(i)
      (h[x] ||= self.class.new).add(i)
    }
    h
  end

  #
  # call-seq:
  #   set.divide { |obj1, obj2| block } -> set
  #   set.divide -> enumerator
  #
  # Divides the set into a set of subsets according to the commonality defined
  # by the given block. Returns an enumerator if no block is given.
  #
  #   set = Set.new([1, 2, 3, 4, 5, 6])
  #   set.divide { |x, y| (x % 3) == (y % 3) }
  #   #=> #<Set: {#<Set: {1, 4}>, #<Set: {2, 5}>, #<Set: {3, 6}>}>
  #
  def divide(&func)
    return to_enum :divide unless block_given?

    if func.arity == 2
      raise NotImplementedError, "Set#divide with 2 arity block is not implemented."
    end

    Set.new(classify(&func).values)
  end
end
