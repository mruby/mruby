class Set
  def initialize(enum = nil, &block)
    __init
    return self if enum.nil?

    if block
      __do_with_enum(enum) { add(block.call(_1)) }
      self
    else
      merge(enum)
    end
  end

  # internal method
  def __do_with_enum(enum, &block)
    if enum.respond_to?(:each)
      enum.each(&block)
    else
      raise ArgumentError, "value must be enumerable"
    end
  end

  # Merges the elements of the given enumerable object to the set and returns
  # self.
  #
  # @param [Enumerable] enum The enumerable object to merge elements from
  # @return [Set] self
  def merge(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set merge
      __merge(enum)
    else
      # General path: Add each element from the enumerable
      __do_with_enum(enum) { add(_1) }
      self
    end
  end

  # Replaces the contents of the set with the contents of the given enumerable
  # object and returns self.
  #
  # @param [Enumerable] enum The enumerable object to replace with
  # @return [Set] self
  def replace(enum)
    clear
    merge(enum)
  end

  # Deletes every element that appears in the given enumerable object and
  # returns self.
  #
  # @param [Enumerable] enum The enumerable object containing elements to remove
  # @return [Set] self
  def subtract(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set subtraction
      __subtract(enum)
    else
      # General path: Remove each element from the enumerable
      __do_with_enum(enum) { delete(_1) }
      self
    end
  end

  # Returns a new set containing elements common to the set and the given
  # enumerable object.
  #
  # @param [Enumerable] enum The enumerable object to find common elements with
  # @return [Set] A new set containing elements common to both
  def intersection(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set intersection
      __intersection(enum)
    else
      # General path: Implement in Ruby for any enumerable
      n = Set.new
      __do_with_enum(enum) { n.add(_1) if include?(_1) }
      n
    end
  end

  # Alias for #intersection
  alias & intersection

  # Returns a new set built by merging the set and the elements of the given
  # enumerable object.
  #
  # @param [Enumerable] enum The enumerable object to merge with
  # @return [Set] A new set containing all elements from both
  def union(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set union
      __union(enum)
    else
      # General path: Create a duplicate and merge the enumerable
      dup.merge(enum)
    end
  end

  # Aliases for #union
  alias | union
  alias + union

  # Returns a new set built by duplicating the set, removing every element that
  # appears in the given enumerable object.
  #
  # @param [Enumerable] enum The enumerable object to find elements to remove
  # @return [Set] A new set with elements from self that are not in enum
  def difference(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set difference
      __difference(enum)
    else
      # General path: Create a duplicate and remove the enumerable elements
      result = dup
      __do_with_enum(enum) { result.delete(_1) }
      result
    end
  end

  # Alias for #difference
  alias - difference

  # Returns a new set containing elements exclusive between the set and the given
  # enumerable object.
  #
  # @param [Enumerable] enum The enumerable object to find exclusive elements with
  # @return [Set] A new set containing elements exclusive between both
  def ^(enum)
    if enum.is_a?(Set)
      # Fast path: Call C-implemented function for Set-to-Set XOR
      __xor(enum)
    else
      # General path: Convert enum to a set and calculate (self|s2)-(self&s2)
      s2 = Set.new(enum)
      (self | s2) - (self & s2)
    end
  end

  def flatten_merge(set, seen = Set.new)
    seen.add(set.object_id)
    set.each { |e|
      if e.is_a?(Set)
        if seen.include?(e.object_id)
          raise ArgumentError, "tried to flatten recursive Set"
        end

        flatten_merge(e, seen)
      else
        add(e)
      end
    }
    seen.delete(set.object_id)
    self
  end

  def flatten
    self.class.new.flatten_merge(self)
  end

  def flatten!
    if detect { |e| e.is_a?(Set) }
      replace(flatten())
    else
      nil
    end
  end

  def superset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if size < set.size
    set.all? { include?(_1) }
  end
  alias >= superset?

  def proper_superset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if size <= set.size
    set.all? { include?(_1) }
  end
  alias > proper_superset?

  def subset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if set.size < size
    all? { set.include?(_1) }
  end
  alias <= subset?

  def proper_subset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if set.size <= size
    all? { set.include?(_1) }
  end
  alias < proper_subset?

  def intersect?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    if size < set.size
      any? { set.include?(_1) }
    else
      set.any? { include?(_1) }
    end
  end

  def disjoint?(set)
    !intersect?(set)
  end

  def each(&block)
    return to_enum :each unless block_given?
    # Use C implementation's to_a method and iterate
    to_a.each(&block)
    self
  end

  def delete_if
    return to_enum :delete_if unless block_given?
    select { yield _1 }.each { delete(_1) }
    self
  end

  def keep_if
    return to_enum :keep_if unless block_given?
    reject { yield _1 }.each { delete(_1) }
    self
  end

  def collect!
    return to_enum :collect! unless block_given?
    set = self.class.new
    each { set << yield(_1) }
    replace(set)
  end
  alias map! collect!

  def reject!(&block)
    return to_enum :reject! unless block_given?
    n = size
    delete_if(&block)
    size == n ? nil : self
  end

  def select!(&block)
    return to_enum :select! unless block_given?
    n = size
    keep_if(&block)
    size == n ? nil : self
  end
  alias filter! select!

  def <=>(set)
    return unless set.is_a?(Set)

    case size <=> set.size
    when -1 then -1 if proper_subset?(set)
    when +1 then +1 if proper_superset?(set)
    else 0 if self.==(set)
    end
  end

  def classify
    return to_enum :classify unless block_given?
    h = {}
    each { |i|
      x = yield(i)
      (h[x] ||= self.class.new).add(i)
    }
    h
  end

  def divide(&func)
    return to_enum :divide unless block_given?

    if func.arity == 2
      raise NotImplementedError, "Set#divide with 2 arity block is not implemented."
    end

    Set.new(classify(&func).values)
  end
end
