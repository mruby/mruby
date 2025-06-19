class Set
  # internal method
  def __do_with_enum(enum, &block)
    if enum.respond_to?(:each)
      enum.each(&block)
    else
      raise ArgumentError, "value must be enumerable"
    end
  end

  # internal method to get internal hash (compatibility bridge)
  def __get_hash
    # For C implementation, create a Ruby hash representation
    # This is only used by Ruby fallback methods
    hash = {}
    each { |element| hash[element] = true }
    hash
  end


  # Helper method for initialize with block
  def __init_with_block(enum, &block)
    __do_with_enum(enum) { |o| add(block.call(o)) }
    self
  end

  # Helper method for merge with enumerable
  def __merge_enum(enum)
    __do_with_enum(enum) { |o| add(o) }
    self
  end

  # Helper method for subtract with enumerable
  def __subtract_enum(enum)
    __do_with_enum(enum) { |o| delete(o) }
    self
  end

  # Helper method for intersection with enumerable
  def __intersection_enum(enum)
    n = Set.new
    __do_with_enum(enum) { |o| n.add(o) if include?(o) }
    n
  end

  # Helper method for complex equality checks
  def __equal_fallback(other)
    if other.is_a?(self.class) && self.size == other.size
      other.all? { |o| include?(o) }
    else
      false
    end
  end

  def flatten_merge(set, seen = Set.new)
    seen.add(set.object_id)
    set.each { |e|
      if e.is_a?(Set)
        if seen.include?(e_id = e.object_id)
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
    set.all? { |o| include?(o) }
  end
  alias >= superset?

  def proper_superset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if size <= set.size
    set.all? { |o| include?(o) }
  end
  alias > proper_superset?

  def subset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if set.size < size
    all? { |o| set.include?(o) }
  end
  alias <= subset?

  def proper_subset?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    return false if set.size <= size
    all? { |o| set.include?(o) }
  end
  alias < proper_subset?

  def intersect?(set)
    raise ArgumentError, "value must be a set" unless set.is_a?(Set)
    if size < set.size
      any? { |o| set.include?(o) }
    else
      set.any? { |o| include?(o) }
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
    select { |o| yield o }.each { |o| delete(o) }
    self
  end

  def keep_if
    return to_enum :keep_if unless block_given?
    reject { |o| yield o }.each { |o| delete(o) }
    self
  end

  def collect!
    return to_enum :collect! unless block_given?
    set = self.class.new
    each { |o| set << yield(o) }
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

  # Additional convenience methods that leverage C optimizations

  # Returns true if this set is a proper subset of the given set
  def proper_subset_of?(set)
    proper_subset?(set)
  end

  # Returns true if this set is a proper superset of the given set
  def proper_superset_of?(set)
    proper_superset?(set)
  end

  # Returns true if this set is a subset of the given set
  def subset_of?(set)
    subset?(set)
  end

  # Returns true if this set is a superset of the given set
  def superset_of?(set)
    superset?(set)
  end

  # Efficiently merge multiple enumerables at once
  def merge_all(*enums)
    enums.each { |enum| merge(enum) }
    self
  end

  # Efficiently subtract multiple enumerables at once
  def subtract_all(*enums)
    enums.each { |enum| subtract(enum) }
    self
  end

  # Create a new set with elements from multiple enumerables
  def self.union(*enums)
    result = new
    enums.each { |enum| result.merge(enum) }
    result
  end

  # Create a new set with intersection of multiple sets
  def self.intersection(*sets)
    return new if sets.empty?

    result = sets.first.dup
    sets[1..-1].each { |set| result = result & set }
    result
  end

  # Optimized version of replace for arrays
  def replace_with_array(array)
    clear
    add_all(*array)
    self
  end

  # Fast check if set contains exactly these elements
  def contains_exactly?(*elements)
    size == elements.size && include_all?(*elements)
  end

  # Fast check if set overlaps with given elements
  def overlaps_with?(*elements)
    include_any?(*elements)
  end
end
