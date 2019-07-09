##
# Range
#
# ISO 15.2.14
class Range

  ##
  # Calls the given block for each element of +self+
  # and pass the respective element.
  #
  # ISO 15.2.14.4.4
  def each(&block)
    return to_enum :each unless block

    val = self.first
    last = self.last

    # numerics are special
    if (val.kind_of?(Fixnum) || val.kind_of?(Float)) && (last.kind_of?(Fixnum) || last.kind_of?(Float))
      lim = last
      lim += 1 unless exclude_end?
      i = val
      while i < lim
        block.call(i)
        i += 1
      end
      return self
    end

    if val.kind_of?(String) && last.kind_of?(String) # strings are special
      if val.respond_to? :upto
        return val.upto(last, exclude_end?, &block)
      else
        str_each = true
      end
    end

    raise TypeError, "can't iterate" unless val.respond_to? :succ

    return self if (val <=> last) > 0

    while (val <=> last) < 0
      block.call(val)
      val = val.succ
      if str_each
        break if val.size > last.size
      end
    end

    block.call(val) if !exclude_end? && (val <=> last) == 0
    self
  end

  # redefine #hash 15.3.1.3.15
  def hash
    h = first.hash ^ last.hash
    h += 1 if self.exclude_end?
    h
  end
end

##
# Range is enumerable
#
# ISO 15.2.14.3
class Range
  include Enumerable

  def max(&block)
    val = self.first
    last = self.last
    # numerics are special
    if (val.kind_of?(Fixnum) || val.kind_of?(Float)) && (last.kind_of?(Fixnum) || last.kind_of?(Float))
      return nil if val > last
      return nil if val == last && exclude_end?

      max = last
      max -= 1 if exclude_end?
      max = val if block && block.call(val, last) > 0
      return max
    end

    max = nil
    each do |item|
      max =
        if max.nil?
          item
        elsif block && block.call(max, item) > 0
          item
        elsif item > max
          item
        else
          max
        end
    end
    max
  end

  def min(&block)
    val = self.first
    last = self.last

    # numerics are special
    if (val.kind_of?(Fixnum) || val.kind_of?(Float)) && (last.kind_of?(Fixnum) || last.kind_of?(Float))
      return nil if val > last
      return nil if val == last && exclude_end?

      min = val
      if block && block.call(val, last) > 0
        min = last
        min -= 1 if exclude_end?
      end
      return min
    end

    min = nil
    each do |item|
      min =
        if min.nil?
          item
        elsif block && block.call(min, item) < 0
          item
        elsif item < min
          item
        else
          min
        end
    end
    min
  end
end
