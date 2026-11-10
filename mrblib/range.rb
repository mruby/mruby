##
# Range
#
# ISO 15.2.14
class Range
  ##
  # Range is enumerable
  #
  # ISO 15.2.14.3
  include Enumerable

  ##
  # Calls the given block for each element of `self`
  # and pass the respective element.
  #
  # ISO 15.2.14.4.4
  def each(&block)
    return to_enum(:each) unless block

    val = self.begin
    last = self.end

    if Integer === val && last.nil?
      i = val
      while true
        yield(i)
        i += 1
      end
      return self
    end

    if String === val && last.nil?
      if val.respond_to? :__upto_endless
        return val.__upto_endless(&block)
      else
        str_each = true
      end
    end

    # a range of symbols steps their names, as String#upto does, when
    # mruby-symbol-ext and mruby-string-ext are there to give it
    if Symbol === val && (last.nil? || Symbol === last) && val.respond_to?(:succ)
      name = val.to_s
      if last.nil?
        return name.__upto_endless { |s| yield s.to_sym } if name.respond_to? :__upto_endless
      elsif name.respond_to? :upto
        name.upto(last.to_s, exclude_end?) { |s| yield s.to_sym }
        return self
      end
    end

    if Integer === val && Integer === last # integers are special
      lim = last
      lim += 1 unless exclude_end?
      i = val
      while i < lim
        yield(i)
        i += 1
      end
      return self
    end

    if String === val && String === last # strings are special
      if val.respond_to? :upto
        return val.upto(last, exclude_end?, &block)
      else
        str_each = true
      end
    end

    raise TypeError, "can't iterate" unless val.respond_to? :succ

    return self if (val <=> last) > 0

    while (val <=> last) < 0
      yield(val)
      val = val.succ
      if str_each
        break if val.size > last.size
      end
    end

    yield(val) if !exclude_end? && (val <=> last) == 0
    self
  end

  # redefine #hash 15.3.1.3.15
  def hash
    # Use self.begin/self.end instead of first/last to handle endless/beginless ranges
    h = self.begin.hash ^ self.end.hash
    h += 1 if self.exclude_end?
    h
  end

  ##
  # call-seq:
  #    rng.to_a                   -> array
  #    rng.entries                -> array
  #
  # Returns an array containing the items in the range.
  #
  #   (1..7).to_a  #=> [1, 2, 3, 4, 5, 6, 7]
  #   (1..).to_a   #=> RangeError: cannot convert endless range to an array
  def to_a
    a = __num_to_a
    return a if a
    super
  end
  alias entries to_a
end
