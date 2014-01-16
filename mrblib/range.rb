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
    val = self.first
    unless val.respond_to? :succ
      raise TypeError, "can't iterate"
    end

    last = self.last
    return self if (val <=> last) > 0

    while((val <=> last) < 0)
      block.call(val)
      val = val.succ
    end

    if not exclude_end? and (val <=> last) == 0
      block.call(val)
    end
    self
  end

  # redefine #hash 15.3.1.3.15
  def hash
    h = first.hash ^ last.hash
    if self.exclude_end?
      h += 1
    end
    h
  end
end

##
# Range is enumerable
#
# ISO 15.2.14.3
module Enumerable; end
class Range
  include Enumerable
end
