##
# modifying existing methods
##

# See /mrblib/kernel.rb
module Kernel
  def loop
    return to_enum :loop unless block_given?

    while(true)
      yield
    end
  rescue => StopIteration
    nil
  end
end

# See /mrblib/numeric.rb
module Integral
  def times &block
    return to_enum :times unless block_given?

    i = 0
    while i < self
      block.call i
      i += 1
    end
    self
  end
end

# See /mrblib/enum.rb
module Enumerable
  def collect(&block)
    return to_enum :collect unless block_given?

    ary = []
    self.each{|val|
      ary.push(block.call(val))
    }
    ary
  end
  alias map collect
end

# See /mrblib/array.rb
class Array
  def each(&block)
    return to_enum :each unless block_given?

    idx, length = -1, self.length-1
    while idx < length and length <= self.length and length = self.length-1
      elm = self[idx += 1]
      unless elm
        if elm == nil and length >= self.length
          break
        end
      end
      block.call(elm)
    end
    self
  end
end

# See /mrblib/hash.rb
class Hash
  def each(&block)
    return to_enum :each unless block_given?

    self.keys.each { |k| block.call [k, self[k]] }
    self
  end
end

# See /mrblib/range.rb
class Range
  def each &block
    return to_enum :each unless block_given?

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
end
