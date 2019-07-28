##
# chain.rb Enumerator::Chain class
# See Copyright Notice in mruby.h

module Enumerable
  def chain(*args)
    Enumerator::Chain.new(self, *args)
  end
end

class Enumerator
  def +(other)
    Chain.new(self, other)
  end

  class Chain
    include Enumerable

    def initialize(*args)
      @enums = args
    end

    def each(&block)
      return to_enum unless block_given?

      @enums.each { |e| e.each(&block) }

      self
    end

    def size
      @enums.reduce(0) do |a, e|
        return nil unless e.respond_to?(:size)
        a + e.size
      end
    end

    def rewind
      i = @enums.size - 1
      while 0 <= i
        e = @enums[i]
        e.rewind if e.respond_to?(:rewind)
        i -= 1
      end

      self
    end

    def +(other)
      self.class.new(self, other)
    end

    def inspect
      "#<#{self.class}: #{@enums.inspect}>"
    end
  end
end
