##
# chain.rb Enumerator::Chain class
# See Copyright Notice in mruby.h

module Enumerable
  #
  # call-seq:
  #   enum.chain(*enums) -> enumerator_chain
  #
  # Returns an Enumerator::Chain object which can enumerate over this
  # enumerable and the given enumerables in sequence.
  #
  #   e = (1..3).chain([4, 5])
  #   e.to_a  #=> [1, 2, 3, 4, 5]
  #
  def chain(*args)
    Enumerator::Chain.new(self, *args)
  end
end

class Enumerator
  #
  # call-seq:
  #   enum + other_enum -> enumerator_chain
  #
  # Returns an Enumerator::Chain object which can enumerate over this
  # enumerator and the given enumerator in sequence.
  #
  #   e1 = (1..3).each
  #   e2 = [4, 5].each
  #   (e1 + e2).to_a  #=> [1, 2, 3, 4, 5]
  #
  def +(other)
    Chain.new(self, other)
  end

  class Chain
    include Enumerable

    #
    # call-seq:
    #   Enumerator::Chain.new(*enums) -> enumerator_chain
    #
    # Generates a new enumerator which iterates over each one of the
    # given enumerable objects in sequence.
    #
    #   e = Enumerator::Chain.new(1..3, [4, 5])
    #   e.to_a  #=> [1, 2, 3, 4, 5]
    #
    def initialize(*args)
      @enums = args.freeze
      @pos = -1
    end

    #
    # call-seq:
    #   chain.each { |obj| block } -> chain
    #   chain.each                 -> enumerator
    #
    # Iterates over the elements of the first enumerable by calling the
    # each method on it with the given block, then proceeds to the next
    # enumerable in the chain and continues until the end.
    #
    #   e = Enumerator::Chain.new(1..3, [4, 5])
    #   e.each { |x| puts x }
    #   # prints: 1, 2, 3, 4, 5
    #
    def each(&block)
      return to_enum unless block

      i = 0
      while i < @enums.size
        @pos = i
        @enums[i].each(&block)
        i += 1
      end

      self
    end

    #
    # call-seq:
    #   chain.size -> integer or nil
    #
    # Returns the total size of the enumerator chain if all of the
    # chained enumerables define size. Otherwise it returns nil.
    #
    #   Enumerator::Chain.new(1..3, [4, 5]).size  #=> 5
    #   Enumerator::Chain.new(1..3, loop).size    #=> nil
    #
    def size
      @enums.reduce(0) do |a, e|
        return nil unless e.respond_to?(:size)
        a + e.size
      end
    end

    #
    # call-seq:
    #   chain.rewind -> chain
    #
    # Rewinds the enumerator chain by calling the rewind method on each
    # enumerable that has been iterated, in reverse order. Each enumerable
    # that defines a rewind method will be rewound.
    #
    #   e = Enumerator::Chain.new((1..3), [4, 5])
    #   e.next  #=> 1
    #   e.rewind
    #   e.next  #=> 1
    #
    def rewind
      while 0 <= @pos && @pos < @enums.size
        e = @enums[@pos]
        e.rewind if e.respond_to?(:rewind)
        @pos -= 1
      end

      self
    end

    #
    # call-seq:
    #   chain + other_enum -> enumerator_chain
    #
    # Returns a new Enumerator::Chain object which will enumerate over the
    # elements of this chain, followed by the elements of other_enum.
    #
    #   e1 = Enumerator::Chain.new(1..3, [4, 5])
    #   e2 = e1 + [6, 7]
    #   e2.to_a  #=> [1, 2, 3, 4, 5, 6, 7]
    #
    def +(other)
      self.class.new(self, other)
    end

    #
    # call-seq:
    #   chain.inspect -> string
    #
    # Returns a printable version of the enumerator chain.
    #
    #   Enumerator::Chain.new(1..3, [4, 5]).inspect
    #   #=> "#<Enumerator::Chain: [1..3, [4, 5]]>"
    #
    def inspect
      "#<#{self.class}: #{@enums.inspect}>"
    end
  end
end
