module Enumerable

  #
  # call-seq:
  #   enum.lazy -> lazy_enumerator
  #
  # Returns an Enumerator::Lazy, which redefines most Enumerable
  # methods to postpone enumeration and enumerate values only on an
  # as-needed basis.
  #
  # === Example
  #
  # The following program finds pythagorean triples:
  #
  #   def pythagorean_triples
  #     (1..Float::INFINITY).lazy.flat_map {|z|
  #       (1..z).flat_map {|x|
  #         (x..z).select {|y|
  #           x*x + y*y == z*z
  #         }.map {|y|
  #           [x, y, z]
  #         }
  #       }
  #     }
  #   end
  #   # show first ten pythagorean triples
  #   p pythagorean_triples.take(10).force # take is lazy, so force is needed
  #   p pythagorean_triples.first(10)      # first is eager
  #   # show pythagorean triples less than 100
  #   p pythagorean_triples.take_while { |*, z| z < 100 }.force
  #
  def lazy
    Enumerator::Lazy.new(self)
  end
end

class Enumerator
  # == Acknowledgements
  #
  #   Based on https://github.com/yhara/enumerable-lazy
  #   Inspired by https://github.com/antimon2/enumerable_lz
  #   http://jp.rubyist.net/magazine/?0034-Enumerable_lz (ja)
  class Lazy < Enumerator
    #
    # call-seq:
    #   Lazy.new(obj, &block)
    #
    # Creates a new Lazy enumerator. When the enumerator is actually enumerated
    # (e.g. by calling #force), obj will be enumerated and each value passed
    # to the given block. The block can yield values back by calling yielder.yield.
    # For example, to create a method that acts like Array#select:
    #
    #   def select
    #     Lazy.new(self) do |yielder, value|
    #       yielder.yield(value) if yield(value)
    #     end
    #   end
    #
    def initialize(obj, &block)
      super(){|yielder|
        begin
          obj.each{|x|
            if block
              block.call(yielder, x)
            else
              yielder << x
            end
          }
        rescue StopIteration
        end
      }
    end

    #
    # call-seq:
    #   lazy.to_enum(method = :each, *args)                 -> lazy_enum
    #   lazy.to_enum(method = :each, *args) {|*args| ... }  -> lazy_enum
    #   lazy.enum_for(method = :each, *args)                -> lazy_enum
    #   lazy.enum_for(method = :each, *args) {|*args| ... } -> lazy_enum
    #
    # Similar to Object#to_enum, except it returns a lazy enumerator.
    # This makes it easy to define Enumerable methods that will
    # naturally remain lazy if called on a lazy enumerator.
    #
    #   For example:
    #
    #   module Enumerable
    #     def filter_map(&block)
    #       map(&block).compact
    #     end
    #   end
    #
    def to_enum(meth=:each, *args, &block)
      unless self.respond_to?(meth)
        raise ArgumentError, "undefined method #{meth}"
      end
      lz = Lazy.new(self, &block)
      obj = self
      lz.instance_eval {
        @obj = obj
        @meth = meth
        @args = args
      }
      lz
    end
    alias enum_for to_enum

    #
    # call-seq:
    #   lazy.map {|obj| block } -> lazy_enumerator
    #   lazy.collect {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#map, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.map {|i| i**2 }
    #   #=> #<Enumerator::Lazy: #<Enumerator::Lazy: 1..Infinity>:map>
    #   (1..Float::INFINITY).lazy.map {|i| i**2 }.first(3)
    #   #=> [1, 4, 9]
    #
    def map(&block)
      Lazy.new(self){|yielder, val|
        yielder << block.call(val)
      }
    end
    alias collect map

    #
    # call-seq:
    #   lazy.select {|obj| block } -> lazy_enumerator
    #   lazy.find_all {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#select, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.select {|i| i.even? }.first(3)
    #   #=> [2, 4, 6]
    #
    def select(&block)
      Lazy.new(self){|yielder, val|
        if block.call(val)
          yielder << val
        end
      }
    end
    alias find_all select

    #
    # call-seq:
    #   lazy.reject {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#reject, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.reject {|i| i.even? }.first(3)
    #   #=> [1, 3, 5]
    #
    def reject(&block)
      Lazy.new(self){|yielder, val|
        unless block.call(val)
          yielder << val
        end
      }
    end

    #
    # call-seq:
    #   lazy.grep(pattern) -> lazy_enumerator
    #
    # Like Enumerable#grep, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.grep(1..10).force
    #   #=> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    #
    def grep(pattern)
      Lazy.new(self){|yielder, val|
        if pattern === val
          yielder << val
        end
      }
    end

    #
    # call-seq:
    #   lazy.grep_v(pattern) -> lazy_enumerator
    #
    # Like Enumerable#grep_v, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.grep_v(2..4).first(3)
    #   #=> [1, 5, 6]
    #
    def grep_v(pattern)
      Lazy.new(self){|yielder, val|
        unless pattern === val
          yielder << val
        end
      }
    end

    #
    # call-seq:
    #   lazy.drop(n) -> lazy_enumerator
    #
    # Like Enumerable#drop, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.drop(3).first(3)
    #   #=> [4, 5, 6]
    #
    def drop(n)
      dropped = 0
      Lazy.new(self){|yielder, val|
        if dropped < n
          dropped += 1
        else
          yielder << val
        end
      }
    end

    #
    # call-seq:
    #   lazy.drop_while {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#drop_while, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.drop_while {|i| i < 4 }.first(3)
    #   #=> [4, 5, 6]
    #
    def drop_while(&block)
      dropping = true
      Lazy.new(self){|yielder, val|
        if dropping
          if not block.call(val)
            yielder << val
            dropping = false
          end
        else
          yielder << val
        end
      }
    end

    #
    # call-seq:
    #   lazy.take(n) -> lazy_enumerator
    #
    # Like Enumerable#take, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.take(3).force
    #   #=> [1, 2, 3]
    #
    def take(n)
      if n == 0
        return Lazy.new(self){raise StopIteration}
      end
      taken = 0
      Lazy.new(self){|yielder, val|
        yielder << val
        taken += 1
        if taken >= n
          raise StopIteration
        end
      }
    end

    #
    # call-seq:
    #   lazy.take_while {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#take_while, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.take_while {|i| i < 4 }.force
    #   #=> [1, 2, 3]
    #
    def take_while(&block)
      Lazy.new(self){|yielder, val|
        if block.call(val)
          yielder << val
        else
          raise StopIteration
        end
      }
    end

    #
    # call-seq:
    #   lazy.flat_map {|obj| block } -> lazy_enumerator
    #   lazy.collect_concat {|obj| block } -> lazy_enumerator
    #
    # Like Enumerable#flat_map, but chains operation to be lazy-evaluated.
    #
    #   ["foo", "bar"].lazy.flat_map {|i| i.each_char.lazy}.force
    #   #=> ["f", "o", "o", "b", "a", "r"]
    #
    def flat_map(&block)
      Lazy.new(self){|yielder, val|
        ary = block.call(val)
        # TODO: check ary is an Array
        ary.each{|x|
          yielder << x
        }
      }
    end
    alias collect_concat flat_map

    #
    # call-seq:
    #   lazy.zip(arg, ...) -> lazy_enumerator
    #   lazy.zip(arg, ...) {|arr| block } -> lazy_enumerator
    #
    # Like Enumerable#zip, but chains operation to be lazy-evaluated.
    # However, if a block is given to zip, values are enumerated immediately.
    #
    #   (1..Float::INFINITY).lazy.zip(('a'..'z').cycle).first(3)
    #   #=> [[1, "a"], [2, "b"], [3, "c"]]
    #
    def zip(*args, &block)
      enums = [self] + args
      Lazy.new(self){|yielder, val|
        ary = enums.map{|e| e.next}
        if block
          yielder << block.call(ary)
        else
          yielder << ary
        end
      }
    end

    #
    # call-seq:
    #   lazy.uniq -> lazy_enumerator
    #   lazy.uniq {|item| block } -> lazy_enumerator
    #
    # Like Enumerable#uniq, but chains operation to be lazy-evaluated.
    #
    #   (1..Float::INFINITY).lazy.map {|i| i % 3}.uniq.first(3)
    #   #=> [1, 2, 0]
    #
    def uniq(&block)
      hash = {}
      Lazy.new(self){|yielder, val|
        if block
          v = block.call(val)
        else
          v = val
        end
        unless hash.include?(v)
          yielder << val
          hash[v] = val
        end
      }
    end

    #
    # call-seq:
    #   lazy.force -> array
    #
    # Forces lazy evaluation and returns an array containing the values
    # enumerated by the lazy enumerator. This is an alias for to_a.
    #
    #   (1..Float::INFINITY).lazy.take(3).force
    #   #=> [1, 2, 3]
    #
    alias force to_a
  end
end
