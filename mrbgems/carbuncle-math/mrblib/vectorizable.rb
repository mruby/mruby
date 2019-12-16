module Carbuncle
  class Point; end
  class Vector3; end
  class Vector4; end

  # Represent a vectorizable element, it's any element than allows vector operations.
  # All vectorizable elements are also Enumerable by default.
  module Vectorizable
    include Enumerable

    CLASS = {
      2 => Carbuncle::Point,
      3 => Carbuncle::Vector3,
      4 => Carbuncle::Vector4
    }.freeze

    def each(&block)
      to_a.each(&block)
    end

    def [](index)
      to_a[index]
    end

    def []=(index, value)
      result = to_a
      result[index] = value
      set(*result)
    end

    def length
      size
    end

    def to_s
      inspect
    end

    def +(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] += other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def -(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] -= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def *(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] *= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def /(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] /= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def %(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] %= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def **(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] **= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def <<(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] <<= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def >>(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] >>= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    def -@
      dup.tap do |vector|
        size.times { |i| vector[i] = -vector[i] }
      end
    end
  end
end
