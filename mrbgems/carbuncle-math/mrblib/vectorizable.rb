module Carbuncle
  class Point; end
  class Vector3; end
  class Vector4; end

  # Represent a vectorizable element, it's any element than allows vector operations.
  # All vectorizable elements are also Enumerable by default.
  module Vectorizable
    include Enumerable

    # Contains which class is a vectorizable of n-elements
    # It is used for vector operations across classes.
    # While Carbuncle::Color and Carbuncle::Rect are both Vectorizable objects of size 4, Vector4 is the more generic one.
    CLASS = {
      2 => Carbuncle::Point,
      3 => Carbuncle::Vector3,
      4 => Carbuncle::Vector4
    }.freeze

    # Iterates over each element on the vectorizable.
    # If no block is given an iterator will be returned.
    # @param [Proc] block A block to be called on each element of the array
    def each(&block)
      to_a.each(&block)
    end

    # Returns the n-th element of a vectorizable object
    # @param [Integer] index The n-th position to set
    # @return [Numeric]
    def [](index)
      to_a[index]
    end

    # Sets the n-th element of the devcotirzable object
    # @param [Integer] index The n-th position to set
    # @param [Numeric] the new value of the n-th position.
    # @return [self]
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

    # @!group Vector Operations

    # @return [Vectorizable]
    # @overload +(number)
    #   Iterates over this vector's values to create a new one with the addition of each value and this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    #   @example
    #     Point.new(1, 2) + 2 # => Point(3, 4)
    # @overload +(vector)
    #   iterates over itself and the vector,, returning a new vector with the added values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to add.
    #   @example
    #     Point.new(1, 2) + Point.new(3, 4) # => Point(4, 5)
    #   @return [Vectorizable]
    def +(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] += other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload -(number)
    #   Iterates over this vector's values to create a new one with the substraction of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload -(vector)
    #   iterates over itself and the vector,, returning a new vector with the substracted values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def -(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] -= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload *(number)
    #   Iterates over this vector's values to create a new one with the multiplication of each value and this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload *(vector)
    #   iterates over itself and the vector, returning a new vector with the multiplied values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def *(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] *= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload /(number)
    #   Iterates over this vector's values to create a new one with the division of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload /(vector)
    #   Iterates over itself and the vector, returning a new vector with the divided values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def /(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] /= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload %(number)
    #   Iterates over this vector's values to create a new one with the modulo of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload %(vector)
    #   Iterates over itself and the vector, returning a new vector with the multiplied values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def %(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] %= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload **(number)
    #   Iterates over this vector's values to create a new one with the power of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload **(vector)
    #   Iterates over itself and the vector, returning a new vector with the power between values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def **(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] **= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload <<(number)
    #   Iterates over this vector's values to create a new one with the left shift of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload <<(vector)
    #   Iterates over itself and the vector, returning a new vector with the shifted values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def <<(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] <<= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # @overload >>(number)
    #   Iterates over this vector's values to create a new one with the right shift of each value by this number.
    #   @param [Numeric] number
    #   @return [Vectorizable]
    # @overload >>(vector)
    #   Iterates over itself and the vector, returning a new vector with the shifted values.
    #   @param [Vectorizable, Array] vector Anothe vector or array to substract.
    #   @return [Vectorizable]
    def >>(other)
      dup.tap do |vector|
        size.times do |i|
          vector[i] >>= other.is_a?(Numeric) ? other : other[i]
        end
      end
    end

    # @return [Vectorizable]
    # Returns a new vector, with each of it's values negated.
    # @example
    #   -Point.new(1, 2) => Point(-1, -2)
    # @return [Vectorizable]
    def -@
      dup.tap do |vector|
        size.times { |i| vector[i] = -vector[i] }
      end
    end
  end
end
