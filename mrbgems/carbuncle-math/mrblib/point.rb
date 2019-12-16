module Carbuncle
  module Vectorizable; end

  # @note The point allows vectorial operations, with the values of x and y.
  class Point
    include Vectorizable

    %w[x y].permutation(2) do |fields|
      define_method(fields.join) do
        Vectorizable::CLASS[2].new(*fields.map { |field| send(field) })
      end

      define_method("#{fields.join}=") do |other|
        2.times do |index|
          send(:"#{fields[index]}=", other[index])
        end
      end
    end

    # @!attribute xy [rw]
    #   A vectorial operation to extract [x, y] from the point or to assign it
    #   @return [Carbuncle::Point]
    # @!attribute yx [rw]
    #   A vectorial operation to extract [y, x] from the point or to assign it
    #   @return [Carbuncle::Point]
    #   @example vectorial operations
    #    # swap a value with another:
    #    point.xy = point.yx # swaps x and y
    #    # the same as:
    #    point.x, point.y = point.y, point.x

    def inspect
      "Point(#{x}, #{y})"
    end

    def size
      2
    end

    def to_a
      [x, y]
    end
  end
end
