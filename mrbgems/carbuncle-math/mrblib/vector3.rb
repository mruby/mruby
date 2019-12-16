module Carbuncle
  module Vectorizable; end

  class Vector3
    include Vectorizable

    (2..3).each do |i|
      %w[x y z].permutation(i) do |fields|
        define_method(fields.join) do
          Vectorizable::CLASS[i].new(*fields.map { |field| send(field) })
        end

        define_method("#{fields.join}=") do |other|
          2.times do |index|
            send(:"#{fields[index]}=", other[index])
          end
        end
      end
    end

    # @!attribute xy [rw]
    #   @return [Array]
    # @!attribute xz [rw]
    #   @return [Array]
    # @!attribute yx [rw]
    #   @return [Array]
    # @!attribute yz [rw]
    #   @return [Array]
    # @!attribute zx [rw]
    #   @return [Array]
    # @!attribute zy [rw]
    #   @return [Array]
    # @!attribute xyz [rw]
    #   @return [Array]
    # @!attribute xzy [rw]
    #   @return [Array]
    # @!attribute yxz [rw]
    #   @return [Array]
    # @!attribute yzx [rw]
    #   @return [Array]
    # @!attribute zxy [rw]
    #   @return [Array]
    # @!attribute xzy [rw]
    #   @return [Array]

    def inspect
      "Vector3(#{x}, #{y}, #{z})"
    end

    def size
      3
    end

    def to_a
      [x, y, z]
    end
  end
end
