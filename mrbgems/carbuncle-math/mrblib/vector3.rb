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
    #   @return [Carbuncle::Point]
    # @!attribute xz [rw]
    #   @return [Carbuncle::Point]
    # @!attribute yx [rw]
    #   @return [Carbuncle::Point]
    # @!attribute yz [rw]
    #   @return [Carbuncle::Point]
    # @!attribute zx [rw]
    #   @return [Carbuncle::Point]
    # @!attribute zy [rw]
    #   @return [Carbuncle::Point]
    # @!attribute xyz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yxz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yzx [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute zxy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzy [rw]
    #   @return [Carbuncle::Vector3]

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
