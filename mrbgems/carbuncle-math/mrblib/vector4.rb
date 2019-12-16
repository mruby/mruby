module Carbuncle
  module Vectorizable; end

  # Represents a point in fourth dimensional space.
  class Vector4
    include Vectorizable

    (2..4).each do |i|
      %w[x y z w].permutation(i) do |fields|
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
    # @!attribute xw [rw]
    #   @return [Carbuncle::Point]
    # @!attribute yx [rw]
    #   @return [Carbuncle::Point]
    # @!attribute yz [rw]
    #   @return [Carbuncle::Point]
    # @!attribute yw [rw]
    #   @return [Carbuncle::Point]
    # @!attribute zx [rw]
    #   @return [Carbuncle::Point]
    # @!attribute zy [rw]
    #   @return [Carbuncle::Point]
    # @!attribute zw [rw]
    #   @return [Carbuncle::Point]
    # @!attribute wx [rw]
    #   @return [Carbuncle::Point]
    # @!attribute wy [rw]
    #   @return [Carbuncle::Point]
    # @!attribute wz [rw]
    #   @return [Carbuncle::Point]
    # @!attribute xyz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xyw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yxz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yxw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yzx [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute yzw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute zxy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute zxw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xzw [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wxy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wyx [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wzx [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wxz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wyz [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute wzy [rw]
    #   @return [Carbuncle::Vector3]
    # @!attribute xyzw [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute xywz [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute xwzy [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute xzwy [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute yxzw [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute yzxw [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute ywzx [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute yxwz [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute zxyw [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute zyxw [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute zywx [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute zwyx [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute zwxy [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wxyz [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wxzy [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wyxz [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wyzx [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wzxy [rw]
    #   @return [Carbuncle::Vector4]
    # @!attribute wxzy [rw]
    #   @return [Carbuncle::Vector4]

    def inspect
      "Vector4(#{x}, #{y}, #{z}, #{w})"
    end

    def size
      4
    end

    def to_a
      [x, y, z, w]
    end
  end
end
