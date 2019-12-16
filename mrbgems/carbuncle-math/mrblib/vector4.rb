module Carbuncle
  module Vectorizable; end

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
    #   @return [Array]
    # @!attribute xz [rw]
    #   @return [Array]
    # @!attribute xw [rw]
    #   @return [Array]
    # @!attribute yx [rw]
    #   @return [Array]
    # @!attribute yz [rw]
    #   @return [Array]
    # @!attribute yw [rw]
    #   @return [Array]
    # @!attribute zx [rw]
    #   @return [Array]
    # @!attribute zy [rw]
    #   @return [Array]
    # @!attribute zw [rw]
    #   @return [Array]
    # @!attribute wx [rw]
    #   @return [Array]
    # @!attribute wy [rw]
    #   @return [Array]
    # @!attribute wz [rw]
    #   @return [Array]
    # @!attribute xyz [rw]
    #   @return [Array]
    # @!attribute xyw [rw]
    #   @return [Array]
    # @!attribute xzy [rw]
    #   @return [Array]
    # @!attribute xzw [rw]
    #   @return [Array]
    # @!attribute yxz [rw]
    #   @return [Array]
    # @!attribute yxw [rw]
    #   @return [Array]
    # @!attribute yzx [rw]
    #   @return [Array]
    # @!attribute yzw [rw]
    #   @return [Array]
    # @!attribute zxy [rw]
    #   @return [Array]
    # @!attribute zxw [rw]
    #   @return [Array]
    # @!attribute xzy [rw]
    #   @return [Array]
    # @!attribute xzw [rw]
    #   @return [Array]
    # @!attribute wxy [rw]
    #   @return [Array]
    # @!attribute wyx [rw]
    #   @return [Array]
    # @!attribute wzx [rw]
    #   @return [Array]
    # @!attribute wxz [rw]
    #   @return [Array]
    # @!attribute wyz [rw]
    #   @return [Array]
    # @!attribute wzy [rw]
    #   @return [Array]
    # @!attribute xyzw [rw]
    #   @return [Array]
    # @!attribute xywz [rw]
    #   @return [Array]
    # @!attribute xwzy [rw]
    #   @return [Array]
    # @!attribute xzwy [rw]
    #   @return [Array]
    # @!attribute yxzw [rw]
    #   @return [Array]
    # @!attribute yzxw [rw]
    #   @return [Array]
    # @!attribute ywzx [rw]
    #   @return [Array]
    # @!attribute yxwz [rw]
    #   @return [Array]
    # @!attribute zxyw [rw]
    #   @return [Array]
    # @!attribute zyxw [rw]
    #   @return [Array]
    # @!attribute zywx [rw]
    #   @return [Array]
    # @!attribute zwyx [rw]
    #   @return [Array]
    # @!attribute zwxy [rw]
    #   @return [Array]
    # @!attribute wxyz [rw]
    #   @return [Array]
    # @!attribute wxzy [rw]
    #   @return [Array]
    # @!attribute wyxz [rw]
    #   @return [Array]
    # @!attribute wyzx [rw]
    #   @return [Array]
    # @!attribute wzxy [rw]
    #   @return [Array]
    # @!attribute wxzy [rw]
    #   @return [Array]

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
