module Carbuncle
  module Vectorizable; end

  class Rect
    include Vectorizable

    (2..4).each do |i|
      %w[x y w h].permutation(i) do |fields|
        define_method(fields.join) do
          Vectorizable::CLASS[i].new(*fields.map { |field| send(field) })
        end

        define_method("#{fields.join}=") do |other|
          i.times do |index|
            send(:"#{fields[index]}=", other[index])
          end
        end
      end
    end

    def inspect
      "Rect(#{x}, #{y}, #{w}, #{h})"
    end

    def size
      4
    end

    def to_a
      [x, y, w, h]
    end
  end
end
