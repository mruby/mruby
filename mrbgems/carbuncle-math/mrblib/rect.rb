module Carbuncle
  class Rect
    include Enumerable

    (2..4).each do |i|
      %w[x y w h].permutation(i) do |fields|
        define_method(fields.join) do
          fields.map { |field| send(field) }
        end

        define_method("#{fields.join}=") do |other|
          i.times do |index|
            send(:"#{fields[index]}=", other[index])
          end
        end
      end
    end

    def to_s
      inspect
    end

    def inspect
      "Rect(#{x}, #{y}, #{w}, #{h})"
    end

    def size
      4
    end

    def each(&block)
      [x, y, w, h].each(&block)
    end
  end
end
