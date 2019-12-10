module Carbuncle
  class Vector4
    include Enumerable

    (2..4).each do |i|
      %w[x y z w].permutation(i) do |fields|
        define_method(fields.join) do
          fields.map { |field| send(field) }
        end

        define_method("#{fields.join}=") do |other|
          2.times do |index|
            send(:"#{fields[index]}=", other[index])
          end
        end
      end
    end

    def to_s
      inspect
    end

    def inspect
      "Vector4(#{x}, #{y}, #{z}, #{w})"
    end

    def size
      4
    end

    def each(&block)
      [x, y, z, w].each(&block)
    end
  end
end
