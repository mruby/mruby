module Carbuncle
  class Vector3
    include Enumerable

    (2..3).each do |i|
      %w[x y z].permutation(i) do |fields|
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
      "Vector3(#{x}, #{y}, #{z})"
    end

    def size
      3
    end

    def each(&block)
      [x, y, z].each(&block)
    end
  end
end
