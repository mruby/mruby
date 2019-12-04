class Carbuncle::Point
  %w[x y].permutation(2) do |fields|
    define_method(fields.join) do
      fields.map { |field| send(field) }
    end

    define_method("#{fields.join}=") do |other|
      2.times do |index|
        send(:"#{fields[index]}=", other[index])
      end
    end
  end

  def to_s
    inspect
  end

  def inspect
    "Point(#{x}, #{y})"
  end
end
