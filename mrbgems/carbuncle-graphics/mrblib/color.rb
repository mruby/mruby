class Carbuncle::Color
  include Carbuncle::Vectorizable

  (2..4).each do |i|
    %w[r g b a].permutation(i) do |fields|
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
    "Color(#{r}, #{g}, #{b}, #{a})"
  end

  def size
    4
  end

  def to_a
    [r, g, b, a]
  end
end
