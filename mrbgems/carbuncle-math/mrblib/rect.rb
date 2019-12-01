class Carbuncle::Rect
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
end
