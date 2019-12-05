module Carbuncle
  class Text
    delegate :x, :y, :x=, :y=, to: :position

    def update(dt); end

    def size
      font.measure_text(value)
    end

    def width
      size.x
    end

    def height
      size.y
    end
  end
end
