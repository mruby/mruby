module Carbuncle
  class Text
    attr_reader :value
    delegate :x, :y, :x=, :y=, to: :position

    def update(dt); end

    def value=(value)
      @value = value&.to_s || ''
    end

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
