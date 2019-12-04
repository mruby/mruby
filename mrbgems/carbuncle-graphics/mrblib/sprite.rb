module Carbuncle
  class Sprite
    delegate :x, :y, :x=, :y=, to: :position
    delegate :x, :y, :x=, :y=, to: :scale, prefix: true
    delegate :x, :y, :x=, :y=, to: :pivot, prefix: true

    def opacity
      color.alpha
    end

    def opacity=(value)
      color.alpha = value
    end

    def ox
      pivot.x * width
    end

    def oy
      pivot.y * height
    end

    def ox=(value)
      pivot.x = value.to_f / width
    end

    def oy=(value)
      pivot.y = value.to_f / height
    end
  end
end
