module Carbuncle
  class Plane
    delegate :width, :height, :w, :h, to: :src_rect

    def update(dt); end

    def opacity
      color.alpha
    end

    def opacity=(value)
      color.alpha = value
    end

    def ox
      origin.x
    end

    def oy
      origin.y
    end

    def ox=(value)
      origin.x = value
    end

    def oy=(value)
      origin.y = value
    end
  end
end
