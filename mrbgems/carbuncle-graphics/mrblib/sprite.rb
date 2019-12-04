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

    def z=(value)
      @z = value.to_f
    end
  end
end
