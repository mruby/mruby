module Carbuncle
  class ShadowText
    class Proxy
      def initialize(text)
        @text = text
      end

      def color
        @text.color
      end

      def color=(value)
        @text.color = value
      end
    end

    attr_accessor :offset

    def initialize
      @shadow_text = Carbuncle::Text.new
      @shadow_text.color.set(64, 64, 64)
      @foreground_text = Carbuncle::Text.new
      @foreground_text.font = @shadow_text.font
      @offset = Carbuncle::Point.new(2, 2)
    end

    def shadow
      @shadow ||= Carbuncle::ShadowText::Proxy.new(@shadow_text)
    end

    def foreground
      @foreground ||= Carbuncle::ShadowText::Proxy.new(@foreground_text)
    end

    def value
      @foreground_text.value
    end

    def font
      @foreground_text.font
    end

    def color
      @foreground_text.color
    end

    def position
      @foreground_text.position
    end

    def font=(value)
      @shadow_text.font = value
      @foreground_text.font = value
    end

    def color=(value)
      @foreground_text.color = value
    end

    def value=(value)
      @shadow_text.value = value
      @foreground_text.value = value
    end

    def position=(value)
      @foreground_text.position = value
      @shadow_text.position.set(offset.x + @foreground.x, offset.y + @foreground.y)
    end

    def size
      base_size = @foreground_text.size
      x = base_size.x + offset.x.abs
      y = base_size.y + offset.y.abs
      Carbuncle::Point.new(x, y)
    end

    def width
      size.x
    end

    def height
      size.y
    end

    def update(dt)
      @shadow_text.update(dt)
      @foreground_text.update(dt)
      @shadow_text.position.set(offset.x + @foreground_text.x, offset.y + @foreground_text.y)
    end

    def draw
      @shadow_text.draw
      @foreground_text.draw
    end
  end
end
