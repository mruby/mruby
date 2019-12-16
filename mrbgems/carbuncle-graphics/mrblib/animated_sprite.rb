module Carbuncle
  # This class is an extension of a Sprite.
  # Represents an Sprite, but instead of being manually handled, you just select the rows and columns.
  # It also handles animations by name
  class AnimatedSprite
    # This class represents an animation
    Animation = Struct.new(:name, :frames, :delay)
    # This class handles the animation cicle for the AnimatedSprite class.
    class AnimationSet
      attr_reader :current

      # @param [AnimatedSprite] sprite
      def initialize(sprite)
        @sprite = sprite
        @time = 0
        @index = 0
        @current = nil
        @loops = -1
        @animations = {}
      end

      def update(dt)
        return unless running?

        current_animation = @animations[current]
        @time += dt
        while @time > current_animation.delay
          @time -= current_animation.delay
          @index = (@index + 1) % current_animation.frames.size
          @loops -= 1 if @index.zero? && @loops > 0
        end
      end

      def frame
        return Caruncle::Point.new if current.blank?

        @animations[@current].frames[@index]
      end

      def add(animation)
        @animations[animation.name] = animation
      end

      def <<(animation)
        add(animation)
      end

      def push(animation)
        add(animation)
      end

      def start(animation, loops = -1)
        @index = 0
        @time = 0
        @loops = loops
        @current = animation
      end

      def running?
        return false if @animations[current].blank?
        return true if @loops < 0

        @loops >= 0 && last_frame?
      end

      def last_frame?
        @index < (@animations[current].frames.size - 1)
      end
    end
    delegate :x, :x=, :y, :y=, :position, :position=, :opacity, :opacity=,
             :pivot, :pivot=, :scale, :scale=, :color, :color=, :texture,
             to: :sprite

    attr_reader :rows
    attr_reader :columns
    attr_reader :sprite
    attr_reader :animations

    def initialize(texture = nil)
      @sprite = Carbuncle::Sprite.new(texture)
      self.rows = 1
      self.columns = 1
      @frame = Carbuncle::Point.new
      @animations = Carbuncle::AnimatedSprite::AnimationSet.new
    end

    def rows=(value)
      raise ArgumentError, 'Value is not a number' unless value.is_a?(Numeric)
      raise ArgumentError, 'Value must be positive' if value < 1

      @rows = value
      @frame_width = texture.present? ? texture.width / rows : 1
    end

    def columns=(value)
      raise ArgumentError, 'Value is not a number' unless value.is_a?(Numeric)
      raise ArgumentError, 'Value must be positive' if value < 1

      @columns = value
      @frame_height = texture.present? ? texture.height / columns : 1
    end

    def update(dt)
      @sprite.update(dt)
      update_animation(dt)
      update_rect
    end

    def draw
      @sprite.draw
    end

    def update_animation(dt)
      animations.update(dt)

      @frame = animations.frame
    end

    def update_rect
      return unless texture.present?

      @sprite.src_rect.set(
        @frame.x * @frame_width,
        @frame.y * @frame_height,
        @frame_width,
        @frame_height
      )
    end

    def width
      @frame_width
    end

    def height
      @frame_height
    end

    def texture=(value)
      @sprite.texture = value
      @frame_width = texture.present? ? texture.width / rows : 1
      @frame_height = texture.present? ? texture.height / columns : 1
    end
  end
end
