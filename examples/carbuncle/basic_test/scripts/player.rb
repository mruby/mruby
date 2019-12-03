class Player < Carbuncle::Sprite
  attr_reader :frame, :pose, :frame_width, :frame_height, :animation_step

  def initialize
    super(Carbuncle::Texture.new('graphics/characters/kurea.png'))
    @frame_width = texture.width / 4
    @frame_height = texture.height / 10
    @frame = 0
    @pose = 0
    @animation_time = 0
    @animation_step = (1.0 / 60.0) * 8
    pivot.set(0, 1)
    update_rect
  end

  def update(dt)
    update_input(dt)
    update_frame(dt)
  end

  def update_input(dt)
    dx, dy = 0, 0
    if Carbuncle::Keyboard.down?(:up)
      dy -= 40
    elsif Carbuncle::Keyboard.down?(:down)
      dy += 40
    end
    if Carbuncle::Keyboard.down?(:left)
      dx -= 40
    elsif Carbuncle::Keyboard.down?(:right)
      dx += 40
    end
    position.x += dx * dt
    position.y += dy * dt
  end

  def update_frame(dt)
    @animation_time += dt
    while @animation_time > animation_step
      @animation_time -= animation_step
      @frame = (@frame + 1) % 4
      update_rect
    end
  end

  def update_rect
    src_rect.set(frame * frame_width, pose * frame_height, frame_width, frame_height)
  end
end
