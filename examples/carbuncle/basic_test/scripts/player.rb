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
    pivot.set(0.5, 1)
    @attacking = false
    update_rect
  end

  def update(dt)
    update_input(dt)
    update_frame(dt)
  end

  def update_input(dt)
    return if attacking?

    dx = 0
    if Carbuncle::Keyboard.press?(:a)
      attack!
    elsif Carbuncle::Keyboard.down?(:left)
      dx -= 80
      scale.x = 1
      @frame = 0 if @pose != 4
      @pose = 4
    elsif Carbuncle::Keyboard.down?(:right)
      dx += 80
      @frame = 0 if @pose != 5
      @pose = 5
    else
      @frame = 0 if @pose != 0
      @pose = 0
    end
    position.x += dx * dt
  end

  def update_frame(dt)
    @animation_time += dt
    while @animation_time > animation_step
      @animation_time -= animation_step
      @frame = (@frame + 1) % 4
      if attacking? && @frame == 0
        @attacking = false
        @frame = 0
      end
      update_rect
    end
  end

  def update_rect
    src_rect.set(frame * frame_width, pose * frame_height, frame_width, frame_height)
  end

  def attacking?
    @attacking
  end

  def attack!
    @attacking = true
    @pose = rand > 0.5 ? 6 : 7
    @frame = 0
  end
end
