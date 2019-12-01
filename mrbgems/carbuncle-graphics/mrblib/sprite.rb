class Carbuncle::Sprite
  def x
    position.x
  end

  def x=(value)
    position.x = value
  end

  def y
    position.y
  end

  def y=(value)
    position.y = value
  end

  def opacity
    color.alpha
  end

  def opacity=(value)
    color.alpha = value
  end
end
