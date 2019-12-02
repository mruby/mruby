class Carbuncle::Sprite
  delegate :x, :y, :x=, :y=, to: :position

  def opacity
    color.alpha
  end

  def opacity=(value)
    color.alpha = value
  end
end
