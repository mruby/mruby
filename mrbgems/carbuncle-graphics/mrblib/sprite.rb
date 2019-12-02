class Carbuncle::Sprite
  delegate :x, :y, :x=, :y=, to: :position
  delegate :x, :y, :x=, :y=, to: :scale, prefix: true
  delegate :x, :y, :x=, :y=, to: :pivot, prefix: true

  def opacity
    color.alpha
  end

  def opacity=(value)
    color.alpha = value
  end
end
