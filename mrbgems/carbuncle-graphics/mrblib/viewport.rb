module Carbuncle
  class Viewport < Carbuncle::Container
    delegate :set, :x, :y, :width, :height, :w, :h, :x=, :y=, :width=, :height=, :w=, :h=, to: :rect
  end
end
