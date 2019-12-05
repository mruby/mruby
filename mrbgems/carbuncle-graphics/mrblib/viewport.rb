module Carbuncle
  class Viewport < Carbuncle::Container
    delegate :set, to: :rect
  end
end
