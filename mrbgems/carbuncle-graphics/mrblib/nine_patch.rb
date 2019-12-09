module Carbuncle
  class NinePatch
    delegate :x, :y, :x=, :y=, to: :position
  end
end
