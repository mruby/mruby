module Carbuncle
  module Mouse
    # @!parse
    #   # Use the symbols, like Mouse.press?(:left) instead of this ones, this are only put here as reference.
    #   LEFT   = 1
    #   RIGHT  = 2
    #   MIDDLE = 3
    #   LEFT_BUTTON = 1
    #   RIGHT_BUTTON = 2
    #   MIDDLE_BUTTON = 3
    class << self
      # @!attribute [rw] position
      #   The position for the mouse.
      #   @return [Carbuncle::Point]
      # @!attribute [rw] x
      #   The x coordinate for the mouse.
      #   @return [Integer]
      # @!attribute [rw] y
      #   The y coordinate for the mouse.
      #   @return [Integer]
      # @!attribute [r] wheel_move
      #   The movement for the mouse wheel.
      #   @return [Integer]
      # @!attribute [r] wheel
      #   The movement for the mouse wheel.
      #   @return [Integer]
    end
  end
end
