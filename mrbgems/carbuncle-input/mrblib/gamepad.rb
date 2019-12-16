module Carbuncle
  class Gamepad
    # Checks if the controller has the XBox™ layout
    def xbox?
      name.present? && (name =~ /Microsoft/i || name =~ /XBox/i)
    end

    # Checks if the controller has the PlayStation™ layout
    def ps?
      name.present? && (name =~ /PlayStation/i || name =~ /Sony/i)
    end

    # Checks if the controller has the PlayStation™ layout
    def play_station?
      ps?
    end

    # Checks if the controller has the PlayStation™ layout
    def playstation?
      ps?
    end

    # Checks if the controller has the Nintendo™ layout
    def nintendo?
      name =~ /Nintendo/i
    end

    # Returns the detected controller layout.
    # @return [Symbol]
    def layout
      return :xbox if xbox?
      return :nintendo if nintendo?
      return :play_station if ps?

      :unknown
    end

    # @!parse
    #   DPAD_UP = 1
    #   DPAD_RIGHT = 2
    #   DPAD_DOWN = 3
    #   DPAD_LEFT = 4
    #   LEFT_UP = 1
    #   LEFT_RIGHT = 2
    #   LEFT_DOWN = 3
    #   LEFT_FACE_LEFT = 4
    #   LEFT_FACE_UP = 1
    #   LEFT_FACE_RIGHT = 2
    #   LEFT_FACE_DOWN = 3
    #   LEFT_FACE_LEFT = 4
    #   UP = 1
    #   RIGHT = 2
    #   DOWN = 3
    #   LEFT = 4
    #   RIGHT_UP = 5
    #   RIGHT_RIGHT = 6
    #   RIGHT_DOWN = 7
    #   RIGHT_LEFT = 8
    #   RIGHT_FACE_UP = 5
    #   RIGHT_FACE_RIGHT = 6
    #   RIGHT_FACE_DOWN = 7
    #   RIGHT_FACE_LEFT = 8
    #   L1 = 9
    #   L2 = 10
    #   LEFT1 = 9
    #   LEFT2 = 10
    #   LB = 9
    #   LT = 10
    #   LEFT_BUTTON = 9
    #   LEFT_TRIGGER = 10
    #   ZL = 10
    #   R1 = 11
    #   R2 = 12
    #   RIGHT1 = 11
    #   RIGHT2 = 12
    #   RB = 11
    #   RT = 12
    #   RIGHT_BUTTON = 11
    #   RIGHT_TRIGGER = 12
    #   ZR = 12
    #   MIDDLE_LEFT = 13
    #   SELECT = 13
    #   MIDDLE = 14
    #   HOME = 14
    #   MIDDLE_RIGHT = 15
    #   START = 15
    #   LEFT_THUMB = 16
    #   L3 = 16
    #   RIGHT_THUMB = 17
    #   R3 = 17
  end
end
