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
  end
end
