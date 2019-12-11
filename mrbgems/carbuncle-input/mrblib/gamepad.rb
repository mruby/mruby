module Carbuncle
  class Gamepad
    def xbox?
      name.present? && (name =~ /Microsoft/i || name =~ /XBox/i)
    end

    def ps?
      name.present? && (name =~ /PlayStation/i || name =~ /Sony/i)
    end

    def play_station?
      ps?
    end

    def playstation?
      ps?
    end

    def nintendo?
      name =~ /Nintendo/i
    end

    def layout
      return :xbox if xbox?
      return :nintendo if nintendo?
      return :play_station if ps?

      :unknown
    end
  end
end
