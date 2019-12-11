module Carbuncle
  class Gamepad
    def xbox?
      name.present? && (name =~ /Microsoft/i || name =~/XBox/i)
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
  end
end
