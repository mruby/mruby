module Carbuncle
  module Keyboard
    class << self
      # Checks if any of the keys are pressed.
      # @param [Array] args a list of keys.
      # @see press?
      def any_press?(*args)
        args.any?(&method(:press?))
      end

      # Checks if all of the keys are pressed.
      # @param [Array] args a list of keys.
      # @see press?
      def all_press?(*args)
        args.all?(&method(:press?))
      end

      # Checks if any of the keys are down.
      # @param [Array] args a list of keys.
      # @see down?
      def any_down?(*args)
        args.any?(&method(:down?))
      end

      # Checks if all of the keys are down.
      # @param [Array] args a list of keys.
      # @see down?
      def all_down?(*args)
        args.all?(&method(:down?))
      end

      # Checks if any of the keys are released.
      # @param [Array] args a list of keys.
      # @see release?
      def any_release?(*args)
        args.any?(&method(:release?))
      end

      # Checks if all of the keys are released.
      # @param [Array] args a list of keys.
      # @see release?
      def all_release?(*args)
        args.all?(&method(:release?))
      end

      # Checks if any of the keys are up.
      # @param [Array] args a list of keys.
      # @see up?
      def any_up?(*args)
        args.any?(&method(:up?))
      end

      # Checks if all of the keys are up.
      # @param [Array] args a list of keys.
      # @see up?
      def all_up?(*args)
        args.all?(&method(:up?))
      end

      # @!parse
      #   # Note that the numbers are suggestions, and dependant on your platform.
      #   # Don't use numbers directly, use the constant or the symbol when handling keyboard input.
      #   # This are in the documentation as a reference.
      #   # @example
      #   #   # Use this:
      #   #   Carbuncle::Keyboard.press?(:a) # => Cross platform!
      #   #   # Over this:
      #   #   Carbuncle::Keyboard.press?(65) # => Only works if A is mapped to the index 65!
      #   BACKSPACE = 0; SPACE = 1
      #   ESCAPE = 2; ESC = 3
      #   ENTER = 4; RETURN = 5
      #   DELETE = 6; DEL = 7
      #   RIGHT = 8; LEFT = 9; DOWN = 10; UP = 11
      #   ARROW_RIGHT = 8; ARROW_LEFT = 9; ARROW_DOWN = 10; ARROW_UP = 11
      #   RIGHT_ARROW = 8; LEFT_ARROW = 9; DOWN_ARROW = 10; UP_ARROW = 11
      #   F1 = 12; F2 = 13; F3 = 14; F4 = 15; F5 = 16; F6 = 17
      #   F7 = 18; F8 = 19; F9 = 20; F10 = 21; F11 = 22; F12 = 23
      #   LEFT_SHIFT = 24; LEFT_CONTROL = 25; LEFT_ALT = 26
      #   RIGHT_SHIFT = 27; RIGHT_CONTROL = 28; RIGHT_ALT = 29
      #   ZERO = 91; ONE = 92; TWO = 93; THREE = 94; FOUR = 95
      #   FIVE = 96; SIX = 97; SEVEN = 98; EIGHT = 99; NINE = 100
      #   A = 65; B = 66; C = 67; D = 68; E = 69; F = 70; G = 71; H = 72; I = 73; J = 74
      #   K = 75; L = 76; M = 77; N = 78; O = 79; P = 80; Q = 81; R = 82; S = 83; T = 84
      #   U = 85; V = 86; W = 87; X = 88; Y = 89; Z = 90
    end
  end
end
