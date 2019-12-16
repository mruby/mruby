module Carbuncle
  module Gesture
    class << self
      %w[tap double_tap hold drag swipe_right swipe_left swipe_up swipe_down pinch_in pinch_out].each do |gesture|
        define_method :"#{gesture}?" do
          detect?(gesture)
        end
      end

      # @!method tap?
      #   Checks if a tap gesture is being done.
      #   A tap is a quick touch and lif on your screen.
      #   @return [Boolean]
      # @!method double_tap?
      #   Checks if a double tap gesture is being done.
      #   A tap is a quick touch and lif on your screen.
      #   @return [Boolean]
      #   @see tap?
      # @!method hold?
      #   Checks if a hold gesture is being done.
      #   A hold action is when the screen is being touched without lifting from some time.
      #   @return [Boolean]
      # @!method drag?
      #   Checks if a drag gesture is being done.
      #   A drag actions is when the user holds and then moves the finger on the screen.
      #   @return [Boolean]
      # @!method swipe_left?
      #   Checks if a swipe gesture to the left is being done.
      #   A swipe is when the user moves the finger quickly across a direction into the screen.
      #   @return [Boolean]
      # @!method swipe_right?
      #   Checks if a swipe gesture to the right is being done.
      #   A swipe is when the user moves the finger quickly across a direction into the screen.
      #   @return [Boolean]
      # @!method swipe_up?
      #   Checks if a swipe gesture to the up is being done.
      #   A swipe is when the user moves the finger quickly across a direction into the screen.
      #   @return [Boolean]
      # @!method swipe_down?
      #   Checks if a swipe gesture to the down is being done.
      #   A swipe is when the user moves the finger quickly across a direction into the screen.
      #   @return [Boolean]

      # Checks if any pinch gesture is being done
      def pinch?
        pinch_in? || pinch_out?
      end

      # Checks if a swipe on any direction is done.
      # A swipe is when the user moves the finger quickly across a direction into the screen.
      def swipe?
        swipe_right? || swipe_left? || swipe_up? || swipe_down?
      end
    end

    # All touch events are handled using this base class
    class Event
      def to_s
        inspect
      end
    end

    # Handles the contents of the Hold gesture
    class Hold < Carbuncle::Gesture::Event
      # The amount of second the user is holding
      # @return [Float]
      attr_reader :duration

      def initialize(duration)
        @duration = duration
      end

      def inspect
        "Hold(#{duration})"
      end
    end

    # Handles the dinformation about the drag event
    class Drag < Carbuncle::Gesture::Event
      # The Vector of the touch
      # @return [Point]
      attr_reader :vector
      # The angle of the Drag
      # @return [Float]
      attr_reader :angle

      def initialize(vector, angle)
        @vector = vector
        @angle = angle
      end

      def inspect
        "Drag(#{vector}, #{angle})"
      end
    end

    # Has the information of the pinch gesture
    class Pinch < Carbuncle::Gesture::Event
      # The pinch vector
      # @return [Point]
      attr_reader :vector
      # The pinch's angle
      # @return [Float]
      attr_reader :angle

      def initialize(vector, angle)
        @vector = vector
        @angle = angle
      end

      def inspect
        "Pinch(#{vector}, #{angle})"
      end
    end
  end
end
