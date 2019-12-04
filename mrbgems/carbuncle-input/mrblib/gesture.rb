module Carbuncle
  module Gesture
    class << self
      %w[tap double_tap hold drag swipe_right swipe_left swipe_up swipe_down pinch_in pinch_out].each do |gesture|
        define_method :"#{gesture}?" do
          detect?(gesture)
        end
      end

      def pinch?
        pinch_in? || pinch_out?
      end

      def swipe?
        swipe_right? || swipe_left? || swipe_up? || swipe_down?
      end
    end

    class Event
      def to_s
        inspect
      end
    end

    class Hold < Carbuncle::Gesture::Event
      attr_reader :duration

      def initialize(duration)
        @duration = duration
      end

      def inspect
        "Hold(#{duration})"
      end
    end

    class Drag < Carbuncle::Gesture::Event
      attr_reader :vector, :angle

      def initialize(vector, angle)
        @vector = vector
        @angle = angle
      end

      def inspect
        "Drag(#{vector}, #{angle})"
      end
    end

    class Pinch < Carbuncle::Gesture::Event
      attr_reader :vector, :angle

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
