module Carbuncle
  module Gesture
    class << self
      %w[tap double_tap hold drag swipe_right swipe_left swipe_up swipe_down].each do |gesture|
        define_method :"#{gesture}?" do
          detect?(gesture)
        end
      end
    end
  end
end
