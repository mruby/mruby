module Carbuncle
  class Font
    class << self
      attr_reader :default_name, :default_size

      def default_name=(value)
        @default_name = value&.to_s
      end

      def default_size=(value)
        @default_size = value.to_i
      end
    end

    self.default_name = nil
    self.default_size = 32
  end
end
