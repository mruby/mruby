module Carbuncle
  module Keyboard
    class << self
      def any_press?(*args)
        args.any?(&method(:press?))
      end

      def all_press?(*args)
        args.all?(&method(:press?))
      end

      def any_down?(*args)
        args.any?(&method(:down?))
      end

      def all_down?(*args)
        args.all?(&method(:down?))
      end

      def any_release?(*args)
        args.any?(&method(:release?))
      end

      def all_release?(*args)
        args.all?(&method(:release?))
      end

      def any_up?(*args)
        args.any?(&method(:up?))
      end

      def all_up?(*args)
        args.all?(&method(:up?))
      end
    end
  end
end
