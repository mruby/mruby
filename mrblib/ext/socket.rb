if Object.const_defined? :Socket
  module Socket
    class Option
      def initialize(family, level, optname, data)
        @family  = family
        @level   = level
        @optname = optname
        @data    = data
      end

      def self.bool(family, level, optname, bool)
        self.new(family, level, optname, [(bool ? 1 : 0)].pack('i'))
      end

      def self.int(family, level, optname, integer)
        self.new(family, level, optname, [integer].pack('i'))
      end

      #def self.linger(family, level, optname, integer)
      #end

      attr_reader :data, :family, :level, :optname

      def bool
        @data.unpack('i')[0] != 0
      end

      def inspect
        # notyet
      end

      def int
        @data.unpack('i')[0]
      end

      def linger
        raise NotImplementedError.new
      end

      def unpack(template)
        raise NotImplementedError.new
      end
    end
  end

  class SocketError < StandardError; end
end
