# The encoding errors CRuby raises, with what they carry. String#encode
# raises InvalidByteSequenceError and UndefinedConversionError for the bytes
# it cannot convert, and ConverterNotFoundError for an encoding it does not
# convert; CompatibilityError is for code that names it.
class EncodingError < StandardError
end

module Encoding
  class CompatibilityError < EncodingError
  end

  class ConverterNotFoundError < EncodingError
  end

  class InvalidByteSequenceError < EncodingError
    attr_reader :error_bytes, :source_encoding_name, :destination_encoding_name

    def source_encoding
      @source_encoding_name
    end

    def destination_encoding
      @destination_encoding_name
    end

    def incomplete_input?
      @incomplete_input
    end
  end

  class UndefinedConversionError < EncodingError
    attr_reader :error_char, :source_encoding_name, :destination_encoding_name

    def source_encoding
      @source_encoding_name
    end

    def destination_encoding
      @destination_encoding_name
    end
  end
end
