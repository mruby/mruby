module Carbuncle
  class Shader
    def define_uniform(name, type)
      symbol = name.to_s.to_sym
      @uniform_types ||= {}
      @uniform_values ||= {}
      raise ArgumentError, "'#{name}' is already defined as attribute." if @uniforms[symbol].present?

      @uniform_types[symbol] = type
      @uniform_values[symbol] = type.new

      invalid_type = "Invalid type for uniform '#{name}', expected '#{type.name}', but #{value.class.name} was set."

      define_method symbol do
        @uniform_values[symbol]
      end

      define_method :"#{symbol}=" do |value|
        raise ArgumentError, invalid_type unless value.is_a?(type)

        @uniform_values[symbol] = type
      end
    end
  end
end
