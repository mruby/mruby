module Carbuncle
  class Shader
    VALID_UNIFORM_CLASSES = [
      Integer, Float,
      Carbuncle::Point, Carbuncle::Vector3, Carbuncle::Vector4,
      Carbuncle::Rect, Carbuncle::Color, Carbuncle::Matrix,
      Carbuncle::Texture
    ].freeze

    def add_uniform(name, uniform_class, value = nil)
      symbol = name.to_sym
      variable = name.to_s.underscore
      @uniform_types[symbol]  = uniform_class
      @uniform_values[symbol] = value.presence || default_value_for(uniform_class)
      @uniform_locations[symbol] = find_uniform_location(name)
      if @uniform_locations[symbol] < 0
        raise ArgumentError, "Couldn't find location for uniform '#{name}'"
      end
      unless VALID_UNIFORM_CLASSES.include?(uniform_class)
        raise ArgumentError, "#{uniform_class.name} is not a valid uniform type."
      end

      define_singleton_method(variable) { @uniform_values[symbol] }
      define_singleton_method(:"#{variable}=") { |new_value| assign_uniform(symbol, new_value) }
    end

    def update(dt = 0)
      update_uniforms
    end

    def assign_uniform(name, value)
      symbol = name.to_sym
      klass = @uniform_types[symbol]
      unless @uniform_types.key?(symbol)
        raise ArgumentError, "Uniform '#{name}' does not exists on shader."
      end

      value = value.to_f if klass == Float
      value = value.to_i if klass == Integer
      unless value.is_a?(klass)
        raise ArgumentError, "Uniform '#{name}' should be of type #{klass.name}"
      end

      @uniform_values[symbol] = value
    end

    def update_uniforms
      @uniform_values.each do |symbol, value|
        send_uniform_value(@uniform_locations[symbol], value)
      end
    end

    def uniforms
      @uniform_values.dup
    end

    def uniforms=(values)
      values.each do |name, value|
        assign_uniform(name, value)
      end
    end

    def default_value_for(uniform_class)
      return 0 if uniform_class == Integer
      return 0.0 if uniform_class == Float

      uniform_class.new
    end
  end
end
