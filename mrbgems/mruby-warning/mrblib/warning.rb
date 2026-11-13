##
# Warning
#
# Turns the categories of warnings on and off, and writes warnings.
# Kernel#warn (mruby-io) drops a warning in a category that is off, and
# hands the rest to Warning.warn, which a program may redefine to send
# them elsewhere.
#
module Warning
  # the categories, each on or off as CRuby starts
  @categories = {
    deprecated: false,
    experimental: true,
    performance: false,
    strict_unused_block: false,
  }

  ##
  # call-seq:
  #   Warning.categories  -> array
  #
  # The names of the categories.
  #
  #   Warning.categories  #=> [:deprecated, :experimental, :performance, :strict_unused_block]
  #
  def self.categories
    @categories.keys
  end

  ##
  # call-seq:
  #   Warning[category]  -> true or false
  #
  # Whether warnings in the category are written.
  #
  #   Warning[:deprecated]    #=> false
  #   Warning[:experimental]  #=> true
  #
  def self.[](category)
    @categories[__category(category)]
  end

  ##
  # call-seq:
  #   Warning[category] = flag  -> flag
  #
  # Turns warnings in the category on or off.
  #
  #   Warning[:deprecated] = true
  #
  def self.[]=(category, flag)
    @categories[__category(category)] = flag ? true : false
  end

  # the category, raising for one that is not a Symbol or not known
  def self.__category(category)
    unless category.is_a?(Symbol)
      raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
    end
    raise ArgumentError, "unknown category: #{category}" unless @categories.key?(category)
    category
  end

  ##
  # call-seq:
  #   Warning.warn(message, category: nil)  -> nil
  #
  # Writes the message to $stderr as it is, unless its category is off.
  # Kernel#warn calls this, so redefining it changes where warnings go.
  #
  #   Warning.warn("careful\n")                         # careful
  #   Warning.warn("old\n", category: :deprecated)      # (nothing)
  #
  def warn(message, category: nil)
    unless message.is_a?(String)
      raise TypeError, "wrong argument type #{message.class} (expected String)"
    end
    return nil unless category.nil? || Warning[category]
    $stderr.write(message)
    nil
  end

  extend self
end
