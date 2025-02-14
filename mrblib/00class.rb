class BasicObject
  def !=(other)
    if self == other
      false
    else
      true
    end
  end
end

class Module
  # 15.2.2.4.11
  alias attr attr_reader

  # 15.2.2.4.27
  def include(*args)
    args.reverse!
    mod = self
    args.each do |m|
      m.__send__(:append_features, mod)
      m.__send__(:included, mod)
    end
    self
  end

  def prepend(*args)
    args.reverse!
    mod = self
    args.each do |m|
      m.__send__(:prepend_features, mod)
      m.__send__(:prepended, mod)
    end
    self
  end
end
