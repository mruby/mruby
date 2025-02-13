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
  # 15.2.2.4.27
  def include(*args)
    args.reverse!
    args.each do |m|
      m.append_features(self)
      m.included(self)
    end
    self
  end

  def prepend(*args)
    args.reverse!
    args.each do |m|
      m.prepend_features(self)
      m.prepended(self)
    end
    self
  end
end
