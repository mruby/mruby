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
end
