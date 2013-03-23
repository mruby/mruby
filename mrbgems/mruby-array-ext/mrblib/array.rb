class Array
  def uniq!
    ary = self.dup
    result = []
    while ary.size > 0
      result << ary.shift
      ary.delete(result.last)
    end
    if result.size == self.size
      nil
    else
      self.replace(result)
    end
  end

  def uniq
    ary = self.dup
    ary.uniq!
    ary
  end

  def -(elem)
    raise TypeError, "can't convert to Array" unless elem.class == Array

    hash = {}
    array = []
    elem.each { |x| hash[x] = true }
    self.each { |x| array << x unless hash[x] }
    array
  end

  def |(elem)
    raise TypeError, "can't convert to Array" unless elem.class == Array

    ary = self + elem
    ary.uniq! or ary
  end

  def &(elem)
    raise TypeError, "can't convert to Array" unless elem.class == Array

    hash = {}
    array = []
    elem.each{|v| hash[v] = true }
    self.each do |v|
      if hash[v]
        array << v
        hash.delete v
      end
    end
    array
  end

  def flatten(depth=nil)
    ar = []
    self.each do |e|
      if e.is_a?(Array) && (depth.nil? || depth > 0)
        ar += e.flatten(depth.nil? ? nil : depth - 1)
      else
        ar << e
      end
    end
    ar
  end

  def flatten!(depth=nil)
    modified = false
    ar = []
    self.each do |e|
      if e.is_a?(Array) && (depth.nil? || depth > 0)
        ar += e.flatten(depth.nil? ? nil : depth - 1)
        modified = true
      else
        ar << e
      end
    end
    if modified
      self.replace(ar)
    else
      nil
    end
  end

  def compact
    result = self.dup
    result.compact!
    result
  end

  def compact!
    result = self.select { |e| e != nil }
    if result.size == self.size
      nil
    else
      self.replace(result)
    end
  end
end
