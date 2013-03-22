class Array
  def uniq!
    ary = self.dup
    result = []
    while ary.size > 0
      result << ary.shift
      ary.delete(result.last)
    end
    self.replace(result)
  end

  def uniq
    self.dup.uniq!
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

    (self + elem).uniq!
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

  def flatten!
    self.replace(self.flatten)
  end

  def compact
    result = self.dup
    result.compact!
    result
  end

  def compact!
    result = self.select { |e| e != nil }
    self.replace(result)
  end
end
