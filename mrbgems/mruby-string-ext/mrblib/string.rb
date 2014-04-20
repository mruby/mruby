class String
  def lstrip
    a = 0
    z = self.size - 1
    a += 1 while " \f\n\r\t\v".include?(self[a]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def rstrip
    a = 0
    z = self.size - 1
    z -= 1 while " \f\n\r\t\v\0".include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def strip
    a = 0
    z = self.size - 1
    a += 1 while " \f\n\r\t\v".include?(self[a]) and a <= z
    z -= 1 while " \f\n\r\t\v\0".include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def lstrip!
    s = self.lstrip
    (s == self) ? nil : self.replace(s)
  end

  def rstrip!
    s = self.rstrip
    (s == self) ? nil : self.replace(s)
  end

  def strip!
    s = self.strip
    (s == self) ? nil : self.replace(s)
  end

# call-seq:
#    str.casecmp(other_str)   -> -1, 0, +1 or nil
#
# Case-insensitive version of <code>String#<=></code>.
#
#    "abcdef".casecmp("abcde")     #=> 1
#    "aBcDeF".casecmp("abcdef")    #=> 0
#    "abcdef".casecmp("abcdefg")   #=> -1
#    "abcdef".casecmp("ABCDEF")    #=> 0
#
  def casecmp(str)
    self.downcase <=> str.to_str.downcase
  rescue NoMethodError
    raise TypeError, "no implicit conversion of #{str.class} into String"
  end

  def partition(sep)
    raise TypeError, "type mismatch: #{sep.class} given" unless sep.is_a? String
    n = index(sep)
    unless n.nil?
      m = n + sep.size
      [ slice(0, n), sep, slice(m, size - m) ]
    else
      [ self, "", "" ]
    end
  end

  def rpartition(sep)
    raise TypeError, "type mismatch: #{sep.class} given" unless sep.is_a? String
    n = rindex(sep)
    unless n.nil?
      m = n + sep.size
      [ slice(0, n), sep, slice(m, size - m) ]
    else
      [ "", "", self ]
    end
  end
end
