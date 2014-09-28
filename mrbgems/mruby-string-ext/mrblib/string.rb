class String

  ##
  # call-seq:
  #    string.clear    ->  string
  #
  # Makes string empty.
  #
  #    a = "abcde"
  #    a.clear    #=> ""
  #
  def clear
    self.replace("")
  end

  ##
  # call-seq:
  #    str.lstrip   -> new_str
  #
  # Returns a copy of <i>str</i> with leading whitespace removed. See also
  # <code>String#rstrip</code> and <code>String#strip</code>.
  #
  #    "  hello  ".lstrip   #=> "hello  "
  #    "hello".lstrip       #=> "hello"
  #
  def lstrip
    a = 0
    z = self.size - 1
    a += 1 while " \f\n\r\t\v".include?(self[a]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  ##
  # call-seq:
  #    str.rstrip   -> new_str
  #
  # Returns a copy of <i>str</i> with trailing whitespace removed. See also
  # <code>String#lstrip</code> and <code>String#strip</code>.
  #
  #    "  hello  ".rstrip   #=> "  hello"
  #    "hello".rstrip       #=> "hello"
  #
  def rstrip
    a = 0
    z = self.size - 1
    z -= 1 while " \f\n\r\t\v\0".include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  ##
  # call-seq:
  #    str.strip   -> new_str
  #
  # Returns a copy of <i>str</i> with leading and trailing whitespace removed.
  #
  #    "    hello    ".strip   #=> "hello"
  #    "\tgoodbye\r\n".strip   #=> "goodbye"
  #
  def strip
    a = 0
    z = self.size - 1
    a += 1 while " \f\n\r\t\v".include?(self[a]) and a <= z
    z -= 1 while " \f\n\r\t\v\0".include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  ##
  # call-seq:
  #    str.lstrip!   -> self or nil
  #
  # Removes leading whitespace from <i>str</i>, returning <code>nil</code> if no
  # change was made. See also <code>String#rstrip!</code> and
  # <code>String#strip!</code>.
  #
  #    "  hello  ".lstrip   #=> "hello  "
  #    "hello".lstrip!      #=> nil
  #
  def lstrip!
    s = self.lstrip
    (s == self) ? nil : self.replace(s)
  end

  ##
  # call-seq:
  #    str.rstrip!   -> self or nil
  #
  # Removes trailing whitespace from <i>str</i>, returning <code>nil</code> if
  # no change was made. See also <code>String#lstrip!</code> and
  # <code>String#strip!</code>.
  #
  #    "  hello  ".rstrip   #=> "  hello"
  #    "hello".rstrip!      #=> nil
  #
  def rstrip!
    s = self.rstrip
    (s == self) ? nil : self.replace(s)
  end

  ##
  #  call-seq:
  #     str.strip!   -> str or nil
  #
  #  Removes leading and trailing whitespace from <i>str</i>. Returns
  #  <code>nil</code> if <i>str</i> was not altered.
  #
  def strip!
    s = self.strip
    (s == self) ? nil : self.replace(s)
  end

  ##
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

  ##
  # call-seq:
  #    str.slice!(fixnum)           -> new_str or nil
  #    str.slice!(fixnum, fixnum)   -> new_str or nil
  #    str.slice!(range)            -> new_str or nil
  #    str.slice!(other_str)        -> new_str or nil
  #
  # Deletes the specified portion from <i>str</i>, and returns the portion
  # deleted.
  #
  #    string = "this is a string"
  #    string.slice!(2)        #=> "i"
  #    string.slice!(3..6)     #=> " is "
  #    string.slice!("r")      #=> "r"
  #    string                  #=> "thsa sting"
  #
  def slice!(arg1, arg2=nil)
    raise "wrong number of arguments (for 1..2)" if arg1 == nil && arg2 == nil

    if arg1 != nil && arg2 != nil
      idx = arg1
      idx += self.size if arg1 < 0
      if idx >= 0 && idx <= self.size && arg2 > 0
        str = self[idx, arg2]
      else
        return nil
      end
    else
      validated = false
      if arg1.kind_of?(Range)
        beg = arg1.begin
        ed = arg1.end
        beg += self.size if beg < 0
        ed += self.size if ed < 0
        validated = true
      elsif arg1.kind_of?(String)
        validated = true
      else
        idx = arg1
        idx += self.size if arg1 < 0
        validated = true if idx >=0 && arg1 < self.size   
      end
      if validated
        str = self[arg1]
      else
        return nil
      end
    end
    unless str == nil || str == ""
      if arg1 != nil && arg2 !=nil
        idx = arg1 >= 0 ? arg1 : self.size+arg1
        str2 = self[0...idx] + self[idx+arg2..-1]
      else
        if arg1.kind_of?(Range)
          idx = beg >= 0 ? beg : self.size+beg
          idx2 = ed>= 0 ? ed : self.size+ed
          str2 = self[0...idx] + self[idx2+1..-1]
        elsif arg1.kind_of?(String)
          idx = self.index(arg1)
          str2 = self[0...idx] + self[idx+arg1.size..-1] unless idx == nil
        else
          idx = arg1 >= 0 ? arg1 : self.size+arg1
          str2 = self[0...idx] + self[idx+1..-1]
        end
      end
      self.replace(str2) unless str2 == nil
    end
    str
  end
end
