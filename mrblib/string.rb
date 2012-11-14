##
# String
#
# ISO 15.2.10
class String

  ##
  # Calls the given block for each line
  # and pass the respective line.
  #
  # ISO 15.2.10.5.15
  def each_line(&block)
    # expect that str.index accepts an Integer for 1st argument as a byte data
    offset = 0
    while(pos = self.index(0x0a, offset))
      block.call(self[offset, pos + 1 - offset])
      offset = pos + 1
    end
    block.call(self[offset, self.size - offset]) if self.size > offset
    self
  end

  ##
  # Replace all matches of +pattern+ with +replacement+.
  # Call block (if given) for each match and replace
  # +pattern+ with the value of the block. Return the
  # final value.
  #
  # ISO 15.2.10.5.18
  def gsub(*args, &block)
    unless (args.size == 1 && block) || args.size == 2
      raise ArgumentError, "wrong number of arguments"
    end

    ### *** TODO *** ###
    unless Object.const_defined?(:Regexp)
      raise NotImplementedError, "gsub not available (yet)"
    else
      self._gsub(*args, &block)
    end
  end

  ##
  # Replace all matches of +pattern+ with +replacement+.
  # Call block (if given) for each match and replace
  # +pattern+ with the value of the block. Modify
  # +self+ with the final value.
  #
  # ISO 15.2.10.5.19
  def gsub!(*args, &block)
    str = self.gsub(*args, &block)
    if str != self
      self.replace(str)
      self
    else
      nil
    end
  end

  ##
  # Calls the given block for each match of +pattern+
  # If no block is given return an array with all
  # matches of +pattern+.
  #
  # ISO 15.2.10.5.32
  def scan(reg, &block)
    ### *** TODO *** ###
    unless Object.const_defined?(:Regexp)
      raise NotImplementedError, "scan not available (yet)"
    else
      self._scan(reg, &block)
    end
  end

  ##
  # Replace only the first match of +pattern+ with
  # +replacement+. Call block (if given) for each
  # match and replace +pattern+ with the value of the
  # block. Return the final value.
  #
  # ISO 15.2.10.5.36
  def sub(*args, &block)
    unless (args.size == 1 && block) || args.size == 2
      raise ArgumentError, "wrong number of arguments"
    end

    ### *** TODO *** ###
    unless Object.const_defined?(:Regexp)
      raise NotImplementedError, "sub not available (yet)"
    else
      self._sub(*args, &block)
    end
  end

  ##
  # Replace only the first match of +pattern+ with
  # +replacement+. Call block (if given) for each
  # match and replace +pattern+ with the value of the
  # block. Modify +self+ with the final value.
  #
  # ISO 15.2.10.5.37
  def sub!(*args, &block)
    str = self.sub(*args, &block)
    if str != self
      self.replace(str)
      self
    else
      nil
    end
  end

  ##
  # Call the given block for each character of
  # +self+.
  def each_char(&block)
    pos = 0
    while(pos < self.size)
      block.call(self[pos])
      pos += 1
    end
    self
  end

  ##
  # Call the given block for each byte of +self+.
  def each_byte(&block)
    bytes = self.bytes
    pos = 0
    while(pos < bytes.size)
      block.call(bytes[pos])
      pos += 1
    end
    self
  end

  ##
  # Modify +self+ by replacing the content of +self+
  # at the position +pos+ with +value+.
  def []=(pos, value)
    b = self[0, pos]
    a = self[pos+1..-1]
    self.replace([b, value, a].join(''))
  end

  # for strip family
  SPACES__ = " \f\n\r\t\v"

  def lstrip
    a = 0
    z = self.size - 1
    a += 1 while SPACES__.include?(self[a]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def rstrip
    a = 0
    z = self.size - 1
    z -= 1 while SPACES__.include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def strip
    a = 0
    z = self.size - 1
    a += 1 while SPACES__.include?(self[a]) and a <= z
    z -= 1 while SPACES__.include?(self[z]) and a <= z
    (z >= 0) ? self[a..z] : ""
  end

  def strip!
    self.replace(self.strip)
  end

  def %(args)
    sprintf(self, *args)
  end

  def slice!(arg1, arg2 = 1)
    if arg1.class == Fixnum
      rval = self[arg1, arg2]
      self.replace(self[0...arg1] + self[(arg1 + arg2)...-1]) if arg2 > 0
    elsif arg1.class == String
      rval = arg1
      self.gsub!(arg1, "")
    else
      return nil
    end
    rval
  end
end

##
# String is comparable
#
# ISO 15.2.10.3
module Comparable; end
class String
  include Comparable
end
