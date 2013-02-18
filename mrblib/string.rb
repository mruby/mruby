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
    lc = ''
    if args.size == 2
      lc = args[1] if self[-1] == args[0]
      split(args[0]).join(args[1]) + lc
    elsif args.size == 1 && block
      split(args[0]).join(block.call(args[0]))
    else
      raise ArgumentError, "wrong number of arguments"
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
    if args.size == 2
      split(args[0], 2).join(args[1])
    elsif args.size == 1 && block
      split(args[0], 2).join(block.call(args[0]))
    else
      raise ArgumentError, "wrong number of arguments"
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
end

##
# String is comparable
#
# ISO 15.2.10.3
module Comparable; end
class String
  include Comparable
end
