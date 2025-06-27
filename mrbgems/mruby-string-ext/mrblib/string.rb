class String

  ##
  # Call the given block for each character of
  # +self+.
  def each_char(&block)
    return to_enum :each_char unless block
    pos = 0
    while pos < self.size
      block.call(self[pos])
      pos += 1
    end
    self
  end

  ##
  # call-seq:
  #    str.chars                   -> array
  #    str.chars {|char| block }   -> str
  #
  # Returns an array of characters in str when called without a block.
  # When called with a block, passes each character to the block.
  #
  #    "hello".chars                #=> ["h", "e", "l", "l", "o"]
  #    "hello".chars {|c| print c } #=> "hello"
  #
  def chars(&block)
    if block_given?
      __chars.each(&block)
      self
    else
      __chars
    end
  end

  def codepoints(&block)
    cp = __codepoints()
    if block_given?
      cp.each do|x|
        block.call(x)
      end
      self
    else
      cp
    end
  end
  alias each_codepoint codepoints

  ##
  #  call-seq:
  #    string.lines                ->  array of string
  #    string.lines {|s| block}    ->  array of string
  #
  #  Returns strings per line;
  #
  #    a = "abc\ndef"
  #    a.lines    #=> ["abc\n", "def"]
  #
  #  If a block is given, it works the same as <code>each_line</code>.
  def lines(&blk)
    lines = self.__lines
    if blk
      lines.each do |line|
        blk.call(line)
      end
      self
    else
      lines
    end
  end

  ##
  #  call-seq:
  #     str.upto(other_str, exclusive=false) {|s| block }   -> str
  #     str.upto(other_str, exclusive=false)                -> an_enumerator
  #
  #  Iterates through successive values, starting at <i>str</i> and
  #  ending at <i>other_str</i> inclusive, passing each value in turn to
  #  the block. The <code>String#succ</code> method is used to generate
  #  each value. If optional second argument exclusive is omitted or is false,
  #  the last value will be included; otherwise it will be excluded.
  #
  #  If no block is given, an enumerator is returned instead.
  #
  #     "a8".upto("b6") {|s| print s, ' ' }
  #     for s in "a8".."b6"
  #       print s, ' '
  #     end
  #
  #  <em>produces:</em>
  #
  #     a8 a9 b0 b1 b2 b3 b4 b5 b6
  #     a8 a9 b0 b1 b2 b3 b4 b5 b6
  #
  #  If <i>str</i> and <i>other_str</i> contains only ascii numeric characters,
  #  both are recognized as decimal numbers. In addition, the width of
  #  string (e.g. leading zeros) is handled appropriately.
  #
  #     "9".upto("11").to_a   #=> ["9", "10", "11"]
  #     "25".upto("5").to_a   #=> []
  #     "07".upto("11").to_a  #=> ["07", "08", "09", "10", "11"]
  def upto(max, exclusive=false, &block)
    return to_enum(:upto, max, exclusive) unless block
    raise TypeError, "no implicit conversion of #{max.class} into String" unless max.kind_of? String

    len = self.length
    maxlen = max.length
    # single character
    if len == 1 and maxlen == 1
      c = self.ord
      e = max.ord
      while c <= e
        break if exclusive and c == e
        yield c.chr(__ENCODING__)
        c += 1
      end
      return self
    end
    # both edges are all digits
    bi = self.to_i(10)
    ei = max.to_i(10)
    if (bi > 0 or bi == "0"*len) and (ei > 0 or ei == "0"*maxlen)
      while bi <= ei
        break if exclusive and bi == ei
        s = bi.to_s
        s = s.rjust(len, "0") if s.length < len
        yield s
        bi += 1
      end
      return self
    end
    bs = self
    while true
      n = (bs <=> max)
      break if n > 0
      break if exclusive and n == 0
      yield bs
      break if n == 0
      bsiz = bs.size
      break if bsiz > max.size || bsiz == 0
      bs = bs.succ
    end
    self
  end

  def __upto_endless(&block)
    len = self.length
    # both edges are all digits
    bi = self.to_i(10)
    if bi > 0 or bi == "0"*len
      while true
        s = bi.to_s
        s = s.rjust(len, "0") if s.length < len
        yield s
        bi += 1
      end
      return self
    end
    bs = self
    while true
      yield bs
      bs = bs.succ
    end
    self
  end
end
