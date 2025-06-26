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


  def partition(sep)
    raise TypeError, "type mismatch: #{sep.class} given" unless sep.is_a? String
    n = index(sep)
    unless n.nil?
      m = n + sep.size
      [ slice(0, n), sep, slice(m, size - m) ]
    else
      [ self[0..-1], "", "" ]
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
    raise FrozenError, "can't modify frozen String" if frozen?
    raise ArgumentError, "wrong number of arguments (expected 1..2)" if arg1.nil? && arg2.nil?

    if !arg1.nil? && !arg2.nil?
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
        ed -= 1 if arg1.exclude_end?
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
    unless str.nil? || str == ""
      if !arg1.nil? && !arg2.nil?
        idx = arg1 >= 0 ? arg1 : self.size+arg1
        str2 = self[0...idx] + self[idx+arg2..-1].to_s
      else
        if arg1.kind_of?(Range)
          idx = beg >= 0 ? beg : self.size+beg
          idx2 = ed>= 0 ? ed : self.size+ed
          str2 = self[0...idx] + self[idx2+1..-1].to_s
        elsif arg1.kind_of?(String)
          idx = self.index(arg1)
          str2 = self[0...idx] + self[idx+arg1.size..-1] unless idx.nil?
        else
          idx = arg1 >= 0 ? arg1 : self.size+arg1
          str2 = self[0...idx] + self[idx+1..-1].to_s
        end
      end
      self.replace(str2) unless str2.nil?
    end
    str
  end

  ##
  #  call-seq:
  #     str.insert(index, other_str)   -> str
  #
  #  Inserts <i>other_str</i> before the character at the given
  #  <i>index</i>, modifying <i>str</i>. Negative indices count from the
  #  end of the string, and insert <em>after</em> the given character.
  #  The intent is insert <i>aString</i> so that it starts at the given
  #  <i>index</i>.
  #
  #     "abcd".insert(0, 'X')    #=> "Xabcd"
  #     "abcd".insert(3, 'X')    #=> "abcXd"
  #     "abcd".insert(4, 'X')    #=> "abcdX"
  #     "abcd".insert(-3, 'X')   #=> "abXcd"
  #     "abcd".insert(-1, 'X')   #=> "abcdX"
  #
  def insert(idx, str)
    if idx == -1
      return self << str
    elsif idx < 0
      idx += 1
    end
    self[idx, 0] = str
    self
  end

  ##
  #  call-seq:
  #     str.ljust(integer, padstr=' ')   -> new_str
  #
  #  If <i>integer</i> is greater than the length of <i>str</i>, returns a new
  #  <code>String</code> of length <i>integer</i> with <i>str</i> left justified
  #  and padded with <i>padstr</i>; otherwise, returns <i>str</i>.
  #
  #     "hello".ljust(4)            #=> "hello"
  #     "hello".ljust(20)           #=> "hello               "
  #     "hello".ljust(20, '1234')   #=> "hello123412341234123"
  def ljust(idx, padstr = ' ')
    raise ArgumentError, 'zero width padding' if padstr == ''
    return self if idx <= self.size
    pad_repetitions = idx / padstr.size
    padding = (padstr * pad_repetitions)[0, idx-self.size]
    self + padding
  end

  ##
  #  call-seq:
  #     str.rjust(integer, padstr=' ')   -> new_str
  #
  #  If <i>integer</i> is greater than the length of <i>str</i>, returns a new
  #  <code>String</code> of length <i>integer</i> with <i>str</i> right justified
  #  and padded with <i>padstr</i>; otherwise, returns <i>str</i>.
  #
  #     "hello".rjust(4)            #=> "hello"
  #     "hello".rjust(20)           #=> "               hello"
  #     "hello".rjust(20, '1234')   #=> "123412341234123hello"
  def rjust(idx, padstr = ' ')
    raise ArgumentError, 'zero width padding' if padstr == ''
    return self if idx <= self.size
    pad_repetitions = idx / padstr.size
    padding = (padstr * pad_repetitions)[0, idx-self.size]
    padding + self
  end

  ##
  #  call-seq:
  #     str.center(width, padstr=' ')   -> new_str
  #
  #  Centers +str+ in +width+. If +width+ is greater than the length of +str+,
  #  returns a new String of length +width+ with +str+ centered and padded with
  #  +padstr+; otherwise, returns +str+.
  #
  #     "hello".center(4)         #=> "hello"
  #     "hello".center(20)        #=> "       hello        "
  #     "hello".center(20, '123') #=> "1231231hello12312312"
  def center(width, padstr = ' ')
    raise ArgumentError, 'zero width padding' if padstr == ''
    return self if width <= self.size
    width -= self.size
    pad1 = width / 2
    pad2 = width - pad1
    (padstr*pad1)[0,pad1] + self + (padstr*pad2)[0,pad2]
  end


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
  # call-seq:
  #    str.prepend(other_str)  -> str
  #
  # Prepend---Prepend the given string to <i>str</i>.
  #
  #    a = "world"
  #    a.prepend("hello ") #=> "hello world"
  #    a                   #=> "hello world"
  def prepend(*args)
    len = args.size
    while len > 0
      len -= 1
      self[0, 0] = args[len]
    end
    self
  end

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
    end
    lines
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
