class String
  def match(re, pos = 0)
    re = Regexp.new(re) if re.is_a?(String)
    re.match(self, pos)
  end

  def match?(re, pos = 0)
    re = Regexp.new(re) if re.is_a?(String)
    re.match?(self, pos)
  end

  def =~(re)
    re =~ self
  end

  def sub(pattern, replacement = nil, &block)
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    md = pattern.match(self)
    return self.dup unless md

    pre = md.pre_match
    post = md.post_match
    if block
      rep = block.call(md[0]).to_s
    else
      rep = replacement.to_s
      # handle \0, \1, etc. in replacement string
      rep = rep.gsub(/\\(\d)/) { md[$1.to_i] || "" } if rep.include?("\\")
    end
    pre + rep + post
  end

  def gsub(pattern, replacement = nil, &block)
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    result = ""
    rest = self
    while rest.length > 0
      md = pattern.match(rest)
      break unless md
      result += md.pre_match
      if block
        result += block.call(md[0]).to_s
      else
        rep = replacement.to_s
        rep = rep.gsub(/\\(\d)/) { md[$1.to_i] || "" } if rep.include?("\\")
        result += rep
      end
      matched_len = md[0].length
      if matched_len == 0
        # avoid infinite loop on zero-length match
        result += rest[0] if rest.length > 0
        rest = rest[1..-1] || ""
      else
        rest = md.post_match
      end
    end
    result + rest
  end

  def scan(pattern)
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    result = []
    pos = 0
    while pos <= self.length
      md = pattern.match(self, pos)
      break unless md
      if md.captures.empty?
        result << md[0]
      elsif md.captures.length == 1
        result << md.captures[0]
      else
        result << md.captures
      end
      if md[0].length == 0
        pos = md.end(0) + 1
      else
        pos = md.end(0)
      end
    end
    if block_given?
      result.each { |m| yield m }
      self
    else
      result
    end
  end

  def split(pattern = nil, limit = -1)
    return super if pattern.nil?
    if pattern.is_a?(String)
      return super if pattern.length == 1 || !pattern.include?('\\')
      pattern = Regexp.new(Regexp.escape(pattern))
    end
    result = []
    rest = self
    count = 0
    while rest.length > 0
      if limit > 0 && count >= limit - 1
        result << rest
        return result
      end
      md = pattern.match(rest)
      break unless md
      result << md.pre_match
      rest = md.post_match
      count += 1
      # skip zero-length match at beginning
      if md[0].length == 0
        if rest.length > 0
          result[-1] = result[-1] + rest[0]
          rest = rest[1..-1] || ""
        else
          break
        end
      end
    end
    result << rest
    # remove trailing empty strings if no limit
    if limit < 0
      while result.length > 0 && result[-1] == ""
        result.pop
      end
    end
    result
  end
end
