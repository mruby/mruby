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
    unless block
      return pattern.__sub_str(self, replacement.to_s)
    end
    md = pattern.match(self)
    return self.dup unless md
    md.pre_match + block.call(md[0]).to_s + md.post_match
  end

  def gsub(pattern, replacement = nil, &block)
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    unless block
      return pattern.__gsub_str(self, replacement.to_s)
    end
    # block case: keep in Ruby to avoid VM callback from C
    parts = []
    rest = self
    while rest.length > 0
      md = pattern.match(rest)
      break unless md
      parts << md.pre_match
      parts << block.call(md[0]).to_s
      matched_len = md[0].length
      if matched_len == 0
        parts << rest[0] if rest.length > 0
        rest = rest[1..-1] || ""
      else
        rest = md.post_match
      end
    end
    parts << rest
    parts.join
  end

  def scan(pattern)
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    result = pattern.__scan(self)
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
