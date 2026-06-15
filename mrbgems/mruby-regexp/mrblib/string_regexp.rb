class String
  # Capture the C-defined String#split under `__split` before the override
  # below replaces it, so the override can delegate non-regexp patterns
  # back to the core implementation.
  alias __split split

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
    pos = 0
    while pos <= self.length
      md = pattern.match(self, pos)
      break unless md
      match_start = md.begin(0)
      match_end = md.end(0)
      parts << self[pos...match_start]
      parts << block.call(md[0]).to_s
      if match_start == match_end
        parts << self[match_end] if match_end < self.length
        pos = match_end + 1
      else
        pos = match_end
      end
    end
    parts << self[pos..-1]
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

  # Regexp-aware split.  Falls back to the C-defined split (aliased as
  # `__split` in mrb_mruby_regexp_gem_init before this override loads) for
  # nil or simple-string patterns; converts string-with-backslash to a
  # Regexp and handles regexp patterns in Ruby.
  def split(pattern = nil, limit = -1)
    return __split(pattern, limit) if pattern.nil?
    if pattern.is_a?(String)
      return __split(pattern, limit) if pattern.length == 1 || !pattern.include?('\\')
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
