class String
  # Capture the C-defined String#split under `__split` before the override
  # below replaces it, so the override can delegate non-regexp patterns
  # back to the core implementation.
  alias __split split

  # `match` and `match?` accept a Regexp or a String and reject everything
  # else.  The check lives in C (see Regexp.__match_pattern) so that the
  # argument cannot steer it: it cannot pose as a Regexp, and there is no
  # helper on String for a subclass to redefine.  Compiling an accepted String
  # stays here, so the check does not have to call back into the VM; `String
  # ===` goes through `Module#===` and cannot be redefined either.
  def match(re, pos = 0, &block)
    re = Regexp.__match_pattern(re)
    re = Regexp.new(re) if String === re
    re.match(self, pos, &block)
  end

  def match?(re, pos = 0)
    re = Regexp.__match_pattern(re)
    re = Regexp.new(re) if String === re
    re.match?(self, pos)
  end

  def =~(re)
    # A String argument would dispatch back to this method and recurse, so
    # reject it up front (CRuby raises the same TypeError).
    raise TypeError, "type mismatch: String given" if re.is_a?(String)
    re =~ self
  end

  def sub(*args, &block)
    # CRuby accepts 1..2 arguments with a block, but demands exactly 2
    # without one, and reports the expected count accordingly.
    if block
      unless (1..2).include?(args.length)
        raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
      end
    elsif args.length != 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 2)"
    end
    pattern, replacement = *args
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    # A replacement argument wins over the block, as in CRuby.
    if args.length == 2
      return pattern.__sub_str(self, replacement.to_s)
    end
    md = pattern.match(self)
    return self.dup unless md
    md.pre_match + block.call(md[0]).to_s + md.post_match
  end

  def gsub(*args, &block)
    unless (1..2).include?(args.length)
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    # Without mruby-enumerator this is core Kernel#to_enum, which raises
    # NotImplementedError; every other path here stays usable, so the gem does
    # not depend on Enumerator.
    return to_enum(:gsub, *args) if args.length == 1 && !block
    pattern, replacement = *args
    pattern = Regexp.new(Regexp.escape(pattern)) if pattern.is_a?(String)
    # A replacement argument wins over the block, as in CRuby.
    if args.length == 2
      return pattern.__gsub_str(self, replacement.to_s)
    end
    # block case: keep in Ruby to avoid VM callback from C
    parts = []
    pos = 0
    len = self.bytesize
    binary = Regexp.__binary_string?(self)
    while pos <= len
      md = pattern.__byte_match(self, pos)
      break unless md
      # gsub works in byte space (match pos, byteslice). begin/end report
      # character offsets (CRuby-compatible), so use the byte accessors.
      match_start = md.__byte_begin(0)
      match_end = md.__byte_end(0)
      parts << self.byteslice(pos, match_start - pos)
      parts << block.call(md[0]).to_s
      if match_start == match_end
        if match_end < len
          if binary
            parts << self.byteslice(match_end, 1)
            pos = match_end + 1
          else
            rest = self.byteslice(match_end..-1)
            char = rest[0]
            parts << char
            pos = match_end + char.bytesize
          end
        else
          pos = match_end + 1
        end
      else
        pos = match_end
      end
    end
    parts << self.byteslice(pos..-1)
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
  # nil or string patterns, and handles regexp patterns in Ruby.
  def split(pattern = nil, *args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length + 1}, expected 0..2)"
    end

    limit_given = args.length > 0
    limit = limit_given ? args[0] : 0
    if limit_given && !limit.is_a?(Integer)
      if limit.respond_to?(:to_int)
        limit = limit.to_int
        unless limit.is_a?(Integer)
          raise TypeError, "no implicit conversion of #{limit.class} to Integer)"
        end
      else
        limit = limit.__to_int
      end
    end
    if pattern.nil? || pattern.is_a?(String)
      return limit_given ? __split(pattern, limit) : __split(pattern)
    end
    return self.empty? ? [] : [self] if limit == 1
    unless pattern.is_a?(Regexp)
      raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
    end

    result = []
    field_start = 0
    search_pos = 0
    len = self.bytesize
    count = 0
    while search_pos <= len
      if limit > 0 && count >= limit - 1
        result << (self.byteslice(field_start..-1) || "")
        return result
      end
      md = pattern.__byte_match(self, search_pos)
      break unless md
      match_start = md.__byte_begin(0)
      match_end = md.__byte_end(0)

      if match_start == match_end
        rest = self.byteslice(match_end..-1)
        if rest && rest.bytesize > 0
          char = rest[0]
          search_pos = match_end + char.bytesize
        else
          search_pos = match_end + 1
        end
        next if match_start == field_start
      end

      result << self.byteslice(field_start, match_start - field_start)
      count += 1

      if match_start == match_end
        field_start = match_end
      else
        field_start = match_end
        search_pos = match_end
      end
      i = 1
      while i < md.length
        result << md[i] unless md[i].nil?
        i += 1
      end
    end
    if len > 0 && field_start <= len && (field_start < len || limit != 0)
      result << self.byteslice(field_start..-1)
    end

    if limit == 0
      while result.length > 0 && result[-1] == ""
        result.pop
      end
    end
    result
  end
end
