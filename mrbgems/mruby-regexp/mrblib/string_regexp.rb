class String
  # Capture the C-defined String#split under `__split` before the override
  # below replaces it, so the override can delegate non-regexp patterns
  # back to the core implementation.
  alias __split split

  # Same for String#[], which the override below replaces along with its
  # `slice` twin.  `__aref` also serves as the internal spelling of `str[i]`
  # inside this file: the loops in `gsub` and `split` would otherwise pay for
  # a Ruby frame per iteration on a call that can never take a Regexp.
  alias __aref []

  # `match` and `match?` accept a Regexp or a String and reject everything
  # else.  The check lives in C (see Regexp.__check_pattern) so that the
  # argument cannot steer it: it cannot pose as a Regexp, and there is no
  # helper on String for a subclass to redefine.  Compiling an accepted String
  # stays here, so the check does not have to call back into the VM; `String
  # ===` goes through `Module#===` and cannot be redefined either.
  def match(re, pos = 0, &block)
    re = Regexp.__check_pattern(re)
    re = Regexp.new(re) if String === re
    re.match(self, pos, &block)
  end

  def match?(re, pos = 0)
    re = Regexp.__check_pattern(re)
    re = Regexp.new(re) if String === re
    re.match?(self, pos)
  end

  def =~(re)
    # A String argument would dispatch back to this method and recurse, so
    # reject it up front (CRuby raises the same TypeError).  `is_a?` is
    # redefinable, so a String subclass denying its own type would slip past
    # the guard and recurse anyway; `Module#===` reads the real type.
    raise TypeError, "type mismatch: String given" if String === re
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
    pattern = Regexp.__check_pattern(pattern)
    # Unlike `match`, a String pattern is quoted rather than compiled: it is a
    # literal here, the distinction CRuby draws between get_pat_quoted and
    # get_pat.  Only the quoting is taken from it: get_pat_quoted also accepts
    # anything answering `to_str`, where `__check_pattern` keeps to a real
    # String, as `match` already does.
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
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
    # After the to_enum return above, so that `"abc".gsub(:b)` yields an
    # Enumerator and raises on the first iteration, as CRuby does.
    pattern = Regexp.__check_pattern(pattern)
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
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
            char = rest.__aref(0)
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
    pattern = Regexp.__check_pattern(pattern)
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
    result = pattern.__scan(self)
    if block_given?
      result.each { |m| yield m }
      self
    else
      result
    end
  end

  # Regexp-aware element reference.  Only a Regexp is handled here; every
  # other argument list goes back to the C-defined `[]` untouched.
  #
  # Note that `str[i]`, `str[range]` and `str["sub"]` never reach this method
  # when the receiver is a String rather than a subclass: OP_GETIDX answers
  # those three argument types from C without consulting the method table, so
  # an override is not visible there.  That is harmless as long as this method
  # only delegates them, which is why the regexp branch is the only behaviour
  # added here.
  def [](*args)
    # Checked before the argument is inspected, so the non-regexp forms keep
    # reporting the CRuby arity rather than silently ignoring an extra
    # argument.
    unless (1..2).include?(args.length)
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    # `is_a?` is redefinable, so an argument denying its own type could steer
    # itself into `__aref` and be read as an index.  `Module#===` reads the
    # real type.
    return __aref(*args) unless Regexp === args[0]
    # `match`, not `match?`: `$~` has to be set even when the match fails,
    # where it becomes nil.  The MatchData is unused in the no-capture case,
    # but the global is not.
    md = args[0].match(self)
    return nil unless md
    # The capture argument is handed to MatchData#[] as it stands: an out of
    # range index answers nil and a name that resolves to no group raises
    # IndexError, which is what CRuby's rb_reg_nth_match() and
    # rb_reg_backref_number() do respectively.
    md[args.length == 2 ? args[1] : 0]
  end

  # CRuby reaches the same code for both names, and Symbol#slice (in
  # mruby-symbol-ext) delegates here, which is what makes `sym[re]` work.
  alias slice []

  # Regexp-aware split.  Falls back to the C-defined split (aliased as
  # `__split` in mrb_mruby_regexp_gem_init before this override loads) for
  # nil or string patterns, and handles regexp patterns in Ruby.
  def split(pattern = nil, *args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length + 1}, expected 0..2)"
    end

    limit_given = args.length > 0
    limit = limit_given ? args[0] : 0
    # `__to_int` is `mrb_ensure_integer_type()`, which asks the object nothing.
    # mruby has no implicit conversion protocol in core, so `Array.new(obj)`,
    # `ary[obj]` and `"s" * obj` all reject an object that only defines
    # `to_int`; dispatching it here would leave this the one place in the tree
    # that accepts one, as the same reasoning keeps `match` off `to_str`.
    # `is_a?` is redefinable, so a limit claiming to be an Integer would skip
    # that conversion and reach the arithmetic below as itself. `Module#===`
    # reads the real type and cannot be redefined.
    if limit_given && !(Integer === limit)
      limit = limit.__to_int
    end
    # `nil?` and `is_a?` are redefinable, so an argument answering either one
    # could steer itself around the check below and reach `__split` instead.
    # `Module#===` reads the real type and cannot be redefined.
    if NilClass === pattern || String === pattern
      return limit_given ? __split(pattern, limit) : __split(pattern)
    end
    return self.empty? ? [] : [self] if limit == 1
    # nil and String patterns already went to __split above, so the String
    # branch of the check is unreachable here and nothing needs quoting.
    pattern = Regexp.__check_pattern(pattern)

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
          char = rest.__aref(0)
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
