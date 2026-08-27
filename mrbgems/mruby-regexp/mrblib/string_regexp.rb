# The overrides below take care not to let a pattern argument lie about its
# type, each in the way its own dispatch needs.  `match`, `match?`, `sub`,
# `sub!`, `gsub`, `gsub!` and `scan` hand the argument to
# `Regexp.__check_pattern`, which accepts a Regexp or a String and rejects
# everything else from C, so the argument cannot steer the decision and there
# is no Ruby-side helper for a subclass to redefine; an accepted String is
# compiled or quoted into a Regexp here before anything is searched.  `split`
# leaves a nil or String pattern to the built-in it aliased and uses the same
# check to reject everything that is not a Regexp.  `slice!` reads the real
# class with `Regexp === pattern` and leaves anything else to the built-in
# method it aliased; `[]` and `[]=` are `str_aref()` and `str_aset()` in
# src/regexp.c, where the same test is the argument's own type.  `=~` rejects a String, which would recurse
# back into this method, and hands anything that is not a Regexp to the
# argument's own `=~`, as CRuby does.
#
# With the type established, each override reaches the engine through class
# methods that take the pattern as an argument (`Regexp.__search`,
# `__byte_search`, `__byte_rsearch`, `__search_p`, `__sub_str`, `__gsub_str`,
# `__sub_lit`, `__gsub_lit`, `__gsub_block`, `__scan`), so
# nothing rewritten on the pattern instance is consulted on the way: the C
# side searches, and what loops and blocks remain stay here.  The MatchData those
# searches answer is built in C too, so what the overrides read from it
# (`[]`, `pre_match`, `begin` and the rest) cannot have been planted by an
# argument.  What remains reachable is a class-wide redefinition of a
# MatchData method, which is the same category as redefining `String#sub`
# itself and is not steered by the argument.
#
# `String#match` is the one deliberate exception: it dispatches `match` to
# the pattern because CRuby's `rb_str_match_m()` does so on purpose, and
# following that is the correct behaviour, not a hole.  The `=~` forward for
# a non-Regexp argument is the same shape, from `rb_str_match()`.  Everywhere
# else the gem is closed the way CRuby is: `rb_str_sub_bang()`,
# `rb_str_subpat()` and the rest reach the engine without asking the pattern
# anything.
class String
  # Capture the C-defined String#split under `__split` before the override
  # below replaces it, so the override can delegate non-regexp patterns
  # back to the core implementation.
  alias __split split

  def scan(pattern)
    pattern = Regexp.__check_pattern(pattern)
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
    return Regexp.__scan(pattern, self) unless block_given?
    # A block reads the match globals of the match it was handed, so the block
    # form has to walk the subject itself and let each search publish as it
    # goes. `Regexp.__scan` collects every match before anything is yielded,
    # which leaves only the last one published, so every call of the block saw
    # the same final `$~`, `` $` ``, `$'` and `$1`.
    #
    # Yield what `__scan` collects: the matched string where the pattern has no
    # group, and an array of the groups where it has any, a single one
    # included. A zero-width match steps one byte on, which is what stops the
    # next search reporting the same place; the engine steps over a byte inside
    # a character on its own.
    pos = 0
    len = self.bytesize
    # A block that changes the receiver is answered for as `rb_str_scan`
    # answers for it, which is the way `__gsub_block` does: one that changed
    # the length is refused with `RuntimeError` by the next search, which takes
    # `len` for that, the next match is searched for in the string it left,
    # and the match left in $~ is a search once more from the offset the last
    # match was found from, on the string as it stands at the end. That search
    # also republishes what the failed one that ends the loop clears; a scan
    # that matched nothing keeps the cleared state. And as in `gsub`, a
    # receiver that still reads as it did when the last match was made gets
    # that match republished, and the search runs only where `__republish`
    # finds it reading differently.
    last = nil
    last_md = nil
    while pos <= len
      md = Regexp.__byte_search(pattern, self, pos, len)
      break unless md
      last = pos
      last_md = md
      yield(md.size == 1 ? md[0] : md.captures)
      match_start = md.__byte_begin(0)
      match_end = md.__byte_end(0)
      pos = match_start == match_end ? match_end + 1 : match_end
    end
    Regexp.__byte_search(pattern, self, last, len) if last && !last_md.__republish(self)
    self
  end

  # Regexp-aware split.  Falls back to the C-defined split (aliased as
  # `__split` above) for nil or string patterns, and handles regexp patterns
  # in Ruby.
  def split(pattern = nil, *args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length + 1}, expected 0..2)"
    end

    limit_given = args.length > 0
    limit = limit_given ? args[0] : 0
    # `Integer.__ensure` is `mrb_ensure_int_type()`, which asks the object nothing.
    # mruby has no implicit conversion protocol in core, so `Array.new(obj)`,
    # `ary[obj]` and `"s" * obj` all reject an object that only defines
    # `to_int`; dispatching it here would leave this the one place in the tree
    # that accepts one, as the same reasoning keeps `match` off `to_str`.
    # Every limit goes through it, an Integer included: a Bigint is an Integer
    # and does not fit `mrb_int`, and `__ensure` is what narrows it and raises
    # the `RangeError` `__split` raises on the string path.
    limit = Integer.__ensure(limit) if limit_given
    # `nil?` and `is_a?` are redefinable, so an argument answering either one
    # could steer itself around the check below and reach `__split` instead.
    # `Module#===` reads the real type and cannot be redefined.
    if NilClass === pattern || String === pattern
      # `__split` is core's `split`, which reaches no search of this gem's, so
      # the subject would go unread on this path where every other one refuses
      # it. CRuby refuses a String or nil pattern too, unlike the literal a
      # search is given, which is why this is not the exemption `sub` takes.
      # A limit of 1 hands the subject back whole without looking into it, and
      # CRuby answers for that as well, so the check waits behind it.
      Regexp.__check_encoding(self) unless limit == 1
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
    binary = Regexp.__binary_string?(self)
    while search_pos <= len
      if limit > 0 && count >= limit - 1
        result << (self.byteslice(field_start..-1) || "")
        return result
      end
      md = Regexp.__byte_search(pattern, self, search_pos)
      break unless md
      match_start = md.__byte_begin(0)
      match_end = md.__byte_end(0)

      if match_start == match_end
        if binary
          # A byte-indexed subject has one position per byte, and the step
          # below reads the rest of it as UTF-8: `byteslice` hands back a
          # string without the flag, so its first element is a whole character
          # again. `gsub` steps by a byte here for the same reason.
          search_pos = match_end + 1
        else
          rest = self.byteslice(match_end..-1)
          if rest && rest.bytesize > 0
            char = rest[0]
            search_pos = match_end + char.bytesize
          else
            search_pos = match_end + 1
          end
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
