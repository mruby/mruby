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

  def sub(*args, &block)
    # CRuby accepts 1..2 arguments with a block, but demands exactly 2
    # without one, and reports the expected count accordingly.  The count is
    # read once and compared, rather than asked of a Range built for the
    # question: this is the first thing every call does, and a Range costs an
    # object and a call to answer what two comparisons answer.
    argc = args.length
    if block
      unless argc == 1 || argc == 2
        raise ArgumentError, "wrong number of arguments (given #{argc}, expected 1..2)"
      end
    elsif argc != 2
      raise ArgumentError, "wrong number of arguments (given #{argc}, expected 2)"
    end
    pattern = Regexp.__check_pattern(args[0])
    # Unlike `match`, a String pattern is quoted rather than compiled: it is a
    # literal here, the distinction CRuby draws between get_pat_quoted and
    # get_pat.  Only the quoting is taken from it: get_pat_quoted also accepts
    # anything answering `to_str`, where `__check_pattern` keeps to a real
    # String, as `match` already does.
    literal = String === pattern
    # A replacement argument wins over the block, as in CRuby.  A literal goes
    # to `__sub_lit`, which searches for its bytes without compiling anything
    # to search with.  What a compiled pattern would be needed for is the
    # Regexp the `$~` it publishes names, and `MatchData#regexp` quotes that
    # one where CRuby quotes it: on the first call that asks for it.
    # A Hash replacement is not text to substitute but a table to look the
    # match up in, which is the block form with the lookup where the call
    # would be: CRuby runs the two down the same tail, so a `\1` in what comes
    # back stands for itself rather than naming a group, and a key the Hash
    # does not hold answers nil, which `to_s` spells as the empty string.  The
    # Hash has to be one, as `__check_pattern` demands a real String: mruby
    # converts nothing with `to_hash` anywhere.
    hash = nil
    if argc == 2
      if Hash === args[1]
        hash = args[1]
      else
        replacement = args[1].to_s
        return Regexp.__sub_lit(pattern, self, replacement) if literal
        return Regexp.__sub_str(pattern, self, replacement)
      end
    end
    # CRuby searches for a literal byte by byte and never reads the subject as
    # UTF-8 on the way, so quoting one into a Regexp here must not put the
    # subject through a check CRuby does not make: `"a\x80b".sub("b", "!")`
    # answers there, where the same call with `/b/` is refused.
    pattern = Regexp.new(Regexp.escape(pattern)) if literal
    md = Regexp.__search(pattern, self, 0, literal)
    return self.dup unless md
    md.pre_match + (hash ? hash[md[0]] : block.call(md[0])).to_s + md.post_match
  end

  def sub!(*args, &block)
    # The argument checks come before the frozen receiver, as in CRuby:
    # `"abc".freeze.sub!(/b/)` raises ArgumentError and
    # `"abc".freeze.sub!(:b, "X")` TypeError, while the two-argument form on
    # the same receiver raises FrozenError.  `gsub!` orders it the other way,
    # also as CRuby does.
    argc = args.length
    if block
      unless argc == 1 || argc == 2
        raise ArgumentError, "wrong number of arguments (given #{argc}, expected 1..2)"
      end
    elsif argc != 2
      raise ArgumentError, "wrong number of arguments (given #{argc}, expected 2)"
    end
    # Resolved here rather than left to `sub` because the match below decides
    # the return value, and a String pattern is a literal on both paths.
    pattern = Regexp.__check_pattern(args[0])
    literal = String === pattern
    # A table to look the match up in rather than text to substitute, as in
    # `sub`; the tail below is where the lookup happens, so both of the paths
    # a replacement takes have to let it past.
    hash = args[1] if argc == 2 && Hash === args[1]
    # Quoting the literal below raises nothing, so asking here is the order the
    # original had: after the argument checks, before any search.
    raise FrozenError, "can't modify frozen String" if frozen?
    # Whether a substitution happened is a question about the match, not about
    # the result: `"aaa".sub!(/a/, "a")` returns self even though the string is
    # unchanged.  The `bang` argument is that question asked of the one search
    # `__sub_lit` already makes, so the literal path does not walk the subject
    # twice to answer it; a failed search clears $~ there as `__search` does.
    if literal && argc == 2 && !hash
      str = Regexp.__sub_lit(pattern, self, args[1].to_s, true)
      return nil unless str
      return self.replace(str)
    end
    pattern = Regexp.new(Regexp.escape(pattern)) if literal
    # A full search and not `match?`, so a failed match clears $~.
    md = Regexp.__search(pattern, self, 0, literal)
    return nil unless md
    if argc == 2 && !hash
      # `sub` matches again and publishes its own $~ over this one, leaving
      # the caller the match `sub` would have left.  The resolved pattern
      # goes down so that it is not compiled a second time; a literal with a
      # replacement never reaches here, having been answered by `__sub_lit`
      # above.  Overwriting `self` afterwards is safe: a MatchData snapshots
      # its subject, so $~ keeps describing the string as it was matched.
      return self.replace(self.sub(pattern, args[1]))
    end
    # The block form does not go down to `sub`, which builds its answer from
    # the snapshot the MatchData holds, because CRuby's `rb_str_sub_bang`
    # builds it from the receiver as the block left it: `s = "hello";
    # s.sub!(/l/) { s.upcase!; "X" }` is "HEXLO" there, where `sub` on the
    # same receiver is "heXlo".  It refuses a block that changed the length
    # first, as `gsub` does, so the offsets of the match still name the bytes
    # they named.  $~ stays what the search above published, or whatever the
    # block put there.  A Hash replacement is here rather than above for the
    # same reason: its default proc is free to reach the receiver, and CRuby
    # answers a lookup that did with the receiver it left.
    len = self.bytesize
    val = (hash ? hash[md[0]] : block.call(md[0])).to_s
    raise RuntimeError, "string modified" if self.bytesize != len
    self.replace(self.byteslice(0, md.__byte_begin(0)) + val + self.byteslice(md.__byte_end(0)..-1))
  end

  def gsub(*args, &block)
    argc = args.length
    unless argc == 1 || argc == 2
      raise ArgumentError, "wrong number of arguments (given #{argc}, expected 1..2)"
    end
    # Without mruby-enumerator this is core Kernel#to_enum, which raises
    # NotImplementedError; every other path here stays usable, so the gem does
    # not depend on Enumerator.
    return to_enum(:gsub, *args) if argc == 1 && !block
    pattern = args[0]
    # After the to_enum return above, so that `"abc".gsub(:b)` yields an
    # Enumerator and raises on the first iteration, as CRuby does.
    pattern = Regexp.__check_pattern(pattern)
    # A String pattern is a literal, as in `sub`, and reaches the subject the
    # way CRuby reaches it: byte by byte, with no reading of it as UTF-8.
    literal = String === pattern
    # A replacement argument wins over the block, as in CRuby.  A literal is
    # searched for as bytes and compiles nothing, as in `sub` above.
    hash = nil
    if argc == 2
      if Hash === args[1]
        hash = args[1]
      else
        replacement = args[1].to_s
        return Regexp.__gsub_lit(pattern, self, replacement) if literal
        return Regexp.__gsub_str(pattern, self, replacement)
      end
    end
    pattern = Regexp.new(Regexp.escape(pattern)) if literal
    # A Hash replacement is a table the match is looked up in, as in `sub`,
    # and CRuby's `str_gsub` walks it with the very loop it walks a block
    # with: what the receiver looks like to each turn, the refusal of a
    # lookup that changed its length and the `$~` left behind are the ones
    # `__gsub_block` already answers for, so the lookup goes down as the
    # block it stands in for.
    return Regexp.__gsub_block(pattern, self, literal) { |m| hash[m] } if hash
    # The walk and the block call are both in `__gsub_block`.  What the loop
    # was written here for, the block reading the globals of the match it was
    # handed, a C loop publishes just as well, and it pays neither the frame
    # per search nor the array of pieces the mrblib one collected.
    Regexp.__gsub_block(pattern, self, literal, &block)
  end

  def gsub!(*args, &block)
    # Before the arity check and before the enumerator below, as in CRuby:
    # `"abc".freeze.gsub!(/a/)` raises FrozenError rather than handing back an
    # Enumerator that fails later.
    raise FrozenError, "can't modify frozen String" if frozen?
    argc = args.length
    unless argc == 1 || argc == 2
      raise ArgumentError, "wrong number of arguments (given #{argc}, expected 1..2)"
    end
    return to_enum(:gsub!, *args) if argc == 1 && !block
    pattern = Regexp.__check_pattern(args[0])
    literal = String === pattern
    # As in `sub!`: the match decides the return value, and a failed search
    # clears $~.  What it publishes on success is replaced right away by the
    # last match of the `gsub` below, which is the one CRuby leaves behind.
    # A literal goes down as the String it was, for the reason `sub!` gives,
    # and a literal with a replacement asks the question of `__gsub_lit`
    # itself rather than searching once to ask and again to substitute.  A
    # Hash is no replacement to hand it: it goes to the `gsub` below, which
    # looks the match up in it.
    if literal && argc == 2 && !(Hash === args[1])
      str = Regexp.__gsub_lit(pattern, self, args[1].to_s, true)
      return nil unless str
      return self.replace(str)
    end
    pattern = Regexp.new(Regexp.escape(pattern)) if literal
    return nil unless Regexp.__search(pattern, self, 0, literal)
    down = literal ? args[0] : pattern
    str = argc == 2 ? self.gsub(down, args[1], &block) : self.gsub(down, &block)
    self.replace(str)
  end

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
