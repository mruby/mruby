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

  # `slice!` comes from mruby-string-ext, which this gem depends on, and is
  # overridden at the end of this file.  The core `[]=` it also reaches is
  # captured as `__aset` in src/regexp.c, before the override defined there
  # takes the name.
  alias __slice_bang slice!

  # The four search methods of src/string.c whose regexp form is overridden at
  # the end of this file.  On a build without MRB_UTF8_STRING the two of a
  # pair are the same C function behind two method table entries, so each
  # still needs its own capture.
  alias __index index
  alias __rindex rindex
  alias __byteindex byteindex
  alias __byterindex byterindex

  # The three from mruby-string-ext, which this gem depends on.
  alias __partition partition
  alias __rpartition rpartition
  alias __start_with? start_with?

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

  # Unlike `match`, the search does not dispatch on the pattern: CRuby's
  # `rb_str_match_m_p()` resolves the argument and searches it directly,
  # where `rb_str_match_m()` sends `match` to it on purpose.
  def match?(re, pos = 0)
    re = Regexp.__check_pattern(re)
    re = Regexp.new(re) if String === re
    Regexp.__search_p(re, self, pos)
  end

  def =~(re)
    # A String argument would dispatch back to this method and recurse, so
    # reject it up front (CRuby raises the same TypeError).  `is_a?` is
    # redefinable, so a String subclass denying its own type would slip past
    # the guard and recurse anyway; `Module#===` reads the real type.
    raise TypeError, "type mismatch: String given" if String === re
    # A real Regexp is searched here rather than asked, as CRuby's
    # `rb_str_match()` does: it sends `=~` to the argument only when the
    # argument is not a Regexp, which is what the tail below keeps doing.
    if Regexp === re
      md = Regexp.__search(re, self)
      return md && md.begin(0)
    end
    re =~ self
  end

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

  # The regexp-aware `[]`, `slice` and `[]=` are `str_aref()` and `str_aset()`
  # in src/regexp.c, where the indexes the core methods answer reach them
  # without a Ruby frame in between.  Everything below stays here, where a
  # block or a loop needs the VM anyway.

  # Regexp-aware `slice!`.  Falls back to the C-defined `slice!` (aliased as
  # `__slice_bang` above) for every other argument form.
  def slice!(*args)
    return __slice_bang(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    # Before the search, where `mrb_str_slice_bang()` and CRuby both put it:
    # a frozen receiver raises even for a pattern that would not have
    # matched, and `$~` is left as it was.  This is the opposite order from
    # `[]=` above, and both are observable.  `frozen?` is redefinable where
    # the C check is not, but no other route to that check leaves the string
    # alone on the way.
    raise FrozenError, "can't modify frozen String" if frozen?
    md = Regexp.__search(args[0], self)
    return nil unless md
    group = args.length > 1 ? args[1] : 0
    if Integer === group
      # Where `[]=` raises, `slice!` answers nil: an index that reaches no
      # group removed nothing.  The normalization is the same, so group 0
      # stays out of the negative end's reach here too.
      size = md.size
      return nil if group >= size || -group >= size
      group += size if group < 0
    end
    beg = md.begin(group)
    # CRuby answers "" for a group that exists but did not take part in the
    # match, and removes nothing.  That falls out of `rb_str_slice_bang()`
    # building the result from the group's -1 offset rather than out of a
    # decision, but it is what the method answers.
    return "" unless beg
    len = md.end(group) - beg
    # From the MatchData, whose subject is a snapshot taken before this
    # method mutates anything, and which is a plain String even when the
    # receiver is a subclass, both as in CRuby.
    removed = md[group]
    __aset(beg, len, "")
    removed
  end

  # Regexp-aware `index`.  Falls back to the C-defined `index` (aliased as
  # `__index` above) for every other argument form.
  def index(*args)
    return __index(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    # `Regexp.__search` normalizes a position the way `index` does and reads
    # it with the same `mrb_get_args()` conversion, so the argument goes over
    # unexamined: a negative one counts back from the end, and one that lands
    # outside the subject answers nil after clearing the match globals.
    # A full search and not `match?`, because those globals are part of the
    # answer.
    md = args.length > 1 ? Regexp.__search(args[0], self, args[1]) : Regexp.__search(args[0], self)
    # `begin` reports character offsets, which is the space `index` answers
    # in; `byteindex` below is the same search read in the other space.
    md && md.begin(0)
  end

  # Regexp-aware `rindex`.  Falls back to the C-defined `rindex` (aliased as
  # `__rindex` above) for every other argument form.
  def rindex(*args)
    return __rindex(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    len = self.length
    pos = len
    if args.length > 1
      # The position is arithmetic here rather than an argument handed
      # straight to the engine, so it has to be an Integer first.
      # `Integer.__ensure` is `mrb_ensure_int_type()`, the same conversion
      # `mrb_get_args()` performs for the `i` of the C method.
      pos = Integer.__ensure(args[1])
      if pos < 0
        pos += len
        # Out of the subject at the negative end is a miss, and a miss
        # clears the match globals.
        return Regexp.__search(args[0], nil) if pos < 0
      elsif pos > len
        # Past the other end is not: `rindex` searches back from the end of
        # the subject, and `mrb_str_byterindex_m()` clamps for the same
        # reason.  `"abc".rindex(/b/, 10)` is 1.
        pos = len
      end
    end
    # The search reads the subject by byte, so the character position it is
    # to stop at has to be read as one here.  A position at the end of the
    # subject is the end of its bytes and needs no reading, which is the form
    # `rindex` is called in when it is called with one argument at all.
    byte_pos = pos == len ? self.bytesize : self[0, pos].bytesize
    md = Regexp.__byte_rsearch(args[0], self, byte_pos)
    md && md.begin(0)
  end

  # Regexp-aware `byteindex`.  Falls back to the C-defined `byteindex`
  # (aliased as `__byteindex` above) for every other argument form.
  def byteindex(*args)
    return __byteindex(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    len = self.bytesize
    pos = 0
    if args.length > 1
      pos = Integer.__ensure(args[1])
      pos += len if pos < 0
    end
    # `__byte_search` takes the position as given and does not range check
    # it, where `Regexp.__search` answers nil for one outside the subject.
    # Both ends are a miss here, as they are for `mrb_str_byteindex_m()`.
    return Regexp.__search(args[0], nil) if pos < 0 || pos > len
    # An offset that lands inside a character names no position the subject
    # has, and the C method refuses one.  It is asked after the range test,
    # where the C method asks it too, so an offset outside the subject stays a
    # miss rather than becoming an error.
    Regexp.__check_byte_pos(self, pos)
    md = Regexp.__byte_search(args[0], self, pos)
    md && md.__byte_begin(0)
  end

  # Regexp-aware `byterindex`.  Falls back to the C-defined `byterindex`
  # (aliased as `__byterindex` above) for every other argument form.
  def byterindex(*args)
    return __byterindex(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    len = self.bytesize
    pos = len
    if args.length > 1
      pos = Integer.__ensure(args[1])
      if pos < 0
        pos += len
        return Regexp.__search(args[0], nil) if pos < 0
      elsif pos > len
        pos = len
      end
    end
    # As in `byteindex` above, and after the same clamp: a position past the
    # end of the subject has already been read as its end, which is a boundary.
    Regexp.__check_byte_pos(self, pos)
    md = Regexp.__byte_rsearch(args[0], self, pos)
    md && md.__byte_begin(0)
  end

  # Regexp-aware `partition`.  Falls back to the C-defined `partition`
  # (aliased as `__partition` above) for every other argument.
  def partition(sep)
    return __partition(sep) unless Regexp === sep
    md = Regexp.__search(sep, self)
    # No match leaves the whole subject in the head, and the copy is a plain
    # String even when the receiver is a subclass, as `mrb_str_dup()` and
    # CRuby's `str_duplicate(rb_cString, str)` both hand back.
    return [self.byteslice(0, self.bytesize), "", ""] unless md
    [md.pre_match, md[0], md.post_match]
  end

  # Regexp-aware `rpartition`.  Falls back to the C-defined `rpartition`
  # (aliased as `__rpartition` above) for every other argument.
  def rpartition(sep)
    return __rpartition(sep) unless Regexp === sep
    # The last match anywhere in the subject, so the limit is its end and the
    # search below never stops early.
    md = Regexp.__byte_rsearch(sep, self, self.bytesize)
    # No match puts the whole subject in the tail, which is the row this
    # method is most often got wrong on.
    return ["", "", self.byteslice(0, self.bytesize)] unless md
    [md.pre_match, md[0], md.post_match]
  end

  # Regexp-aware `start_with?`.  Takes any mix of patterns and hands each
  # non-regexp one to the C-defined `start_with?` (aliased as
  # `__start_with?` above), one at a time, so that a String keeps the C
  # comparison and its error and the arguments are still read left to right.
  def start_with?(*args)
    i = 0
    while i < args.length
      arg = args[i]
      if Regexp === arg
        # A regexp is anchored at the start, not searched for, while the
        # search runs forward from its position.  The engine matches
        # leftmost, so a pattern that can match at 0 does, which makes
        # `begin(0) == 0` the anchored answer rather than an approximation
        # of it.
        md = Regexp.__search(arg, self)
        return true if md && md.begin(0) == 0
        # A match further along is not an answer and CRuby leaves none
        # behind for one, so clear what the search published.  Searching
        # nil is how the globals are cleared.
        Regexp.__search(arg, nil) if md
      elsif __start_with?(arg)
        return true
      end
      i += 1
    end
    false
  end
end
