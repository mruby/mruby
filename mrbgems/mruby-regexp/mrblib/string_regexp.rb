# The overrides below take care not to let a pattern argument lie about its
# type, each in the way its own dispatch needs.  `match`, `match?`, `sub`,
# `sub!`, `gsub`, `gsub!` and `scan` hand the argument to
# `Regexp.__check_pattern`, which accepts a Regexp or a String and rejects
# everything else from C, so the argument cannot steer the decision and there
# is no Ruby-side helper for a subclass to redefine; an accepted String is
# compiled or quoted into a Regexp here before anything is searched.  `split`
# leaves a nil or String pattern to the built-in it aliased and uses the same
# check to reject everything that is not a Regexp.  `[]`, `[]=` and `slice!`
# read the real class with `Regexp === pattern` and leave anything else to the
# built-in method they aliased.  `=~` rejects a String, which would recurse
# back into this method, and hands anything else to the argument's own `=~`,
# as CRuby does.
#
# That is where the guarantee ends.  With the type established, each override
# calls ordinary methods on the pattern (`match`, `match?`, `=~`,
# `__byte_match`, `__sub_str`, `__gsub_str`, `__scan`) and on the MatchData it
# hands back (`[]`, `pre_match`, `post_match`, `begin`, `end`, `size`,
# `length`, `__byte_begin`, `__byte_end`, `__set_globals`).  Every one of
# those is defined with `mrb_define_method()`, so a singleton method on the
# pattern replaces it and the override follows the replacement; the `__`
# prefix is a naming convention, not a protection.
#
# CRuby is open the same way in `rb_str_match_m()`, which dispatches `match` to
# the pattern on purpose, and closed everywhere else: `rb_str_sub_bang()`,
# `rb_str_subpat()`, `rb_str_subpat_set()` and `rb_str_slice_bang()` call
# `rb_reg_search()` directly, and `String#match?` and `String#=~` search a real
# Regexp without asking it anything.  The overrides here dispatch on every
# path, so a rewritten pattern reaches results CRuby would not give it.
#
# The gem accepts that rather than closing it.  Reaching it takes rewriting a
# method on a Regexp instance and then passing that instance to a String
# method; nothing here is reachable from ordinary input, because an argument
# that is not a Regexp is rejected, compiled into a Regexp built here, or left
# to the built-in method the override aliased; `=~` alone forwards it to the
# argument, which CRuby does too.  The type checks in front of the searches
# are claims about the argument, not about a pattern its owner has rewritten.
class String
  # Capture the C-defined String#split under `__split` before the override
  # below replaces it, so the override can delegate non-regexp patterns
  # back to the core implementation.
  alias __split split

  # The same for String#[], whose regexp form is overridden at the end of
  # this file.  `slice` is a second method table entry for the same C
  # function rather than an alias of `[]`, so this one capture serves both.
  alias __aref []

  # The write side of the same pair, overridden at the end of this file too.
  # `[]=` has a single method table entry, and `slice!` comes from
  # mruby-string-ext, which this gem depends on, so it needs its own capture.
  alias __aset []=
  alias __slice_bang slice!

  # The four search methods of src/string.c whose regexp form is overridden at
  # the end of this file.  On a build without MRB_UTF8_STRING the two of a
  # pair are the same C function behind two method table entries, so each
  # still needs its own capture.
  alias __index index
  alias __rindex rindex
  alias __byteindex byteindex
  alias __byterindex byterindex

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

  def sub!(*args, &block)
    # The argument checks come before the frozen receiver, as in CRuby:
    # `"abc".freeze.sub!(/b/)` raises ArgumentError and
    # `"abc".freeze.sub!(:b, "X")` TypeError, while the two-argument form on
    # the same receiver raises FrozenError.  `gsub!` orders it the other way,
    # also as CRuby does.
    if block
      unless (1..2).include?(args.length)
        raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
      end
    elsif args.length != 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 2)"
    end
    # Resolved here rather than left to `sub` because the match below decides
    # the return value, and a String pattern is a literal on both paths.
    pattern = Regexp.__check_pattern(args[0])
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
    raise FrozenError, "can't modify frozen String" if frozen?
    # Whether a substitution happened is a question about the match, not about
    # the result: `"aaa".sub!(/a/, "a")` returns self even though the string is
    # unchanged.  `match` and not `match?`, so a failed match clears $~.
    return nil unless pattern.match(self)
    # `sub` matches again and publishes its own $~ over this one, leaving the
    # caller the match `sub` would have left, a block's own matches included.
    # The resolved pattern takes the place of the original argument so that a
    # String is not quoted and compiled a second time.  Overwriting `self`
    # afterwards is safe: a MatchData snapshots its subject, so $~ keeps
    # describing the string as it was matched.
    str = args.length == 2 ? self.sub(pattern, args[1], &block) : self.sub(pattern, &block)
    self.replace(str)
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
    # The loop normally ends on a failed __byte_match, which clears $~ and the
    # thirteen names that go with it. CRuby leaves the last match behind, so
    # keep it and republish it below. A gsub that matched nothing has nothing
    # to restore and keeps the cleared state, as CRuby does.
    last = nil
    while pos <= len
      md = pattern.__byte_match(self, pos)
      break unless md
      last = md
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
    last.__set_globals if last
    parts.join
  end

  def gsub!(*args, &block)
    # Before the arity check and before the enumerator below, as in CRuby:
    # `"abc".freeze.gsub!(/a/)` raises FrozenError rather than handing back an
    # Enumerator that fails later.
    raise FrozenError, "can't modify frozen String" if frozen?
    unless (1..2).include?(args.length)
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    return to_enum(:gsub!, *args) if args.length == 1 && !block
    pattern = Regexp.__check_pattern(args[0])
    pattern = Regexp.new(Regexp.escape(pattern)) if String === pattern
    # As in `sub!`: the match decides the return value, and `match` clears $~
    # when it fails.  What it publishes on success is replaced right away by
    # the last match of the `gsub` below, which is the one CRuby leaves behind.
    return nil unless pattern.match(self)
    str = args.length == 2 ? self.gsub(pattern, args[1], &block) : self.gsub(pattern, &block)
    self.replace(str)
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

  # Regexp-aware element reference.  Falls back to the C-defined `[]`
  # (aliased as `__aref` above) for every other argument form, and handles a
  # regexp here.
  #
  # `vm_op_getidx()` answers `str[Integer]`, `str[String]` and `str[Range]`
  # from C without consulting the method table, so those three keep bypassing
  # this override.  They are exactly the forms it would have handed back to
  # `__aref` unchanged, so they cost nothing and behave as before, while a
  # regexp index leaves the opcode through its fallback and arrives here as an
  # ordinary send.  `str[i, len]` and every `slice` call are not opcode
  # receivers and do arrive here, paying a Ruby frame on their way to
  # `__aref`.
  def [](*args)
    # Before any argument inspection, so that the non-regexp forms keep the
    # arity check `mrb_get_args()` does.  With no arguments at all `args[0]`
    # is nil, the guard fails, and `__aref()` raises the ArgumentError.
    # `is_a?` is redefinable, so a Regexp denying its own type would slip
    # past and fail in `__aref`; `Module#===` reads the real type.  What it
    # settles is which implementation answers, not what the pattern goes on to
    # decide once it is here; see the note at the top of this file.
    return __aref(*args) unless Regexp === args[0]
    if args.length > 2
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1..2)"
    end
    # `match` and not `match?`: the match globals have to be published here,
    # including the clearing a failed match does, and `match?` leaves them
    # alone.  That is why the MatchData is fetched even with no capture
    # argument, where only its `[0]` is used.
    md = args[0].match(self)
    return nil unless md
    # The capture argument reaches `MatchData#[]` untouched: it already
    # normalizes a negative index, answers nil for an index past the last
    # group and raises IndexError for a name that resolves to none, which is
    # what CRuby does for `str[re, capture]`.
    md[args.length > 1 ? args[1] : 0]
  end

  # `slice` is registered separately from `[]` rather than aliased to it, so
  # the override above would leave it on the C implementation.  This is also
  # what makes `sym[re]` work: `Symbol#[]` is an alias of `Symbol#slice`,
  # which delegates to `String#slice`.
  alias slice []

  # Regexp-aware element assignment.  Falls back to the C-defined `[]=`
  # (aliased as `__aset` above) for every other argument form, and handles a
  # regexp here.
  #
  # `vm_op_setidx()` optimizes Array and Hash only and sends `[]=` for every
  # other receiver, so unlike the read side there is no opcode keeping the
  # ordinary `str[i] = repl` off this override: it pays a Ruby frame on its
  # way to `__aset`.  That is why the delegation guard is a single
  # `Regexp ===`, before any other work.
  def []=(*args)
    return __aset(*args) unless Regexp === args[0]
    unless args.length == 2 || args.length == 3
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 2..3)"
    end
    # `match` and not `match?`, so that the match globals are published here
    # including the clearing a failed match does.  CRuby searches before it
    # checks the receiver for modification, which makes the order observable:
    # a frozen receiver still leaves the match behind, and a pattern that does
    # not match raises IndexError rather than FrozenError.  Letting the
    # mutation below be what raises reproduces both.
    md = args[0].match(self)
    raise IndexError, "regexp not matched" unless md
    group = args.length > 2 ? args[1] : 0
    if Integer === group
      # An index out of range is an error here, not a missing group, and
      # CRuby reports it before normalizing a negative one, so the message
      # names the index as given, and group 0 is out of the negative end's
      # reach.  `MatchData#begin` has its own wording for this and rejects
      # every negative index, so the check cannot be left to it.
      size = md.size
      if group >= size || -group >= size
        raise IndexError, "index #{group} out of regexp"
      end
      group += size if group < 0
    end
    # A String or Symbol reaches `MatchData#begin` as it stands: it resolves
    # the name to its group and raises the IndexError CRuby raises for a name
    # that resolves to none, with the same message.
    beg = md.begin(group)
    # A group that exists but did not take part in the match has nothing to
    # replace.  CRuby names the group's number even when the argument was a
    # name; the number is not reachable from Ruby, so the message repeats the
    # argument as it was given.
    raise IndexError, "regexp group #{group} not matched" unless beg
    # `begin` and `end` report character offsets, which is the space the
    # two-integer form of `[]=` works in, so a multibyte subject needs no
    # further conversion.  The replacement is handed over unchecked: the type
    # check belongs to the core method, as it does for `sub`.
    __aset(beg, md.end(group) - beg, args[-1])
  end

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
    md = args[0].match(self)
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
    # `Regexp#match` normalizes a position the way `index` does and reads it
    # with the same `mrb_get_args()` conversion, so the argument goes over
    # unexamined: a negative one counts back from the end, and one that lands
    # outside the subject answers nil after clearing the match globals.
    # `match` and not `match?`, because those globals are part of the answer.
    md = args.length > 1 ? args[0].match(self, args[1]) : args[0].match(self)
    # `begin` reports character offsets, which is the space `index` answers
    # in; `byteindex` below is the same search read in the other space.
    md && md.begin(0)
  end

  # The last match that starts at or before `limit`, or nil, with the match
  # globals left describing it.  `limit` is a byte offset when `bytes` is
  # true and a character offset otherwise, which is the whole of the
  # difference between `byterindex` and `rindex`.
  #
  # The engine searches forward only, so this walks the subject from the
  # start and keeps the last match that qualifies: linear in the number of
  # positions a match starts at, where the backward search CRuby hands to
  # Onig is not.  Each step resumes one character past the match start and
  # not at the match end, which is what keeps overlapping matches in view:
  # `"aaa".rindex(/aa/)` is 1, where resuming at the end would answer 0.
  def __regexp_rsearch(pattern, limit, bytes)
    found = nil
    pos = 0
    while (md = pattern.match(self, pos))
      break if (bytes ? md.__byte_begin(0) : md.begin(0)) > limit
      found = md
      pos = md.begin(0) + 1
    end
    # The loop leaves behind whatever its last `match` published: the clear a
    # failed one does, or a match past `limit` that is not the answer.  Both
    # have to give way to what the search found.
    found ? found.__set_globals : pattern.match(nil)
    found
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
        return args[0].match(nil) if pos < 0
      elsif pos > len
        # Past the other end is not: `rindex` searches back from the end of
        # the subject, and `mrb_str_byterindex_m()` clamps for the same
        # reason.  `"abc".rindex(/b/, 10)` is 1.
        pos = len
      end
    end
    md = __regexp_rsearch(args[0], pos, false)
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
    # `__byte_match` takes the position as given and does not range check it,
    # where `Regexp#match` answers nil for one outside the subject.  Both
    # ends are a miss here, as they are for `mrb_str_byteindex_m()`.  An
    # offset that lands inside a character is not an error: the C method does
    # not check for one either, and on a build without MRB_UTF8_STRING there
    # is nothing to check.
    return args[0].match(nil) if pos < 0 || pos > len
    md = args[0].__byte_match(self, pos)
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
        return args[0].match(nil) if pos < 0
      elsif pos > len
        pos = len
      end
    end
    md = __regexp_rsearch(args[0], pos, true)
    md && md.__byte_begin(0)
  end
end
