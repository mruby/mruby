# mruby-regexp

Built-in regular expression engine for mruby using a Pike VM (NFA
simulation) with backtracking fallback.

## Features

### Pattern Syntax

- `.` any character (except newline by default)
- `*`, `+`, `?` greedy quantifiers
- `*?`, `+?`, `??`, `{n,m}?` non-greedy quantifiers
- `*+`, `++`, `?+` possessive quantifiers, `a*+` being `(?>a*)`
- `{n}`, `{n,}`, `{n,m}` repetition counts
- `[abc]`, `[a-z]`, `[^abc]` character classes
- `[[a]b]`, `[[^a]b]` nested character classes, joined as a union
- `[a-z&&[^aeiou]]` character class intersection
- `[[:alpha:]]`, `[[:^alpha:]]` POSIX brackets inside a class: `alpha`,
  `digit`, `alnum`, `upper`, `lower`, `space`, `blank`, `xdigit`, `word`,
  `cntrl`, `print`, `graph`, `ascii`, `punct`
- `\d`, `\w`, `\s` digit, word, whitespace shortcuts, ASCII as in CRuby
- `\D`, `\W`, `\S` negated shortcuts
- `\h`, `\H` hex digit and non-hex-digit shortcuts
- `(...)` capture group
- `(?:...)` non-capturing group
- `(?#...)` comment group
- `(?<name>...)`, `(?'name'...)` named capture group
- `|` alternation
- `\N` numbered backreference
- `\k<name>`, `\k'name'` named backreference
- `\k<n>`, `\k'n'` numbered backreference
- `\k<-n>`, `\k'-n'` relative backreference
- `\g<name>`, `\g'name'` subexpression call, recursive inside the group it
  names
- `\g<n>`, `\g<-n>`, `\g<+n>` the same by number, `\g<0>` the whole pattern
- `(?=...)` positive lookahead
- `(?!...)` negative lookahead
- `(?<=...)` positive lookbehind (fixed-length, per branch)
- `(?<!...)` negative lookbehind (fixed-length, per branch)
- `(?>...)` atomic group
- `(?~...)` absent repeater
- `(?(n)yes|no)`, `(?(<name>)yes|no)`, `(?('name')yes|no)` conditional: `yes`
  where the group has matched, `no` where it has not, `no` optional
- `(?imx-imx)` options for the rest of the enclosing group
- `(?imx-imx:...)` options for the group's own body

Three rules settle what a spelling means:

- A quantifier after a quantifier repeats the repeat: `a**` is `(?:a*)*` and
  `a{2}{3}` is `(?:a{2}){3}`. `{n}` has no non-greedy form, so `a{3}?` matches
  empty where the lazy `a{3,3}?` does not.
- `\N` is a backreference where its decimal value is at most 9 or at most the
  number of groups opened before it, and an octal escape past both. A number
  matching no group anywhere in the pattern raises `RegexpError` at compile
  time, so `\1(a)` is valid because group 1 appears later.
- A recursion no input could end (`(?<a>\g<a>)`) raises `RegexpError` at
  compile time, as in CRuby. One that can end is bounded at match time by
  `MRB_REGEXP_STACK_LIMIT`.

### Character Escapes

- `\n`, `\t`, `\r`, `\f`, `\v`, `\a`, `\e` control characters
- `\b` backspace inside a character class, where the word boundary cannot stand
- `\NNN` octal, one to three digits, where the digits spell no backreference
- `\xHH` hex, one or two digits; `\x` with no digit raises `RegexpError`
- `\cX`, `\C-X` control characters, `\c?` being DEL as it is in a String
- `\uXXXX` Unicode codepoint, exactly four hex digits
- `\u{...}` Unicode codepoints, one to six hex digits each, space separated

Outside a character class the list form is a sequence rather than one atom, so
`/\u{61 62}+/` is `ab+`. Inside a class every codepoint is a member of its own
and the one next to a `-` bounds the range, so `/[\u{61 62}-z]/` is `a` plus
`b-z`. A range written backwards raises `RegexpError`, as in CRuby.

### Anchors

- `^` beginning of line
- `$` end of line
- `\A` beginning of string
- `\z` end of string
- `\Z` end of string (or before final newline)
- `\b` word boundary, beside the word characters `[[:word:]]` holds
- `\B` non-word boundary

### Flags

- `i` (`Regexp::IGNORECASE`) case-insensitive matching
- `m` (`Regexp::MULTILINE`) `.` matches newline, `^`/`$` match at line
  boundaries
- `x` (`Regexp::EXTENDED`) free-spacing mode, `#` starts a comment

### Ruby API

```ruby
# Regexp
re = Regexp.new("pattern", Regexp::IGNORECASE)
re = Regexp.new("pattern", "im")  # the same options as letters
re = Regexp.new(other)            # a copy of another Regexp
re = Regexp.compile("pattern")    # Regexp.new under its other name
re = /pattern/i                   # literal syntax
re.match("string")                # => MatchData or nil
re.match("string", pos)           # => same, searching from pos
re.match("string") { |md| ... }   # => block result, or nil if no match
re.match?("string")               # => true/false
re.match?("string", pos)          # => same, searching from pos
re =~ "string"                    # => index or nil
re === "string"                   # => true/false (for case/when)
re.match(:symbol)                 # a Symbol is matched against its name, here
                                  #    and in match?, =~ and ===
re.source                         # => "pattern"
re.options                        # => flags integer
re.casefold?                      # => true where the pattern carries /i
re.named_captures                 # => {"name" => [group_number], ...}
re.names                          # => ["name", ...]
re.to_s                           # => "(?i-mx:pattern)"
re.inspect                        # => "/pattern/i"
re == other                       # => true where source and options agree
re.eql?(other)                    # => same as ==
re.hash                           # => source and options hashed together
Regexp.escape("a.b")              # => "a\\.b"
Regexp.quote("a.b")               # => same as Regexp.escape
Regexp.union("a+", /b/i)          # => /a\+|(?i-mx:b)/
Regexp.union(["a", "b"])          # => /a|b/, one Array stands for its elements
Regexp.union                      # => /(?!)/, which never matches
Regexp.last_match(n)              # => nth capture from last match

# MatchData
md = /(\w+)@(\w+)/.match("user@host")
md[0]                             # => "user@host" (full match)
md[1]                             # => "user"
md[2]                             # => "host"
md[:name]                         # named capture access
md[0, 2]                          # => ["user@host", "user"]
md[0..1]                          # => same as md[0, 2]
md.captures                       # => ["user", "host"]
md.values_at(1, 2)                # => ["user", "host"]
md.to_a                           # => ["user@host", "user", "host"]
md.to_s                           # => "user@host" (same as md[0])
md.size                           # => group count, group 0 included
md.length                         # => same as size
md.begin(0)                       # => match start position
md.end(0)                         # => match end position
md.offset(0)                      # => [begin, end] of the same group
md.pre_match                      # => string before match
md.post_match                     # => string after match
md.named_captures                 # => {"name" => "value", ...}
md.names                          # => ["name", ...]
md.string                         # => the subject the match ran against
md.regexp                         # => the Regexp that matched
md.inspect                        # => '#<MatchData "user@host" 1:"user"
                                  #    2:"host">', groups by number or name

# String methods
str.match(re)                     # => MatchData or nil
str.match(re, pos)                # => same, searching from pos
str.match(re) { |md| ... }        # => block result, or nil if no match
str.match?(re)                    # => true/false
str.match?(re, pos)               # => same, searching from pos
str =~ re                         # => index or nil
str.sub(re, replacement)          # replace first occurrence
str.sub(re) { |m| ... }           # replace with block
str.gsub(re, replacement)         # replace all occurrences
str.gsub(re) { |m| ... }          # replace all with block
str.sub!(re, replacement)         # => self, or nil if no match
str.sub!(re) { |m| ... }          # same, replacing with the block result
str.gsub!(re, replacement)        # => self, or nil if no match
str.gsub!(re) { |m| ... }         # same, replacing with the block result
str.scan(re)                      # => array of matches
str.split(re)                     # => array of parts
str.split(re, limit)              # => same, at most limit parts; a negative
                                  #    limit keeps the trailing empty ones
str[re]                           # => matched substring or nil
str[re, capture]                  # => capture by index or name
str.slice(re)                     # => same as str[re]
str[re] = repl                    # replace the match
str[re, capture] = repl           # replace a capture by index or name
str.slice!(re)                    # remove and return the match, or nil
str.slice!(re, capture)           # same, for a capture by index or name
str.index(re)                     # => match start, or nil
str.index(re, pos)                # => same, searching from pos
str.rindex(re)                    # => last match start, or nil
str.rindex(re, pos)               # => last match starting at or before pos
str.byteindex(re)                 # => match start in bytes, or nil
str.byteindex(re, pos)            # => same, searching from byte pos
str.byterindex(re)                # => last match start in bytes, or nil
str.byterindex(re, pos)           # => same, at or before byte pos
str.partition(re)                 # => [before, match, after]
str.rpartition(re)                # => [before, last match, after]
str.start_with?(re)               # => true/false (anchored at the start)
str.start_with?(re, "s", ...)     # => true where any one of them does

# Symbol methods (the String methods applied to the symbol's name)
sym.match(re)                     # => MatchData or nil
sym.match(re, pos)                # => same, searching from pos
sym.match(re) { |md| ... }        # => block result, or nil if no match
sym.match?(re)                    # => true/false
sym.match?(re, pos)               # => same, searching from pos
sym =~ re                         # => index or nil
sym[re]                           # => matched substring or nil
                                  #    (Symbol#[] comes from mruby-symbol-ext)

# Global variables
$~                                # last MatchData
$&, $`, $', $+, $1, $2, ...       # read from $~ at the moment they are read
                                  #    (all nil while $~ is nil)
```

### Named Captures

As in CRuby, a named group anywhere in a pattern renumbers the whole of it: a
plain `(...)` groups without capturing, and a numbered backreference raises
`RegexpError` in every spelling (`\1`, `\k<1>`, `\k<-1>`). Refer to a group by
name instead. A pattern with no named group numbers its groups as usual, and
`\1`-`\9` work there.

```ruby
md = /(?<a>a)(b)/.match("ab")
md.size                          # => 2
md.captures                      # => ["a"]
md[:a]                           # => "a"
md[2]                            # => nil

"aa".match(/(?<n>\w)\k<n>/)[0]   # => "aa"

Regexp.new("(a)(?<b>b)\\1")
# RegexpError: numbered backref/call is not allowed. (use name)
```

## Engine Architecture

Two engines, chosen automatically at compile time by pattern analysis.

- **Pike VM (NFA simulation)** for patterns without backreferences, non-greedy
  quantifiers, lookaround, atomic groups, absent repeaters, conditionals or
  subexpression calls. O(pattern x text), so it is immune to ReDoS. The branch
  a fork leaves for later waits on a stack the search owns, so a step spends a
  constant amount of C stack however often the pattern forks.
- **Backtracking engine** for the rest, whose state the Pike VM's threads have
  no stack to hold. It backtracks on a heap stack of its own, so a search
  spends a constant amount of C stack however long the subject is. Bounded by
  `MRB_REGEXP_STEP_LIMIT` and `MRB_REGEXP_STACK_LIMIT`.

## Limitations

Every entry is a place this engine answers a pattern differently from CRuby.

- **UTF-8 only where the build reads it**: without `MRB_UTF8_STRING` a pattern
  and a subject are bytes: `/./` matches one byte, `/Ā/` is two atoms, and
  `/i` folds ASCII only. A binary (`ASCII-8BIT`) subject is bytes on either
  build.
- **Fixed-length lookbehind only**: `(?<=...)` and `(?<!...)` take no
  quantifier and no lookaround (`invalid pattern in look-behind`) and are at
  most 255 bytes wide (`lookbehind too long (max 255 bytes)`). Each branch has
  its own width, as in CRuby. A call narrows the whole body to one width, so
  `(?<=\g<1>|zz)(a)` raises where CRuby accepts it, and an option construct
  before the alternation does not enclose it here, so `(?<=(?i:ab|b))x` and
  `(?<=(?i)ab|b)x` are accepted where CRuby raises.
- **No Unicode properties**: `\p{...}` raises `RegexpError` inside a class or
  outside one. A bare `\p` or `\pL` is the letter. `[[:alpha:]]` matches a
  letter of any script.
- **No `\M-X`**: always `RegexpError`; CRuby refuses it only outside a binary
  pattern.
- **No `(?a)`, `(?d)` or `(?u)`**: `undefined group option`, in the toggle and
  the scoped form alike. CRuby's `(?u)` widens `\d`, `\s` and `\w` to Unicode
  and its `(?a)` narrows the POSIX brackets and `\b` to ASCII; here nothing in
  a pattern changes what those hold.
- **A `[` inside a class opens something**: a POSIX bracket or a nested class.
  `[[.a.]]` and `[[=a=]]` raise `RegexpError`; write `[\[]` for the bracket.
  `[a-[b]]` raises where CRuby reads the class holding `b` alone. A negated
  nest holding a bracket type beside another type or a member is refused, so
  `[[^[:alpha:][:digit:]]x]` and `[[^[:alpha:]é]x]` raise where CRuby accepts
  them; `[[^[:alpha:]]x]` is `[[:^alpha:]x]` in both.
- **No `\G`, `\K`, `\R` or `\X`**: `RegexpError` rather than the letter. Inside
  a class each is the letter, as is a bare `\g` anywhere.
- **No nest level on a backreference**: `\k<name+n>`, `\k<name-n>` and
  `(?(<name+n>)...)` raise `RegexpError`, the engine keeping one slot per
  group. A plain `\k<name>` or `(?(<name>)...)` inside a recursion reads what
  the innermost completed invocation left.
- **A named pattern refuses every numbered condition**: CRuby refuses
  `(?(1)...)` there but accepts `(?(<1>)...)`, `(?(<-1>)...)` and
  `(?('1')...)`. Here all four are refused, as `\k<1>` is.
- **A conditional's body is its own**: `(?(1)(?:b|c))` is `yes` = `(?:b|c)`
  with no `no`. CRuby reads it as `(?(1)b|c)` and refuses `(?(1)(?:b|c|d))` as
  three bodies. `(?(1)(?:b|c)x)` and `(?(1)(?i:b|c))` read alike in both.
- **An empty iteration ends a repeat around a call too**, as it ends every
  repeat here. Onigmo's capture-tracking empty check answers a few
  differently, among them `/((?<g1>|){2}b){2}\g<g1>{0}/` and
  `/(?<g1>)b\g<g1>{1,3}?/`.
- **An absent repeater's body captures nothing**: a group inside `(?~...)` is
  left as the match found it. CRuby keeps what the body wrote where Onigmo has
  no restore for the group, so `(a)` in `/(?~(a)(b))/` holds `"a"` there
  against a subject starting with one; `/(?~(a|b)+)/` leaves the group empty
  in both.
- **An intersection asks one question about a character's type**: a class
  carries one disjunction of POSIX brackets and one conjunction of them.
  `[[:alpha:][:digit:]&&[:alnum:][:space:]]` and
  `[[[:alpha:]&&[:^lower:]][:digit:]]` need more and raise `RegexpError`;
  CRuby accepts them. `[[:alpha:]&&[:^lower:]&&[:^upper:]]` is read.
- **Under `/i` an intersection folds the class it made, not its operands**:
  what an ASCII-only set brought is closed within ASCII, so `[s&&\w]` holds
  `s` and `S`, and a character above ASCII folds whatever admitted it, so
  `[\u{100}-\u{200}&&\W]` holds `s` through `ſ`. CRuby folds a single
  character where it stands and holds back a fold that a set ASCII defines
  admitted: there `[s&&\w]` holds `ſ`, `[s-t&&\w]` does not, and
  `[\u{100}-\u{200}&&\W]` misses `s`.
- **A negated shorthand keeps its ASCII in an intersection**: `[[^\W]&&[^a]]`
  is the ASCII word characters without `a`; CRuby reads `[^\W]` as the Unicode
  word characters once it stands in an intersection.
- **No encodings**: a byte that starts no character is that byte. `[\xB5]` and
  `\xB5` hold `0xB5` and neither matches `µ` (`C2 B5`); CRuby raises
  `RegexpError` for either. `[\x80-µ]` raises here.
- **Case folding follows the build**: where the build converts case by ASCII, a
  pattern holding a character that needs a Unicode folding raises
  `RegexpError`; see Configuration.
- **Case-insensitive backreferences match a superset**: `\1` under `i` folds
  each side, so it matches across a width change (`k` and `K`) where CRuby
  does not.
- **`\b` reads `[[:word:]]` below U+0100 too**: CRuby draws a boundary beside
  `²`, `³`, `¹` and `½`, reading a Latin-1 word table for the boundary and the
  Unicode tables for the bracket. Here both are one question, so neither takes
  them.
- **Backward search walks forward**: `rindex`, `byterindex` and `rpartition`
  scan from the start and keep the last match, so the cost grows with the
  number of positions a match starts at.

## Configuration

```c
/* Steps one backtracking search may take (ReDoS protection) */
#ifndef MRB_REGEXP_STEP_LIMIT
#define MRB_REGEXP_STEP_LIMIT 1000000
#endif

/* Entries on the backtracking engine's heap stack (1 to 16,777,216) */
#ifndef MRB_REGEXP_STACK_LIMIT
#define MRB_REGEXP_STACK_LIMIT 2048
#endif

/* How deep a pattern may nest (1 to 1,048,576) */
#ifndef MRB_REGEXP_PARSE_DEPTH_LIMIT
#define MRB_REGEXP_PARSE_DEPTH_LIMIT 4096
#endif
```

A limit outside its range fails the build, and so does
`MRB_REGEXP_RECURSION_LIMIT`, the stack limit's old name. The values are
`Regexp::STEP_LIMIT`, `Regexp::STACK_LIMIT` and `Regexp::PARSE_DEPTH_LIMIT`;
CRuby's counterpart to the first two is `Regexp.timeout`.

### The two search limits

A search that reaches one raises `RegexpError`, `step limit over
(MRB_REGEXP_STEP_LIMIT)` or `stack limit over (MRB_REGEXP_STACK_LIMIT)`; a
stack the allocator refuses to grow raises `NoMemoryError`. The step limit
bounds the work of one search and the stack limit the state it holds, one entry
per branch not yet taken and per capture write not yet undone (a write of the
value already there is not counted). An entry is 32 and 16 bytes on a 64-bit
ABI and 24 and 8 on a 32-bit one, so the default caps a search at 96 KiB or 64
KiB, and halving the limit halves that. The gem's tests need 48 and skip the
assertions that reach the engine below it.

### The parse depth limit

`MRB_REGEXP_PARSE_DEPTH_LIMIT` bounds how deep a pattern may nest. A pattern
past it raises `RegexpError`, `parse depth limit over`, CRuby's message. Every
group, lookaround, atomic group, inline option toggle and nested character
class costs a level. The default is Onigmo's `ONIG_MAX_PARSE_DEPTH`, so the
refusal point is CRuby's.

The parser keeps the levels it has open on a heap stack of its own, as the
backtracking engine keeps its state, so a compile spends a constant amount of C
stack however deep the pattern nests. The limit is a matter of agreement with
CRuby rather than of stack, and one value holds for every build.

What the limit costs is heap, and only while a pattern that deep compiles: a
level is 32 bytes on any ABI, the first eight (the pattern itself is one) live
in the compiler's own frame, and past them the levels move to a buffer that
doubles as it grows. Raising the limit reserves nothing; it bounds what one
pattern can be made to use, at the limit rounded up to a power of two times 32
bytes, plus one String header:

| Limit          | Heap a pattern can spend |
| -------------- | ------------------------ |
| 4096 (default) | 128 KiB                  |
| 512            | 16 KiB                   |
| 128            | 4 KiB                    |
| 32             | 1 KiB                    |

A build that cannot make the allocation raises `NoMemoryError`. A class written
inside a class is a level on a stack of its own, held the same way, and the
class table bounds those as well: a level open holds an entry in it, and the
257th is `too many character classes`. The shallower of the two answers, so at
the default limit a class nests 256 deep and at a limit of 256 or less it nests
as deep as the limit.

The passes that read the finished program keep their branches on stacks of
their own as well: the anchor and first-byte scans, the marks on the
repetitions that can run empty, and the width of a lookbehind. What a compile
spends on the C stack follows neither the pattern's length nor how often it
forks.

### What the build decides

Case folding beyond ASCII and what a POSIX bracket holds above it need
`MRB_UTF8_STRING` without `MRB_USE_ASCII_CTYPE`.

`/i` reads core's case table, the one `String#downcase` reads, so where the
build folds Unicode `/Ā/i` matches `"ā"` and `[^Ā]` under `/i` stops accepting
`"ā"`. Where it folds ASCII only, such a pattern does not compile:

```ruby
/Ā/i     # RegexpError: /i needs Unicode case folding for this character
```

The test is whether the character has a folding, so `/日本/i` and `/😀/i` keep
working. The cased codepoints are held as coarse ranges, so an uncased
codepoint inside one (`ƻ`, U+01BB) is refused too. A codepoint with no single
counterpart to fold to (`ﬀ`) is folded by neither build. `/k/i` matching `"K"`
(U+212A) and `/s/i` matching `"ſ"` need no table, though `\w`, `[:word:]` and
`[:ascii:]` do not reach them.

The POSIX brackets read `re_ctype.h`, 13.9KB of read-only data that
`MRB_USE_ASCII_CTYPE` leaves out. With it `[[:alpha:]]` holds `"あ"`,
`[[:upper:]]` under `/i` reaches `"ā"`, `[[:word:]]` holds every Unicode word
character where `\w` stays ASCII, and `\b` sits beside a character of any
script. `alpha`, `upper` and `lower` are the derived properties Alphabetic,
Uppercase and Lowercase, `space` is White_Space, and the rest come from the
general categories. Without the table a bracket holds its ASCII; `[[:xdigit:]]`
and `[[:ascii:]]` are ASCII on any build.

## Checking against CRuby

The Limitations list is kept by hand; `tools/difftest` keeps it honest:

```console
$ MRUBY_CONFIG=host-debug rake regexp:difftest
6791 patterns, 353 known differences, no new ones
```

The baseline was taken against `build_config/host-debug.rb`, whose full-core
gembox reads strings as UTF-8 and classifies them by Unicode; the default
config has no `mruby-encoding`, so its build is refused against it.

`probe.rb` runs a corpus under either engine and prints, per pattern, where a
match starts in each of a fixed list of subjects, what it captured and which
class it raised, and per character, a column per way of classifying it. The
patterns are generated from their axes, every escape, quantifier and class
form in every context it can stand in, under `//`, `/i`, `/m` and `/x`; the
characters come out of the Unicode Character Database by the rule in
`tools/unicode/corpus_data.rb`, one per class the engine's tables tell apart,
plus ASCII. `compare.rb` diffs the two runs against `baseline.txt`, the
differences that are meant, and fails on a new one or on a line that has
stopped differing, so a fix prunes the list. `rake regexp:difftest:update`
takes a new baseline and `rake regexp:difftest:selftest` feeds `compare.rb`
made-up verdicts. The corpus asserts its shape rather than its size, so one
that has quietly shrunk stops the run.

`compare.rb` refuses a CRuby with an older Unicode than the tables (checked
with `\p{Age=}`; 4.0 is the first that passes) and a baseline taken against a
build that reads or classifies strings differently, which is why this is a task
to run rather than a workflow job. The task uses the build the loaded config
declares; with more than one, name it with `MRUBY`.

## License

MIT License. See the mruby license file for details.

This gem is an original implementation. No external regexp library
code is used.
