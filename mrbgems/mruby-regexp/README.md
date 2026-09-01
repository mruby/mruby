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
- `\g<name>`, `\g'name'` subexpression call, recursive where the call stands
  inside the group it names
- `\g<n>`, `\g<-n>`, `\g<+n>` the same by number, `\g<0>` the whole pattern
- `(?=...)` positive lookahead
- `(?!...)` negative lookahead
- `(?<=...)` positive lookbehind (fixed-length only)
- `(?<!...)` negative lookbehind (fixed-length only)
- `(?>...)` atomic group
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
re = Regexp.compile("pattern")    # Regexp.new under its other name
re = /pattern/i                   # literal syntax
re.match("string")                # => MatchData or nil
re.match("string", pos)           # => same, searching from pos
re.match("string") { |md| ... }   # => block result, or nil if no match
re.match?("string")               # => true/false
re.match?("string", pos)          # => same, searching from pos
re =~ "string"                    # => index or nil
re === "string"                   # => true/false (for case/when)
re.match(:symbol)                 # a Symbol is matched against its name
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
  quantifiers, lookaround, atomic groups or subexpression calls. O(pattern x
  text), so it is immune to ReDoS.
- **Backtracking engine** for the rest, whose state the Pike VM's threads have
  no stack to hold. It backtracks on a stack of its own on the heap, so a
  search spends a constant amount of C stack however long the subject is.
  Bounded by `MRB_REGEXP_STEP_LIMIT` and `MRB_REGEXP_STACK_LIMIT`.

## Limitations

Every entry is a place this engine answers a pattern differently from CRuby.

- **UTF-8 only where the build reads it**: a pattern and a subject read the way
  the build's `String` reads them. Without `MRB_UTF8_STRING` both are bytes:
  `/./` matches one byte, `/Ā/` is two atoms of one byte each, and `/i` folds
  ASCII only. A binary (`ASCII-8BIT`) subject reads by byte on either build.
- **Fixed-length lookbehind only**: `(?<=...)` and `(?<!...)` take no `*`, `+`,
  `?` or alternation, and at most 255 bytes. A call inside one must be
  fixed-length too, recursion included, or `invalid pattern in look-behind`.
- **No Unicode properties**: `\p{Alpha}`, `\p{L}` raise `RegexpError`, inside a
  character class as much as outside one. A bare `\p`, and `\pL`, is the
  letter. `[[:alpha:]]` asks for a letter of any script.
- **No `\M-X` meta escape**: it always raises `RegexpError`, where CRuby
  refuses it only outside a binary pattern.
- **A `[` inside a class opens something**: only a POSIX bracket is read there.
  A collating element (`[[.a.]]`), an equivalence class (`[[=a=]]`) and a
  nested class (`[[a][b]]`) raise `RegexpError`. Write `[\[]`.
- **No `\G`, `\K`, `\R` or `\X`**: they raise `RegexpError` rather than
  standing for their own letter. Inside a character class each is the letter,
  and so is a bare `\g` either way.
- **No nest level on a backreference**: `\k<name+n>` and `\k<name-n>` ask for a
  capture memory per call level where this engine keeps one flat slot per
  group, so they raise `RegexpError`. A plain `\k<name>` still works inside a
  recursion, reading the pair the innermost completed invocation left.
- **An empty iteration ends a repeat around a call too**, which is the rule
  every inline repeat here follows. Onigmo switches such repeats to a
  capture-tracking empty check that answers a few of them differently, among
  them `/((?<g1>|){2}b){2}\g<g1>{0}/` and `/(?<g1>)b\g<g1>{1,3}?/`.
- **No character class intersection**: `[a&&b]` raises `RegexpError`. A lone
  `&` is a member of the class.
- **No encodings**: a byte that starts no whole character is that byte, inside
  a character class as much as outside one. `[\xB5]` and `\xB5` both hold the
  byte `0xB5` and neither matches `µ` (`C2 B5`), where CRuby raises
  `RegexpError` for either spelling. A range whose ends are a byte and a
  character (`[\x80-µ]`) raises `RegexpError`.
- **Case folding follows the build**: where the build converts case by ASCII, a
  pattern holding a character that needs a Unicode folding raises
  `RegexpError`; see Configuration.
- **Case-insensitive backreferences match a superset**: `\1` under `i` folds
  each side and compares, so it matches across a width change (`k` and `K`)
  where CRuby declines to.
- **`\b` reads `[[:word:]]` below U+0100 too**: CRuby draws a boundary beside
  `²`, `³`, `¹` and `½`, reading a character under 256 off a Latin-1 word table
  for the boundary and off the Unicode tables for the bracket. Here the two are
  the same question at every codepoint, so neither takes them.
- **Backward search walks forward**: `rindex`, `byterindex` and `rpartition`
  walk the subject from the start and keep the last match that qualifies, so
  the cost grows with the number of positions a match starts at.

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

/* How deep a pattern may nest, the parser's own C stack (1 to 1,048,576) */
#ifndef MRB_REGEXP_PARSE_DEPTH_LIMIT
#define MRB_REGEXP_PARSE_DEPTH_LIMIT 4096
#endif
```

A build that sets a limit outside its range fails to compile, and so does one
still defining `MRB_REGEXP_RECURSION_LIMIT`, the name the stack limit had while
it counted C frames. The values a build chose are `Regexp::STEP_LIMIT`,
`Regexp::STACK_LIMIT` and `Regexp::PARSE_DEPTH_LIMIT`, for a program that has
to size a subject or a pattern to the build it runs on; CRuby's counterpart to
the two search limits is `Regexp.timeout`.

### The two search limits

A search that reaches one raises `RegexpError`, `step limit over
(MRB_REGEXP_STEP_LIMIT)` or `stack limit over (MRB_REGEXP_STACK_LIMIT)`, rather
than answer with what it had found by then. One whose stack the allocator
refuses to grow raises `NoMemoryError` instead, that being a different thing to
do something about.

The step limit bounds the work one search may do; the stack limit bounds the
state it holds while doing it, the branches it has not taken yet and the writes
it has not taken back, an entry each. A write of the value already in the slot
leaves nothing to take back and is not counted.

The stack limit counts live entries and not bytes, and so bounds the memory one
search may ask for: two arrays of at most `MRB_REGEXP_STACK_LIMIT` entries, an
entry being 32 and 16 bytes on a 64-bit ABI and 24 and 8 on a 32-bit one, so 96
KiB together at the default and 64 KiB respectively. Halving the limit halves
that ceiling. Lowering it costs patterns as well as buying memory: the gem's
tests ask for 48, which is where every pattern they take for granted still
matches, and the assertions that reach the engine skip below it.

### The parse depth limit

`MRB_REGEXP_PARSE_DEPTH_LIMIT` bounds the compiler rather than a search, and
guards the C stack rather than memory or work. A pattern past it raises
`RegexpError`, `parse depth limit over`, which is CRuby's message for the same
refusal. Every construct that opens a level costs one: a group of any kind, a
lookaround, an atomic group, and an inline option toggle, which encloses the
rest of the group it stands in. The default is Onigmo's
`ONIG_MAX_PARSE_DEPTH`, so the refusal point is CRuby's.

**A build on a stack smaller than about 3 MiB has to lower it.** The parser
recurses once per level at some 600 bytes a level on a 64-bit build, so the
deepest pattern the default accepts spends about 2.4 MiB. The compiler is not
the only thing standing on that stack, so a third of it is the share to size
the limit from:

| Stack   | A limit that fits           |
| ------- | --------------------------- |
| 8 MiB   | 4096 (default, CRuby-exact) |
| 1 MiB   | 512                         |
| 256 KiB | 128                         |
| 64 KiB  | 32                          |

Divide the build's own figure, not this one: `-Os` and a 32-bit ABI both make a
level cheaper, and `-fstack-usage` over `re_compile.c` names it, the frames of
`compile_alt` and `compile_seq` summed. Lowering it costs little, since nesting
this deep is not what a written pattern does: a build that sets 128 still takes
every pattern anyone writes, and gives up only the CRuby-exact refusal point.

### What the build decides

Case folding beyond ASCII and what a POSIX bracket holds above it are not this
gem's to configure. Both need a build that defines `MRB_UTF8_STRING` without
`MRB_USE_ASCII_CTYPE`.

`/i` reads the two directions it needs off core's case table, the one
`String#downcase` and the four case methods beside it read, so it folds what
the build's own case conversion folds. There `/Ā/i` matches `"ā"`, `/Σ/i`
matches `"σ"`, and `[^Ā]` under `/i` stops accepting `"ā"`. Where the build
converts case by ASCII those same patterns do not compile:

```ruby
/Ā/i     # RegexpError: /i needs Unicode case folding for this character
```

The test is whether a character has a folding, not whether it is non-ASCII, so
a script without case is unaffected and `/日本/i`, `/العربية/i` and `/😀/i` go
on working. The codepoints that do have one are held as ranges, and those
ranges are coarse: the uncased codepoints inside them are refused with the
rest, `ƻ` (U+01BB) among them. A codepoint with no single counterpart to fold
to (`ﬀ` to `ff`) is never folded by either build. `/k/i` matching `"K"`
(U+212A) and `/s/i` matching `"ſ"` need no table and work on both, though a
class holding the letter only through `\w`, `[:word:]` or `[:ascii:]` does not
reach them, those being sets ASCII defines.

The POSIX brackets read this gem's own table, `re_ctype.h`, 13.9KB of read-only
data that `MRB_USE_ASCII_CTYPE` leaves out. With it the brackets classify as
CRuby's do: `[[:alpha:]]` holds `"あ"` and `[[:^alpha:]]` rejects it,
`[[:upper:]]` under `/i` reaches `"ā"` through `"Ā"`, and `[[:word:]]` holds
every Unicode word character where `\w` stays ASCII. `\b` and `\B` read the
bracket's set rather than the shorthand's, so a boundary sits beside a
character of any script. The types are the ones the Unicode Character Database
publishes: `alpha`, `upper` and `lower` are the derived properties Alphabetic,
Uppercase and Lowercase, `space` is White_Space, and the rest are read off the
general categories. Without the table a bracket holds its ASCII and no more, so
`[[:alpha:]]` misses `"あ"`; `[[:xdigit:]]` and `[[:ascii:]]` are sets ASCII
defines and hold nothing above it on any build.

## Checking against CRuby

The Limitations list is kept by hand. What keeps it honest is `tools/difftest`,
which runs a corpus through both engines and reports where they disagree:

```console
$ rake regexp:difftest
5175 patterns, 101 known differences, no new ones
```

The task asks the build the loaded config declares, and leaves the choice to
`MRUBY` where a config declares more than one build with this gem in it.

`probe.rb` holds the corpus and runs under either engine, asking along two
axes. The **pattern axis** prints a line per pattern: where a match starts in
each of a fixed list of subjects, what it captured, and which class it raised,
the class being part of the answer whether the pattern was refused at compile
time or the search raised against a subject. The patterns are generated from
the axes rather than listed, so an escape, a quantifier or a class form is
covered in every context it can stand in, each asked under `//`, `/i`, `/m` and
`/x`. The **character axis** turns that around: a line per character, a column
per way of classifying one. The characters come out of the Unicode Character
Database by the rule `tools/unicode/corpus_data.rb` states, one out of every
class the engine's tables tell apart, where a class is the whole signature and
not one property at a time, plus the whole of ASCII.

`compare.rb` checks the disagreements against `baseline.txt`, which holds the
ones that are meant, every line in it being one of the limitations above. It
fails on a disagreement the baseline does not hold and on a baseline line that
has stopped disagreeing, so that a fix prunes the list rather than leaving it to
describe an engine that has moved on. `rake regexp:difftest:update` takes a new
baseline, and `rake regexp:difftest:selftest` puts those verdicts to
`compare.rb` with answers made up rather than run. Both `probe.rb` and
`corpus_data.rb` assert their shape rather than their size, so a corpus that has
quietly shrunk stops the run instead of passing by not looking.

Two things bound what the tool can say, and `compare.rb` refuses rather than
reporting either as a regression: a CRuby carrying an older Unicode release than
the tables were generated from (the check is `\p{Age=}`, and CRuby 4.0 is the
first that passes it), and a baseline taken against a build that reads or
classifies its strings differently. Both are why this is a task to run rather
than a job in the workflows, which use whatever CRuby a runner ships.

## License

MIT License. See the mruby license file for details.

This gem is an original implementation. No external regexp library
code is used.
