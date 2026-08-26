# mruby-regexp

Built-in regular expression engine for mruby using a Pike VM (NFA
simulation) with backtracking fallback.

## Features

### Pattern Syntax

- `.` any character (except newline by default)
- `*`, `+`, `?` greedy quantifiers
- `*?`, `+?`, `??` non-greedy quantifiers
- `*+`, `++`, `?+` possessive quantifiers, `a*+` being `(?>a*)`
- `{n}`, `{n,}`, `{n,m}` repetition counts
- a quantifier after a quantifier repeats the repeat: `a**` is `(?:a*)*` and
  `a{2}{3}` is `(?:a{2}){3}`. `{n}` has no non-greedy form, so its `?` is a
  quantifier too and `a{3}?` matches empty where the lazy `a{3,3}?` does not
- `[abc]`, `[a-z]`, `[^abc]` character classes
- `[[:alpha:]]`, `[[:^alpha:]]` POSIX brackets inside a class: `alpha`,
  `digit`, `alnum`, `upper`, `lower`, `space`, `blank`, `xdigit`, `word`,
  `cntrl`, `print`, `graph`, `ascii` and `punct`. Above ASCII each holds
  what CRuby's does where the build classifies characters by Unicode, and
  nothing where it does not; see Configuration
- `\d`, `\w`, `\s` digit, word, whitespace shortcuts, ASCII as in CRuby
- `\D`, `\W`, `\S` negated shortcuts
- `(...)` capture group
- `(?:...)` non-capturing group
- `(?#...)` comment group
- `(?<name>...)`, `(?'name'...)` named capture group
- `|` alternation
- `\N` backreference: a digit run whose decimal value is at most 9 or at
  most the number of groups opened before it; a run past both is an octal
  escape (see below). A reference naming a group the pattern does not have
  raises `RegexpError`, counting the groups of the whole pattern, so `\1(a)`
  is valid
- `\k<name>`, `\k'name'` named backreferences
- `\k<n>`, `\k'n'` numbered backreferences
- `\k<-n>`, `\k'-n'` relative backreferences
- `\g<name>`, `\g'name'` subexpression calls: the group's sub-pattern runs
  again where the call stands, recursively when the call stands inside it,
  so `(?<p>\((?:[^()]|\g<p>)*\))` matches balanced parentheses. `\g<n>` is
  absolute, `\g<-n>` counts back over the groups already opened, `\g<+n>`
  counts forward, and `\g<0>` is the whole pattern. A recursion no input
  could end (`(?<a>\g<a>)`, `(?<a>x\g<a>)`) raises `RegexpError` at compile
  time, as in CRuby; the depth of one that can end is bounded at match time
  by `MRB_REGEXP_STACK_LIMIT`, the call frames living on the backtracking
  stack it counts
- `(?=...)` positive lookahead
- `(?!...)` negative lookahead
- `(?<=...)` positive lookbehind (fixed-length only)
- `(?<!...)` negative lookbehind (fixed-length only)
- `(?>...)` atomic group
- `(?imx-imx)` options for the rest of the enclosing group,
  `(?imx-imx:...)` options for the group's own body

### Character Escapes

- `\n`, `\t`, `\r`, `\f`, `\v`, `\a`, `\e` control characters
- `\NNN` octal, one to three digits, when the digits spell no
  backreference: `\101` is `A`, `\12` is a newline before twelve groups
  and a backreference after them, `\0NN` is always octal; `\8` and `\9`
  that spell no backreference are the digits themselves
- `\xHH` hex, one or two digits; `\x` with no digit raises `RegexpError`
- `\cX`, `\C-X` control characters, where a `\` in the X position opens an
  escape of its own (`\c\n`). `\c?` is DEL, as it is in a String
- `\uXXXX` Unicode codepoint, exactly four hex digits
- `\u{...}` Unicode codepoints, one to six hex digits each, several of
  them separated by spaces: `/\u{61 62}/` is `ab`

Outside a character class the list form is a sequence rather than one
atom, so a quantifier after it repeats the last codepoint only:
`/\u{61 62}+/` is `ab+`. Inside a class every codepoint is a member of
its own, and the one next to a `-` still opens or closes a range: the
last of the list before it and the first after it, so `/[\u{61 62}-z]/`
is `a` plus `b-z` and `/[a-\u{63 7a}]/` is `a-c` plus `z`. A range
written backwards, `[b-a]` or `[b-\u{61 63}]`, raises `RegexpError` as
in CRuby.

### Anchors

- `^` beginning of line
- `$` end of line
- `\A` beginning of string
- `\z` end of string
- `\Z` end of string (or before final newline)
- `\b` word boundary, beside the word characters `[[:word:]]` holds
- `\B` non-word boundary

### Flags

- `i` (`Regexp::IGNORECASE`) case-insensitive matching (Unicode, or ASCII
  where the build converts case by ASCII)
- `m` (`Regexp::MULTILINE`) `.` matches newline; `^`/`$` match at line boundaries
- `x` (`Regexp::EXTENDED`) free-spacing mode; unescaped whitespace ignored, `#` starts comments

### Ruby API

```ruby
# Regexp
re = Regexp.new("pattern", Regexp::IGNORECASE)
re = /pattern/i                   # literal syntax
re.match("string")                # => MatchData or nil
re.match("string") { |md| ... }   # => block result, or nil if no match
re.match?("string")               # => true/false
re =~ "string"                    # => index or nil
re === "string"                   # => true/false (for case/when)
re.match(:symbol)                 # a Symbol is matched against its name
re.source                         # => "pattern"
re.options                        # => flags integer
re.named_captures                 # => {"name" => [group_number], ...}
re.names                          # => ["name", ...]
Regexp.escape("a.b")              # => "a\\.b"
Regexp.last_match(n)              # => nth capture from last match

# MatchData
md = /(\w+)@(\w+)/.match("user@host")
md[0]                             # => "user@host" (full match)
md[1]                             # => "user"
md[2]                             # => "host"
md[:name]                         # named capture access
md.captures                       # => ["user", "host"]
md.to_a                           # => ["user@host", "user", "host"]
md.begin(0)                       # => match start position
md.end(0)                         # => match end position
md.pre_match                      # => string before match
md.post_match                     # => string after match
md.named_captures                 # => {"name" => "value", ...}
md.names                          # => ["name", ...]

# String methods
str.match(re)                     # => MatchData or nil
str.match(re) { |md| ... }        # => block result, or nil if no match
str.match?(re)                    # => true/false
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

# Symbol methods (the String methods applied to the symbol's name)
sym.match(re)                     # => MatchData or nil
sym.match(re) { |md| ... }        # => block result, or nil if no match
sym.match?(re)                    # => true/false
sym =~ re                         # => index or nil
sym[re]                           # => matched substring or nil
                                  #    (Symbol#[] comes from mruby-symbol-ext)

# Global variables
$~                                # last MatchData
$&, $`, $', $+, $1, $2, ...       # read from $~ at the moment they are read
                                  #    (all nil while $~ is nil)
```

## Engine Architecture

The gem uses two execution engines:

**Pike VM (NFA simulation)**: Used for patterns without
backreferences, non-greedy quantifiers, lookaround, atomic groups or
subexpression calls. Guarantees O(pattern x text) time complexity, making it
immune to ReDoS attacks.

**Backtracking engine**: Used when patterns contain `\1`-`\9`
backreferences, non-greedy quantifiers (`*?`, `+?`, `??`),
lookaround assertions (`(?=...)`, `(?!...)`, `(?<=...)`, `(?<!...)`),
atomic groups (`(?>...)`) or subexpression calls (`\g<name>`), whose call
frames the Pike VM's threads have no stack to hold -- a call declines the
Pike path the way a backreference does. It backtracks on a stack of its own on the
heap, so a search spends a constant amount of C stack however long the
subject is. Bounded by a configurable step limit (`MRB_REGEXP_STEP_LIMIT`,
default 1M) against excessive backtracking and by a stack limit
(`MRB_REGEXP_STACK_LIMIT`, default 2048) on how tall that stack may
stand. A search that reaches either raises `RegexpError` naming the limit,
since what it had found by then is not the answer; one whose stack the
allocator refuses to grow raises `NoMemoryError` instead, that being a
different thing to do something about.

The engine is selected automatically at compile time based on
pattern analysis.

## Limitations

- **UTF-8 only where the build reads it**: the engine reads a pattern and a
  subject the way the build's `String` reads them, so everything below about
  characters holds on a build that defines `MRB_UTF8_STRING` (mruby-encoding
  is what defines it). Where it is not defined a string is bytes and so is the
  engine: `/./` matches one byte, `/Ā/` is two atoms of one byte each, and
  `/i` folds ASCII letters and nothing else. A binary (`ASCII-8BIT`) subject
  reads by byte on either build.
- **Fixed-length lookbehind only**: `(?<=...)` and `(?<!...)`
  require a fixed-length pattern (no `*`, `+`, `?`, or alternation).
  Maximum 255 bytes.
- **No Unicode properties**: `\p{Alpha}`, `\p{L}`, etc. are not
  supported and raise `RegexpError`, inside a character class as much as
  outside one. It is the braces that name a property: a bare `\p`, and `\pL`
  as well, is the letter, which is how CRuby reads them too. The POSIX
  brackets read the same data where the build carries it, so `[[:alpha:]]` is
  the way to ask for a letter of any script.
- **A set is not an end of a range**: a shorthand (`\d`, `\w`, ...) and a
  POSIX bracket each name a set rather than a character, so `[a-\d]` and
  `[\d-z]` raise `RegexpError` as they do in CRuby. A `-` at either edge of
  the class is still a member: `[\d-]` holds the digits and the dash.
- **No `\M-X` meta escape**: it sets the high bit, making a byte that starts
  no character, and there is no encoding here to read one against. It raises
  `RegexpError`, as it does in CRuby for a pattern that is not binary.
- **A `[` inside a class opens something**: as in CRuby it never stands for
  itself, and what it opens is read here only when it is a POSIX bracket.
  A collating element (`[[.a.]]`), an equivalence class (`[[=a=]]`) and a
  class nested in this one (`[[a][b]]`) each raise `RegexpError`. Write
  `[\[]` for the bracket itself, which is the spelling CRuby wants too.
- **No `\G`, `\K`, `\R` or `\X`**: the search-start anchor, the match-start
  reset, the linebreak and the grapheme cluster all raise `RegexpError`
  rather than standing for their own letter. Inside a character class CRuby
  reads each as the letter, and so does this; a bare `\g`, which calls a
  group only with a `<name>` or `'name'` after it, is the letter either way.
- **No nest level on a backreference**: `\k<name+n>` and `\k<name-n>` read the
  group as the enclosing recursion left it `n` levels up, which asks for a
  capture memory per call level where this engine keeps one flat slot per
  group, so they raise `RegexpError`. A plain `\k<name>` still works inside a
  recursion: it reads the pair the innermost completed invocation left, and
  reads the group as unmatched inside an invocation no inner one has
  completed within, which are CRuby's answers for the same shapes. The level
  starts at the first `+`
  or `-` past the name's first byte, in CRuby as here, which leaves a group
  whose name holds one out of a backreference's reach: `(?<a-1>x)` names a
  group in both, and `\k<a-1>` reaches it in neither, though `\g<a-1>`, a
  call taking no level, reaches it in both. The first byte is the relative
  form's sign, so `\k<-1>` is still the group one back.
- **A call in a lookbehind must still be fixed-length**: `(?<=\g<a>)` runs
  where the group's body is fixed-length, and raises the lookbehind's own
  `lookbehind must be fixed length` where it is not -- recursion included --
  where CRuby says `invalid pattern in look-behind`.
- **An empty iteration ends a repeat around a call too**: an iteration that
  matched empty ends the repetition and keeps what it captured, which is the
  rule every inline repeat here follows, and a repeat whose body runs
  through a called group follows it unchanged. Onigmo switches such repeats
  to a capture-tracking empty check that can lose matches its own inline
  rule finds: `/((?<g1>|){2}b){2}\g<g1>{0}/` answers nil in CRuby although
  the dead call never runs, and `/(?<g1>)b\g<g1>{1,3}?/` answers nil where
  its greedy spelling matches `"b"`. This engine answers every such pattern
  as the call-free spelling does.
- **No character class intersection**: `[a&&b]` narrows a class to what both
  sides hold in CRuby, and raises `RegexpError` here. A lone `&` is a member
  of the class, as it is in CRuby, and so is an escaped one: `[\&&]` holds
  `&` twice rather than intersecting.
- **No `\x{...}` hex escape**: the hex escape is `\xHH`, so it reaches
  `0xff` at most, and `\x{...}` raises `RegexpError` as CRuby does, since
  the brace is not a hex digit. Write `\u{...}` for a codepoint above that.
- **No encodings**: a pattern is a byte string read the way the build reads a
  String, and there is no encoding to consult about a byte that starts no
  whole character. Such a byte is that byte, inside a character class as much
  as outside one: `[\xB5]` and `\xB5` both hold the byte `0xB5`, and neither
  matches `µ`, which is `C2 B5`. CRuby settles the same question with the
  pattern's encoding and raises `RegexpError` for either spelling. A range
  whose ends are a byte and a character (`[\x80-µ]`) names neither and raises
  `RegexpError`.
- **Case folding follows the build**: The `i` flag reads the Unicode
  foldings that pair one codepoint with one other off core's case table,
  which a build converting case by ASCII does not carry. There `i` folds
  ASCII letters, and a pattern holding a character that needs one of those
  foldings raises `RegexpError` rather than answering as if the character had
  no case. What it refuses is held as ranges, which take in some uncased
  characters as well; see Configuration. A codepoint with no single
  counterpart to fold to (`ﬀ` to `ff`) is never folded by either build.
- **Case-insensitive backreferences match a superset**: `\1` under `i`
  folds each side and compares, so it matches where the capture and the
  repeat hold the same characters in different widths (`k` and `K`).
  CRuby declines to fold across a width change there.
- **`\b` reads `[[:word:]]` below U+0100 too**: CRuby draws a word boundary
  beside `²`, `³`, `¹` and `½`, which its own `[[:word:]]` does not hold; it
  reads a character under 256 off a Latin-1 word table for the boundary and
  off the Unicode tables for the bracket. Here the two are the same question
  at every codepoint, so neither takes them. The difference is CRuby's own
  rather than a rule Ruby states.
- **Step limit on backtracking**: Patterns that require the
  backtracking engine are subject to a step limit.
- **Backward search walks forward**: the engine searches forward only,
  so `rindex`, `byterindex` and `rpartition` walk the subject from the
  start and keep the last match that qualifies. The cost grows with the
  number of positions a match starts at, where CRuby hands the search to
  Onig.

## Named Captures

As in CRuby, declaring a named group anywhere in a pattern changes how the
whole pattern is numbered: a plain `(...)` groups without capturing, and a
numbered backreference is a `RegexpError` in every spelling (`\1`, `\k<1>`,
`\k<-1>`). Refer to a group by name instead.

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

A pattern with no named group numbers its groups as usual, and `\1`-`\9` work
there.

## Configuration

```c
/* Maximum step count for backtracking engine (ReDoS protection) */
#ifndef MRB_REGEXP_STEP_LIMIT
#define MRB_REGEXP_STEP_LIMIT 1000000
#endif

/* Maximum height of the backtracking engine's stack (heap) */
#ifndef MRB_REGEXP_STACK_LIMIT
#define MRB_REGEXP_STACK_LIMIT 2048
#endif

/* How deep a pattern may nest (the parser's own C stack) */
#ifndef MRB_REGEXP_PARSE_DEPTH_LIMIT
#define MRB_REGEXP_PARSE_DEPTH_LIMIT 4096
#endif
```

A search that reaches either limit raises `RegexpError`, `step limit over
(MRB_REGEXP_STEP_LIMIT)` or `stack limit over (MRB_REGEXP_STACK_LIMIT)`,
rather than answer with what it had found by then. The step limit bounds
the work one search may do; the stack limit bounds the state it holds while
doing it: the branches it has not taken yet and the writes it has not taken
back, an entry each. A write of the value already in the slot leaves nothing
to take back and is not counted. What a repetition spends per iteration is
what it holds: one choice point where it captures nothing, and undo records
on top of that for a capture (up to two writes to open a group and one to
close it, an iteration that opens one the attempt has not closed yet paying
for one of the two) and for the record of an iteration that may match empty.
A run longer than the limit reaches it on a pattern that is not pathological;
a build with the memory for it can set it higher, and one that wants a
smaller ceiling can set it lower.

The default is where the state moving off the C stack costs no pattern the
subject it used to match, and no higher. The limit that preceded it counted
C frames, and a frame is not an entry: a fork was one frame and is one
choice point, while a capture was one frame and is up to three undo records.
Of the shapes measured across that change the tightest is `(a)*?b`, which
crossed 498 characters on the old 1,000 frames and crosses 682 on 2,048
entries; a chain of atomic groups or of lookarounds, which spent two frames
a link and now spends none once each has closed, is bounded by the pattern
rather than by this limit either way.

The limit stands between 1 and 16,777,216, and a build that sets it outside
that fails to compile: at 0 no search could hold one entry, and above the
ceiling the arithmetic that sizes the arrays stops holding on a 32-bit ABI.
A low limit is a build's to choose, and what it buys is memory at the price
of the patterns the engine will match: the gem's tests ask for 48, which is
where every pattern they take for granted matches, and the assertions that
reach the engine skip below it while the rest go on running. The two limits
are set apart from one another as well: a build that turns this one up far
enough puts it out of the step limit's reach, since filling the stack costs
a handful of steps an entry, and the tests that pin the stack limit size
their subjects from it are skipped there. The values a build chose are
`Regexp::STACK_LIMIT` and `Regexp::STEP_LIMIT`, for a program that has to
size a subject or a pattern to the build it runs on; CRuby has no
counterpart, its guard being `Regexp.timeout`.

What the stack limit counts is live entries and not bytes. Two arrays hold
them, one for the branches and one for the writes, and they grow
geometrically and keep their capacity for the rest of the search, so a search
that fills one, backtracks, and then fills the other holds both high-water
marks at once. Neither is grown past the limit, so the memory one search may
ask for is bounded by it: at most `MRB_REGEXP_STACK_LIMIT` entries in each
array, an entry being 32 and 16 bytes respectively on a 64-bit ABI and 24 and
8 on a 32-bit one, so 96 KiB together at the default on a 64-bit build and
64 KiB on a 32-bit one. Halving the limit halves that ceiling. The capture
slots and the per-instruction iteration records a search also holds are sized
by the pattern rather than by this limit.

A search whose stack the allocator refuses to grow raises `NoMemoryError`
rather than `RegexpError`. The two are worth telling apart: a limit names the
knob to turn, where turning `MRB_REGEXP_STACK_LIMIT` up in answer to an
allocator that had nothing left would only let the next search ask for more.

The macro was called `MRB_REGEXP_RECURSION_LIMIT` while the engine recursed
once per fork and the limit counted C frames. A build that still defines that
name fails to compile: the two limits count different things, so an old value
does not carry over and the build has to choose a new one.

`MRB_REGEXP_PARSE_DEPTH_LIMIT` is the third limit and the odd one out: it
bounds the compiler rather than a search, and what it guards is the C stack
rather than memory or work. A pattern past it raises `RegexpError`, `parse
depth limit over`, which is CRuby's message for the same refusal.

Every construct that opens a level costs one: a group of any kind, a
lookaround, an atomic group, and an inline option toggle, which encloses the
rest of the group it stands in and so is a level of its own. A pattern of 4096
nested `(?:` and one of 4096 nested `(?i)` are refused alike, at the same
depth CRuby refuses them: the default is Onigmo's `ONIG_MAX_PARSE_DEPTH`.

**A build on a stack smaller than about 3 MiB has to lower it.** The parser
recurses once per level, so the ceiling is a property of the build's stack
rather than of the engine, and the default is chosen for the refusal point
rather than for the stack. A level costs about 600 bytes on a 64-bit build, so
the deepest pattern the default accepts spends some 2.4 MiB, and so does a
deeper one before being refused, since the count is reached at the bottom of
the recursion. Where the stack cannot pay that, the crash the limit exists to
prevent comes back. The compiler is not the only thing standing on that stack
either, so a third of it is the share to size the limit from:

| Stack   | A limit that fits           |
| ------- | --------------------------- |
| 8 MiB   | 4096 (default, CRuby-exact) |
| 1 MiB   | 512                         |
| 256 KiB | 128                         |
| 64 KiB  | 32                          |

Divide the build's own figure, not this one: `-Os` and a 32-bit ABI both make
a level cheaper, and `-fstack-usage` over `re_compile.c` names it, the frames
of `compile_alt` and `compile_seq` summed and the rest inlining into them (560
bytes on a 64-bit gcc `-O2` build, 528 at `-Os`, 592 on a clang `-O3` one).

Lowering it costs little. Nesting this deep is not what a written pattern
does, a handful of levels being ordinary and dozens unusual, so a build that
sets 128 still takes every pattern anyone writes, and gives up only the
CRuby-exact refusal point. The limit stands between 1 and 1,048,576, and a
build that sets it outside that fails to compile.

Case folding beyond ASCII is not this gem's to configure. The table is
core's, carried by any build that defines `MRB_UTF8_STRING` without
`MRB_USE_ASCII_CTYPE`, and is what `String#downcase` and the four case methods
beside it read; `/i` reads the two directions it needs over that same table.
So `/i` folds what the build's own case conversion folds, and a build
converting case by ASCII has nothing for it to fold beyond ASCII either,
whether the conversion was narrowed there or the strings are read as bytes and
hold no character to fold in the first place.

Where the build converts case by Unicode, `/Ā/i` matches `"ā"`, `/Σ/i` matches
`"σ"`, and `[^Ā]` under `/i` stops accepting `"ā"`.

Where it converts by ASCII, those same patterns do not compile:

```ruby
/Ā/i     # RegexpError: /i needs Unicode case folding for this character
```

The test is whether a character has a case folding, not whether it is
non-ASCII, so a script without case is unaffected and `/日本/i`, `/العربية/i`
and `/😀/i` go on working. The codepoints that do have one are held as ranges
rather than one by one, and those ranges are coarse: the uncased codepoints
inside them are refused with the rest, `ƻ` (U+01BB) among them.
Patterns like `/Ā/i` were answering wrongly rather than narrowly before this:
`[Ā]` under `/i` missed `"ā"`, and `[^Ā]` accepted it. Reaching this error
means the pattern wants a build that converts case by Unicode.

`/k/i` matching `"K"` (U+212A) and `/s/i` matching `"ſ"` need no table.
Those two are the only foldings whose result is an ASCII letter, and both
builds carry them, so that folding "ASCII only" covers the whole of the
equivalence class an ASCII letter belongs to rather than the part of it that
is ASCII. A class holding the letter only through `\w`, `[:word:]` or
`[:ascii:]` does not reach them: those are sets ASCII defines, so `[\w]`
under `/i` stays the ASCII word characters and `[^\w]` accepts `"K"` (U+212A),
as in CRuby. A letter written out beside the shorthand (`[\ws]`) folds as usual.

What a POSIX bracket holds above ASCII is this gem's table, `re_ctype.h`,
carried on the same condition as the case table: a build that defines
`MRB_UTF8_STRING` without `MRB_USE_ASCII_CTYPE`. There the brackets classify
as CRuby's do, `[[:alpha:]]` holding `"あ"` and `[[:^alpha:]]` rejecting it,
`[[:upper:]]` under `/i` reaching `"ā"` through `"Ā"`, and `[[:word:]]` every
Unicode word character where `\w` stays ASCII. `\b` and `\B` read the
bracket's set rather than the shorthand's, and so sit beside a character of
any script; without the table they read as `[[:word:]]` does on such a build,
which is the ASCII word characters and no more. The two are not two answers
to one question: a class can be asked for another way, so the shorthand keeps
the set CRuby gives it, while a boundary is the one thing a pattern cannot
spell another way, and takes the set that is useful. The types are the ones the
Unicode Character Database publishes: `alpha`, `upper` and `lower` are the
derived properties Alphabetic, Uppercase and Lowercase, `space` is White_Space,
and the rest are read off the general categories. Without the table a bracket
holds its ASCII and no character above it, so `[[:alpha:]]` misses `"あ"` and
`[[:^alpha:]]` takes it, and a build reading its strings by byte answers the
same, having no character to classify. `[[:xdigit:]]` and `[[:ascii:]]` are
sets ASCII defines and hold nothing above it on any build. The table is 13.9KB
of read-only data; `MRB_USE_ASCII_CTYPE` is what leaves it out.

## Checking against CRuby

Everything under Limitations is a place this engine answers a pattern
differently from CRuby, and the list is kept by hand. What keeps it honest is
`tools/difftest`, which runs a corpus through both engines and reports where
they disagree:

```console
$ rake regexp:difftest
5175 patterns, 101 known differences, no new ones
```

The task asks the build the loaded config declares, so a working tree holding
the builds of several configs still asks the one in hand. A config declaring
more than one build with this gem in it leaves the choice to `MRUBY`, since a
baseline describes one build.

`probe.rb` holds the corpus and runs under either engine. It asks along two
axes, since a pattern and a character are worth asking about differently.

The **pattern axis** prints a line per pattern: where a match starts in each of
a fixed list of subjects, what it captured, and which class it raised. The
class is part of the answer whether the pattern was refused at compile time or
the search raised against a subject, since refusing with `RegexpError` and
refusing with `ArgumentError` are different answers. The patterns are generated
from the axes rather than listed, so an escape, a quantifier or a class form is
covered in every context it can stand in — every printable ASCII character
after a backslash, alone and beside a literal and inside a class and as an end
of a range; every quantifier on every kind of atom; the groups, the anchors,
the backreferences and the POSIX brackets. Each is asked under `//`, `/i`,
`/m` and `/x`, one flag at a time rather than once per combination of them:
asked once under all eight, no pattern differed under a combination that did
not already differ under a single flag. The Unicode properties are four
patterns rather than an axis, since this engine refuses `\p{...}` outright and
asking it about every property would write the same refusal down once per
property; what the four are for is the day it stops refusing, when the
difference stops being one and the line goes GONE.

The **character axis** turns that around: a line per character, a column per
way of classifying one — the POSIX brackets and their complements, the
shorthands, the boundaries, `.`, and a few of them under `i`. The characters
come out of the Unicode Character Database, by the rule
`tools/unicode/corpus_data.rb` states and written out as `corpus.rb` by `rake
unicode:generate` with the tables, so a version bump moves the questions with
the answers.

What that rule takes is one character out of every class the engine's tables
tell apart, where a class is a whole signature and not one property at a time:
the general category, the script, which POSIX types hold the character, the
shape its case folding takes, and the width the encoding spells it in. Taking a
representative per category and another per script separately would leave the
combinations of them unasked, and it is a combination that the tables can get
wrong. Each is the lowest codepoint of its signature, which is the member that
does not move as Unicode grows, and the whole of ASCII is taken as well.

Picking by rule rather than by hand is what found the `\b` difference above. A
list written by hand reaches for `Ā` and `ā` to stand for "a letter with a
case", and those are U+0100 and U+0101 — one codepoint above the Latin-1 table
where CRuby answers by other rules.

`compare.rb` runs it in both and checks the disagreements against
`baseline.txt`, which holds the ones that are meant: a construct this engine
refuses rather than answers wrongly, a byte CRuby settles with the pattern's
encoding, a boundary the two draw off different tables. Every line in it is one
of the limitations above. A disagreement that is not in the baseline is what
the tool is for, and it fails on one; a baseline line that has stopped
disagreeing fails too, so that a fix prunes the list rather than leaving it to
describe an engine that has moved on. `rake regexp:difftest:update` takes a new
baseline.

Those three verdicts are the whole of what the tool reports, so
`rake regexp:difftest:selftest` puts them to `compare.rb` with answers made up
rather than run: a difference the baseline does not hold, one it holds
differently, one it holds that has been fixed, and the two cases it has to stay
quiet about. It runs at the start of every comparison as well, since a mistake
there is a differential test that passes by not looking. So does the reading of
the probe's own output, which is the same hazard a line further back: what
`probe.rb` writes is a protocol, three tab separated fields per answer and one
`#build` line, and a line of another shape, a label answered twice or a run
that never said which build it is are each read as a probe to fix rather than
taken for an answer.

The same task asks the corpus whether it still holds what it says it does. A
differential test goes quiet two ways, and the second one reads exactly like
the first: fewer patterns, no disagreement, green. So `probe.rb` asserts its
shape rather than its size, every escape in every context and every quantifier
on every atom, with a case out of each axis that is not a product named
besides; and `corpus_data.rb` reads the corpus back to check that every
signature the database has really has a character standing for it. Both stop
the run rather than the count going down unremarked.

Two things bound what it can say, and `compare.rb` refuses rather than
reporting either as a regression. The characters are chosen out of the Unicode
release the tables are generated from, so a CRuby carrying an older one would
call a character it has not heard of nothing at all; the check is `\p{Age=}`
and CRuby 4.0 is the first that passes it. And a baseline describes the build
it was taken against, since a build reading its strings as bytes or classifying
them by ASCII answers differently wherever a table is read.

Both are why this is a task to run rather than a job in the workflows, which
use whatever CRuby a runner ships.

## License

MIT License. See the mruby license file for details.

This gem is an original implementation. No external regexp library
code is used.
