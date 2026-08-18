# mruby-regexp

Built-in regular expression engine for mruby using a Pike VM (NFA
simulation) with backtracking fallback.

## Features

### Pattern Syntax

- `.` any character (except newline by default)
- `*`, `+`, `?` greedy quantifiers
- `*?`, `+?`, `??` non-greedy quantifiers
- `{n}`, `{n,}`, `{n,m}` repetition counts
- `[abc]`, `[a-z]`, `[^abc]` character classes
- `\d`, `\w`, `\s` digit, word, whitespace shortcuts
- `\D`, `\W`, `\S` negated shortcuts
- `(...)` capture group
- `(?:...)` non-capturing group
- `(?#...)` comment group
- `(?<name>...)`, `(?'name'...)` named capture group
- `|` alternation
- `\1`-`\9` backreferences
- `\k<name>`, `\k'name'` named backreferences
- `(?=...)` positive lookahead
- `(?!...)` negative lookahead
- `(?<=...)` positive lookbehind (fixed-length only)
- `(?<!...)` negative lookbehind (fixed-length only)
- `(?>...)` atomic group
- `(?imx-imx)` options for the rest of the enclosing group,
  `(?imx-imx:...)` options for the group's own body

### Character Escapes

- `\n`, `\t`, `\r`, `\f`, `\v`, `\a`, `\e` control characters
- `\NNN` octal, one to three digits
- `\xHH` hex, one or two digits
- `\uXXXX` Unicode codepoint, exactly four hex digits
- `\u{...}` Unicode codepoints, one to six hex digits each, several of
  them separated by spaces: `/\u{61 62}/` is `ab`

Outside a character class the list form is a sequence rather than one
atom, so a quantifier after it repeats the last codepoint only:
`/\u{61 62}+/` is `ab+`. Inside a class every codepoint is a member of
its own, and the last one can still open a range: `/[\u{61 62}-z]/` is
`a` plus `b-z`.

### Anchors

- `^` beginning of line
- `$` end of line
- `\A` beginning of string
- `\z` end of string
- `\Z` end of string (or before final newline)
- `\b` word boundary
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
```

## Engine Architecture

The gem uses two execution engines:

**Pike VM (NFA simulation)**: Used for patterns without
backreferences, non-greedy quantifiers, lookaround or atomic groups.
Guarantees O(pattern x text) time complexity, making it immune to
ReDoS attacks.

**Backtracking engine**: Used when patterns contain `\1`-`\9`
backreferences, non-greedy quantifiers (`*?`, `+?`, `??`),
lookaround assertions (`(?=...)`, `(?!...)`, `(?<=...)`, `(?<!...)`)
or atomic groups (`(?>...)`). Protected by a
configurable step limit (`MRB_REGEXP_STEP_LIMIT`, default 1M) to
prevent excessive backtracking.

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
  supported.
- **No `\x{...}` hex escape**: the hex escape is `\xHH`, so it reaches
  `0xff` at most. Write `\u{...}` for a codepoint above that.
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
```

Case folding beyond ASCII is not this gem's to configure. The table is
core's, carried by any build that defines `MRB_UTF8_STRING` without
`MRB_USE_ASCII_CASE`, and is what `String#downcase` and the four case methods
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
is ASCII.

## License

MIT License. See the mruby license file for details.

This gem is an original implementation. No external regexp library
code is used.
