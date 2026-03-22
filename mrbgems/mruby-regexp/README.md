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
- `(?<name>...)` named capture group
- `|` alternation
- `\1`-`\9` backreferences
- `(?=...)` positive lookahead
- `(?!...)` negative lookahead
- `(?<=...)` positive lookbehind (fixed-length only)
- `(?<!...)` negative lookbehind (fixed-length only)

### Anchors

- `^` beginning of line
- `$` end of line
- `\A` beginning of string
- `\z` end of string
- `\Z` end of string (or before final newline)
- `\b` word boundary
- `\B` non-word boundary

### Flags

- `i` (`Regexp::IGNORECASE`) case-insensitive matching (ASCII)
- `m` (`Regexp::MULTILINE`) `.` matches newline; `^`/`$` match at line boundaries
- `x` (`Regexp::EXTENDED`) free-spacing mode; unescaped whitespace ignored, `#` starts comments

### Ruby API

```ruby
# Regexp
re = Regexp.new("pattern", Regexp::IGNORECASE)
re = /pattern/i                   # literal syntax
re.match("string")                # => MatchData or nil
re.match?("string")               # => true/false
re =~ "string"                    # => index or nil
re === "string"                   # => true/false (for case/when)
re.source                         # => "pattern"
re.options                        # => flags integer
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

# String methods
str.match(re)                     # => MatchData or nil
str.match?(re)                    # => true/false
str =~ re                         # => index or nil
str.sub(re, replacement)          # replace first occurrence
str.sub(re) { |m| ... }           # replace with block
str.gsub(re, replacement)         # replace all occurrences
str.gsub(re) { |m| ... }          # replace all with block
str.scan(re)                      # => array of matches
str.split(re)                     # => array of parts

# Global variables
$~                                # last MatchData
```

## Engine Architecture

The gem uses two execution engines:

**Pike VM (NFA simulation)**: Used for patterns without
backreferences, non-greedy quantifiers, or lookahead. Guarantees
O(pattern x text) time complexity, making it immune to ReDoS
attacks.

**Backtracking engine**: Used when patterns contain `\1`-`\9`
backreferences, non-greedy quantifiers (`*?`, `+?`, `??`), or
lookahead assertions (`(?=...)`, `(?!...)`). Protected by a
configurable step limit (`MRB_REGEXP_STEP_LIMIT`, default 1M) to
prevent excessive backtracking.

The engine is selected automatically at compile time based on
pattern analysis.

## Limitations

- **Fixed-length lookbehind only**: `(?<=...)` and `(?<!...)`
  require a fixed-length pattern (no `*`, `+`, `?`, or alternation).
  Maximum 255 bytes.
- **No Unicode properties**: `\p{Alpha}`, `\p{L}`, etc. are not
  supported.
- **ASCII case folding only**: The `i` flag handles ASCII letters
  only.
- **Step limit on backtracking**: Patterns that require the
  backtracking engine are subject to a step limit.

## Configuration

```c
/* Maximum step count for backtracking engine (ReDoS protection) */
#ifndef MRB_REGEXP_STEP_LIMIT
#define MRB_REGEXP_STEP_LIMIT 1000000
#endif
```

## License

MIT License. See the mruby license file for details.

This gem is an original implementation. No external regexp library
code is used.
