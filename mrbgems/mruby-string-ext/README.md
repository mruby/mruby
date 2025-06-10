# mruby-string-ext

This mrbgem adds extended string functionalities to mruby.

## Methods

### `String#clear`

Makes string empty.

```ruby
string.clear
```

Example:

```ruby
a = "abcde"
a.clear    #=> ""
```

### `String#lstrip`

Returns a copy of the string with leading whitespace removed.

```ruby
str.lstrip
```

Example:

```ruby
"  hello  ".lstrip   #=> "hello  "
```

### `String#lstrip!`

Removes leading whitespace from the string, returning `nil` if no change was made. Modifies the receiver in place.

```ruby
str.lstrip!
```

Example:

```ruby
a = "  hello  "
a.lstrip!   #=> "hello  "
b = "hello"
b.lstrip!      #=> nil
```

### `String#rstrip`

Returns a copy of the string with trailing whitespace removed.

```ruby
str.rstrip
```

Example:

```ruby
"  hello  ".rstrip   #=> "  hello"
```

### `String#rstrip!`

Removes trailing whitespace from the string, returning `nil` if no change was made. Modifies the receiver in place.

```ruby
str.rstrip!
```

Example:

```ruby
a = "  hello  "
a.rstrip!   #=> "  hello"
b = "hello"
b.rstrip!      #=> nil
```

### `String#strip`

Returns a copy of the string with leading and trailing whitespace removed.

```ruby
str.strip
```

Example:

```ruby
"    hello    ".strip   #=> "hello"
"\tgoodbye\r\n".strip   #=> "goodbye"
```

### `String#strip!`

Removes leading and trailing whitespace from the string. Returns `nil` if the string was not altered. Modifies the receiver in place.

```ruby
str.strip!
```

Example:

```ruby
a = "    hello    "
a.strip! #=> "hello"
b = "goodbye"
b.strip! #=> nil
```

### `String#partition`

Searches for the first occurrence of the separator `sep` and returns a three-element array: the part before the separator, the separator itself, and the part after the separator. If the separator is not found, returns the original string and two empty strings.

```ruby
string.partition(sep)
```

Example:

```ruby
"hello".partition("l") #=> ["he", "l", "lo"]
"hello".partition("x") #=> ["hello", "", ""]
```

### `String#rpartition`

Searches for the last occurrence of the separator `sep` and returns a three-element array: the part before the separator, the separator itself, and the part after the separator. If the separator is not found, returns two empty strings and the original string.

```ruby
string.rpartition(sep)
```

Example:

```ruby
"hello ello".rpartition("l") #=> ["hello el", "l", "o"]
"hello".rpartition("x")    #=> ["", "", "hello"]
```

### `String#slice!`

Deletes the specified portion from the string, and returns the portion deleted. Modifies the receiver in place.

```ruby
str.slice!(fixnum)
str.slice!(fixnum, fixnum)
str.slice!(range)
str.slice!(other_str)
```

Example:

```ruby
string = "hello world"
string.slice!(4)        #=> "o"
# string is now "hell world"
string.slice!(2..3)     #=> "ll"
# string is now "he world"
string.slice!("l")      #=> "l"
# string is now "he word"
string.slice!("nomatch") #=> nil
```

### `String#insert`

Inserts `other_str` before the character at the given `index`, modifying `str`. Negative indices count from the end of the string.

```ruby
str.insert(index, other_str)
```

Example:

```ruby
"abcd".insert(0, 'X')    #=> "Xabcd"
"abcd".insert(3, 'X')    #=> "abcXd"
"abcd".insert(4, 'X')    #=> "abcdX"
"abcd".insert(-3, 'X')   #=> "abXcd"
"abcd".insert(-1, 'X')   #=> "abcdX"
```

### `String#ljust`

If `integer` is greater than the length of `str`, returns a new string of length `integer` with `str` left justified and padded with `padstr`; otherwise, returns `str`.

```ruby
str.ljust(integer, padstr=' ')
```

Example:

```ruby
"hello".ljust(4)            #=> "hello"
"hello".ljust(20)           #=> "hello               "
"hello".ljust(20, '1234')   #=> "hello123412341234123"
```

### `String#rjust`

If `integer` is greater than the length of `str`, returns a new string of length `integer` with `str` right justified and padded with `padstr`; otherwise, returns `str`.

```ruby
str.rjust(integer, padstr=' ')
```

Example:

```ruby
"hello".rjust(4)            #=> "hello"
"hello".rjust(20)           #=> "               hello"
"hello".rjust(20, '1234')   #=> "123412341234123hello"
```

### `String#center`

Centers `str` in `width`. If `width` is greater than the length of `str`, returns a new String of length `width` with `str` centered and padded with `padstr`; otherwise, returns `str`.

```ruby
str.center(width, padstr=' ')
```

Example:

```ruby
"hello".center(4)         #=> "hello"
"hello".center(20)        #=> "       hello        "
"hello".center(20, '123') #=> "1231231hello12312312"
```

### `String#chars`

If a block is given, calls the block for each character. Otherwise, returns an array of characters in the string.

```ruby
str.chars                 #=> array
str.chars {|char| block } #=> str
```

Example:

```ruby
"hello".chars #=> ["h", "e", "l", "l", "o"]
```

### `String#each_char`

Calls the given block for each character of the string. If no block is given, returns an enumerator.

```ruby
str.each_char {|char| block }  #=> str
str.each_char                  #=> an_enumerator
```

Example:

```ruby
s = ""
"hello".each_char {|c| s << c << '*' } # s is now "h*e*l*l*o*"
```

### `String#codepoints` (alias `each_codepoint`)

If a block is given, calls the block with the Integer ordinal of each character in the string. If no block is given, returns an array of these ordinals.

```ruby
str.codepoints                 #=> array
str.codepoints {|codepoint| block } #=> str
```

Example:

```ruby
"h\u00E9llo".codepoints #=> [104, 233, 108, 108, 111] (if UTF-8 aware)
"hello".codepoints      #=> [104, 101, 108, 108, 111]
```

### `String#prepend`

Prepends the given string(s) to `str`. Modifies `str` in place.

```ruby
str.prepend(other_str, ...)
```

Example:

```ruby
a = "world"
a.prepend("hello ") #=> "hello world"
a                   #=> "hello world"
a.prepend("Greeting: ", "Bob! ") #=> "Greeting: Bob! hello world"
```

### `String#lines`

Returns an array of strings, where each string is a line from the original string. Lines are separated by `\n`. If a block is given, it works the same as `each_line`.

```ruby
string.lines                #=> array
string.lines {|s| block}    #=> string
```

Example:

```ruby
a = "abc\ndef"
a.lines    #=> ["abc\n", "def"]
"hello\nworld".lines {|line| puts line } # prints "hello\n" then "world"
```

### `String#upto`

Iterates through successive values, starting at `str` and ending at `other_str` inclusive (unless `exclusive` is true). The `String#succ` method is used to generate each value.

```ruby
str.upto(other_str, exclusive=false) {|s| block }   #=> str
str.upto(other_str, exclusive=false)                #=> an_enumerator
```

Example:

```ruby
"a8".upto("b1") {|s| print s, ' ' } #=> prints: a8 a9 b0 b1
"9".upto("11").to_a   #=> ["9", "10", "11"]
"07".upto("11").to_a  #=> ["07", "08", "09", "10", "11"]
"a".upto("c", true).to_a #=> ["a", "b"]
```

### `String#swapcase`

Returns a copy of `str` with uppercase alphabetic characters converted to lowercase and lowercase characters converted to uppercase. Effective only in ASCII region.

```ruby
str.swapcase   #=> new_str
```

Example:

```ruby
"Hello".swapcase          #=> "hELLO"
"cYbEr_PuNk11".swapcase   #=> "CyBeR_pUnK11"
```

### `String#swapcase!`

Equivalent to `String#swapcase`, but modifies the receiver in place. Returns `str`, or `nil` if no changes were made.

```ruby
str.swapcase!   #=> str or nil
```

Example:

```ruby
a = "Hello"
a.swapcase!          #=> "hELLO"
a                    #=> "hELLO"
b = "123"
b.swapcase!          #=> nil
```

### `String#concat` (alias `String#<<`)

Appends the argument(s) to the string. If an argument is an Integer, it's considered a codepoint and converted to a character. Modifies the string in place.

```ruby
str.concat(other_str, ...)   #=> str
str << obj                   #=> str
```

Example:

```ruby
s = 'foo'
s.concat('bar', 'baz') # => "foobarbaz"
s                      # => "foobarbaz"
s = 'foo'
s << 'bar' << 32 << 'baz' # => "foobar baz" (32 is space)
```

### `String#append_as_bytes`

Works like `concat` but considers arguments as binary strings. Integer arguments are treated as byte values (0-255) and converted to characters.

```ruby
str.append_as_bytes(*obj)     #=> str
```

Example:

```ruby
s = "test"
s.append_as_bytes(32, "bytes", 33) #=> "test bytes!" (32 is space, 33 is !)
s.append_as_bytes(256) #=> RangeError (byte 256 out of range)
```

### `String#start_with?`

Returns true if `str` starts with one of the `prefixes` given.

```ruby
str.start_with?([prefixes]+)   #=> true or false
```

Example:

```ruby
"hello".start_with?("hell")               #=> true
"hello".start_with?("heaven", "hell")     #=> true
"hello".start_with?("heaven", "paradise") #=> false
```

### `String#end_with?`

Returns true if `str` ends with one of the `suffixes` given.

```ruby
str.end_with?([suffixes]+)   #=> true or false
```

Example:

```ruby
"hello".end_with?("llo")                  #=> true
"hello".end_with?("heaven", "llo")        #=> true
"hello".end_with?("heaven", "paradise")   #=> false
```

### `String#tr`

Returns a copy of `str` with the characters in `from_str` replaced by the corresponding characters in `to_str`.
Supports `c1-c2` range notation and `^` for negation in `from_str`.

```ruby
str.tr(from_str, to_str)   #=> new_str
```

Example:

```ruby
"hello".tr('el', 'ip')      #=> "hippo"
"hello".tr('aeiou', '*')    #=> "h*ll*"
"hello".tr('a-y', 'b-z')    #=> "ifmmp"
"hello".tr('^aeiou', '*')   #=> "*e**o"
```

### `String#tr!`

Translates `str` in place, using the same rules as `String#tr`. Returns `str`, or `nil` if no changes were made.

```ruby
str.tr!(from_str, to_str)   #=> str or nil
```

Example:

```ruby
a = "hello"
a.tr!('el', 'ip')      #=> "hippo"
a                      #=> "hippo"
b = "hello"
b.tr!('xyz', '123')    #=> nil
```

### `String#tr_s`

Processes a copy of `str` as described under `String#tr`, then removes duplicate characters in regions that were affected by the translation (squeeze).

```ruby
str.tr_s(from_str, to_str)   #=> new_str
```

Example:

```ruby
"hello".tr_s('l', 'r')     #=> "hero"
"hello".tr_s('el', '*')    #=> "h*o"
"hello".tr_s('el', 'hx')   #=> "hhxo"
```

### `String#tr_s!`

Performs `String#tr_s` processing on `str` in place, returning `str`, or `nil` if no changes were made.

```ruby
str.tr_s!(from_str, to_str)   #=> str or nil
```

Example:

```ruby
a = "hello"
a.tr_s!('l', 'r')      #=> "hero"
a                      #=> "hero"
b = "hello"
b.tr_s!('x', 'y')      #=> nil
```

### `String#squeeze`

Builds a set of characters from the `other_str` parameter(s). Returns a new string where runs of the same character that occur in this set are replaced by a single character. If no arguments are given, all runs of identical characters are replaced.

```ruby
str.squeeze([other_str])    #=> new_str
```

Example:

```ruby
"yellow moon".squeeze                  #=> "yelow mon"
"  now   is  the".squeeze(" ")         #=> " now is the"
"putters shoot balls".squeeze("m-z")   #=> "puters shot balls"
```

### `String#squeeze!`

Squeezes `str` in place, returning either `str`, or `nil` if no changes were made.

```ruby
str.squeeze!([other_str])   #=> str or nil
```

Example:

```ruby
a = "yellow moon"
a.squeeze!                 #=> "yelow mon"
a                          #=> "yelow mon"
b = "abc"
b.squeeze!                 #=> nil
```

### `String#delete`

Returns a copy of `str` with all characters in the intersection of its arguments removed. Arguments are selectors like for `String#count`.

```ruby
str.delete([other_str]+)    #=> new_str
```

Example:

```ruby
"hello".delete "l"         #=> "heo"
"hello".delete "aeiou"     #=> "hll"
"hello".delete "aeiou", "^l" #=> "l" (deletes vowels, but not 'l')
```

### `String#delete!`

Performs a `delete` operation in place, returning `str`, or `nil` if `str` was not modified.

```ruby
str.delete!([other_str]+)   #=> str or nil
```

Example:

```ruby
a = "hello"
a.delete!("l")             #=> "heo"
a                          #=> "heo"
b = "hello"
b.delete!("xyz")           #=> nil
```

### `String#count`

Each `other_str` parameter defines a set of characters to count. The intersection of these sets defines the characters to count in `str`.

```ruby
str.count([other_str]+)   #=> integer
```

Example:

```ruby
"hello world".count("lo")            #=> 5
"hello world".count("lo", "o")       #=> 2 (chars 'l' and 'o', but only 'o' is in both sets)
"hello world".count("h")             #=> 1
"hello world".count("a-e", "h-l")    #=> 3 (chars 'h', 'e', 'l')
"hello world".count("^l")            #=> 8 (all chars except 'l')
```

### `String#hex`

Treats leading characters of `str` as a string of hexadecimal digits (with an optional sign and an optional `0x`) and returns the corresponding number.

```ruby
str.hex   #=> integer
```

Example:

```ruby
"0x0a".hex    #=> 10
"10".hex      #=> 16
"-10".hex     #=> -16
"ff".hex      #=> 255
"hello".hex   #=> 0 (if no valid hex digits at start)
"0xhello".hex #=> 0
```

### `String#oct`

Treats leading characters of `str` as a string of octal digits (with an optional sign) and returns the corresponding number.

```ruby
str.oct   #=> integer
```

Example:

```ruby
"10".oct      #=> 8
"010".oct     #=> 8
"-10".oct     #=> -8
"077".oct     #=> 63
"hello".oct   #=> 0 (if no valid octal digits at start)
"0o10".oct    #=> 8 (common prefix, depends on MRuby version)
```

### `String#chr` (on String instances)

Returns a one-character string at the beginning of the string.

```ruby
string.chr    #=>  string
```

Example:

```ruby
a = "abcde"
a.chr    #=> "a"
"".chr   #=> "" (or error, mruby specific behavior might differ from CRuby)
```

### `Integer#chr`

Returns a string containing the character represented by the `int`'s value according to `encoding`.

```ruby
int.chr([encoding])  #=>  string
```

Example:

```ruby
65.chr                  #=> "A"
230.chr                 #=> "\xE6" (in ASCII-8BIT)
230.chr("ASCII-8BIT")   #=> "\xE6"
# 230.chr("UTF-8")        #=> "\u00E6" (if MRB_UTF8_STRING enabled)
```

### `String#succ` (alias `String#next`)

Returns the successor to `str`. Increments the rightmost alphanumeric characters.

```ruby
str.succ    #=> new_str
```

Example:

```ruby
"a".succ     #=> "b"
"z".succ     #=> "aa"
"9".succ     #=> "10"
"a9".succ    #=> "b0"
"Az".succ    #=> "Ba"
"zz".succ    #=> "aaa"
```

### `String#succ!` (alias `String#next!`)

Equivalent to `String#succ`, but modifies the receiver in place.

```ruby
str.succ!   #=> str
```

Example:

```ruby
a = "a9"
a.succ!    #=> "b0"
a          #=> "b0"
```

### `String#ord`

Returns the Integer ordinal (codepoint) of the first character in `str`.

```ruby
str.ord   #=> integer
```

Example:

```ruby
"a".ord    #=> 97
"A".ord    #=> 65
"\u20AC".ord #=> 8364 (if UTF-8 and character is euro sign)
"".ord     #=> ArgumentError (empty string)
```

### `String#delete_prefix`

Returns a copy of `str` with leading `prefix` deleted.

```ruby
str.delete_prefix(prefix) -> new_str
```

Example:

```ruby
"hello".delete_prefix("hel") #=> "lo"
"hello".delete_prefix("llo") #=> "hello"
```

### `String#delete_prefix!`

Deletes leading `prefix` from `str`, returning `nil` if no change was made. Modifies the receiver in place.

```ruby
str.delete_prefix!(prefix) -> self or nil
```

Example:

```ruby
a = "hello"
a.delete_prefix!("hel") #=> "lo"
a                       #=> "lo"
b = "hello"
b.delete_prefix!("llo") #=> nil
```

### `String#delete_suffix`

Returns a copy of `str` with trailing `suffix` deleted.

```ruby
str.delete_suffix(suffix) -> new_str
```

Example:

```ruby
"hello".delete_suffix("llo") #=> "he"
"hello".delete_suffix("hel") #=> "hello"
```

### `String#delete_suffix!`

Deletes trailing `suffix` from `str`, returning `nil` if no change was made. Modifies the receiver in place.

```ruby
str.delete_suffix!(suffix) -> self or nil
```

Example:

```ruby
a = "hello"
a.delete_suffix!("llo") #=> "he"
a                       #=> "he"
b = "hello"
b.delete_suffix!("hel") #=> nil
```

### `String#casecmp`

Case-insensitive version of `String#<=>`. Returns -1, 0, or +1. Returns `nil` if `other_str` is not a String.

```ruby
str.casecmp(other_str)   #=> -1, 0, +1 or nil
```

Example:

```ruby
"abcdef".casecmp("abcde")     #=> 1
"aBcDeF".casecmp("abcdef")    #=> 0
"abcdef".casecmp("abcdefg")   #=> -1
"abcdef".casecmp("ABCDEF")    #=> 0
```

### `String#casecmp?`

Returns `true` if `str` and `other_str` are equal after case folding, `false` if they are not equal, and `nil` if `other_str` is not a String.

```ruby
str.casecmp?(other_str)  #=> true, false, or nil
```

Example:

```ruby
"aBcDeF".casecmp?("abcdef")    #=> true
"aBcDeF".casecmp?("abcdeg")    #=> false
```

### `String#+@` (Unary Plus)

Returns `self` if `self` is not frozen. Otherwise returns a mutable (not frozen) duplicate of `self`.

```ruby
+string   #=> new_string or self
```

Example:

```ruby
a = "hello"
b = +a
a.equal?(b) #=> true (both point to the same object)

c = "world".freeze
d = +c
c.equal?(d) #=> false (d is a mutable copy)
d.frozen?   #=> false
```

### `String#-@` (Unary Minus)

Returns a frozen, possibly pre-existing (interned) copy of the string.

```ruby
-string   #=> frozen_string
```

Example:

```ruby
a = "hello"
b = -a
a.equal?(b) #=> false (b is a different, frozen object)
b.frozen?   #=> true

c = "world".freeze
d = -c
c.equal?(d) #=> true (if c was already suitably frozen/interned)
```

### `String#ascii_only?`

Returns `true` if `str` contains only ASCII characters.

```ruby
str.ascii_only?   #=> true or false
```

Example:

```ruby
"abc".ascii_only?      #=> true
"abc\x80".ascii_only?  #=> false (if \x80 is considered non-ASCII)
"日本語".ascii_only? #=> false
```

### `String#b`

Returns a copy of `str` with its encoding set to ASCII-8BIT (binary).

```ruby
str.b   #=> new_str_in_binary_encoding
```

Example:

```ruby
# Assuming str was UTF-8
str_utf8 = "\u00E9" # "é"
str_bin = str_utf8.b
# str_bin might be "\xC3\xA9" if that's the UTF-8 byte representation
# str_bin.encoding will be ASCII-8BIT
```
