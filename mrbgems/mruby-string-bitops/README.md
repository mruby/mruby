# mruby-string-bitops

Bit operations for `String`.
Strings are treated as byte buffers; all operations are independent of
string encoding.

## Methods

### Single-bit operations

- `String#bit_get(offset, lsb_first: true)` - returns `0` or `1`, or
  `nil` when `offset` is beyond the end of the string
- `String#bit_set?(offset, lsb_first: true)` - returns `true` or
  `false`, or `nil` when `offset` is beyond the end of the string
- `String#bit_set(offset, lsb_first: true)` - sets the bit to 1;
  returns `self`
- `String#bit_clear(offset, lsb_first: true)` - sets the bit to 0;
  returns `self`
- `String#bit_flip(offset, lsb_first: true)` - inverts the bit;
  returns `self`

`offset` is a zero-based bit offset. By default, bits within each byte
are numbered from least-significant to most-significant. With
`lsb_first: false`, byte order is unchanged but bits within each byte
are numbered from most-significant to least-significant.

`IndexError` is raised when `offset` is negative, or (for the mutating
methods) when it is beyond the end of the string.

### Bit-region operations

The mutating methods and `bit_count` also act on a contiguous run of
bits, given either as `(offset, length)` or as a `Range`. The two
forms are equivalent, following `String#[]`; passing a `Range` and a
`length` together is an `ArgumentError`.

- `String#bit_set(offset, length, lsb_first: true)`,
  `String#bit_set(range, lsb_first: true)`
- `String#bit_clear(offset, length, lsb_first: true)`,
  `String#bit_clear(range, lsb_first: true)`
- `String#bit_flip(offset, length, lsb_first: true)`,
  `String#bit_flip(range, lsb_first: true)`
- `String#bit_count(offset, length, lsb_first: true)`,
  `String#bit_count(range, lsb_first: true)`

Beginless and endless ranges work as usual (`0..` runs to the last
bit). An inverted range such as `5..2` is empty. `lsb_first` only
changes the numbering of bits within each byte.

The mutating methods require the whole region to be in range and
raise `IndexError` on overrun without modifying any bits. An empty
region is a no-op, but must still begin no later than the position
after the last bit: `"\x00".bit_set(8, 0)` is allowed and
`"\x00".bit_set(9, 0)` raises `IndexError`, as `"abc"[3, 0]` is `""`
while `"abc"[4, 0]` is `nil`. A frozen receiver raises `FrozenError`
even for an empty region.

`bit_count` instead clamps to the bits that exist, and returns `0`
(not `nil`) for a region that lies entirely beyond the end, so that
`length - s.bit_count(offset, length)` is always a number. It has no
single-bit form: `bit_count(offset)` raises `ArgumentError`, and a
count to the end is spelled `bit_count(offset..)`.

Negative offsets and range endpoints raise `IndexError`; there is no
count-from-end normalization. A negative `length` raises
`ArgumentError`, and an explicit `nil` length raises `TypeError`.

### Whole-string operations

- `String#bit_count(lsb_first: true)` - number of set bits (population
  count)
- `String#bitwise_not` / `String#bitwise_not!` - bitwise complement
- `String#bitwise_and(other)` / `String#bitwise_and!(other)`
- `String#bitwise_or(other)` / `String#bitwise_or!(other)`
- `String#bitwise_xor(other)` / `String#bitwise_xor!(other)`

The binary operations require both strings to have the same byte
length, otherwise `ArgumentError` is raised. The non-bang variants
return a new string; the bang variants mutate `self` in place.

## Example

```ruby
s = "\x00\x00"
s.bit_set(3)          # => "\x08\x00"
s.bit_set?(3)         # => true
s.bit_count           # => 1

s.bit_set(4, 8)       # => "\xF8\x0F"
s.bit_flip(0...16)    # => "\x07\xF0"
s.bit_count(4..11)    # => 0
s.bit_count(12..)     # => 4

"\xF0".bitwise_and("\xCC")  # => "\xC0"
"\x0F".bitwise_or("\xF0")   # => "\xFF"
"\xFF".bitwise_not          # => "\x00"
```

## Implementation notes

The bulk kernels (`bit_count` and the `bitwise_*` family) process one
machine word per iteration with 4x unrolling. A bit region is split
into a partial first byte, whole middle bytes, and a partial last
byte; the middle goes through `memset` or the same word-wide kernels,
so a region call costs about as much as the equivalent whole-string
call rather than one method call per bit. The word width follows
the pointer width of the target, so 32-bit targets (common for mruby)
use 32-bit words and avoid emulated 64-bit arithmetic. On
GNU-compatible compilers, word-aligned buffers are accessed directly
through a `may_alias` word pointer, which yields true word loads even
on cores without unaligned access support (e.g. Cortex-M0+).

Note that alignment cannot be assumed: malloc'ed buffers are
word-aligned, but embedded strings start right after the RString
header, which on 64-bit builds leaves them only 4-byte aligned (on
32-bit builds they are word-aligned). Buffers that miss the aligned
fast path are still processed word-at-a-time through a `memcpy`-based
loop; only the tail bytes go through a byte loop.

## Differences from CRuby

- Bit offsets are limited to `mrb_int`; the CRuby Bignum offset path
  has no mruby equivalent. Offsets that do not fit in `mrb_int` raise
  `RangeError` (CRuby raises `ArgumentError` for offsets beyond
  `uint64_t`).
- A `bit_count` result beyond `mrb_int` (reachable only on 32-bit
  `mrb_int` builds with strings over 256MiB) becomes a Bignum when
  mruby-bigint is present and raises `RangeError` otherwise.

- Implicit conversion is not honoured. CRuby converts a binary operand
  with `to_str` and a bit offset with `to_int`; this gem requires a
  real `String` and a real numeric offset. mruby has no implicit
  conversion protocol in core, so `Array.new(obj)`, `ary[obj]` and
  `"s" * obj` all reject an object that merely defines `to_int`, and
  this gem does not become the one place that accepts one.

Matching CRuby, the results of the non-bang bitwise
operations are BINARY (ASCII-8BIT) strings -- the binary flag is
observable through `String#encoding` when mruby-encoding is in the
build, and inert otherwise.

## License

MIT
