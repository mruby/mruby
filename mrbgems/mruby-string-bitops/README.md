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

### Whole-string operations

- `String#bit_count` - number of set bits (population count)
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

"\xF0".bitwise_and("\xCC")  # => "\xC0"
"\x0F".bitwise_or("\xF0")   # => "\xFF"
"\xFF".bitwise_not          # => "\x00"
```

## Implementation notes

The bulk kernels (`bit_count` and the `bitwise_*` family) process one
machine word per iteration with 4x unrolling. The word width follows
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

Matching CRuby, binary operands are converted with `to_str` and bit
offsets with `to_int`, and the results of the non-bang bitwise
operations are BINARY (ASCII-8BIT) strings -- the binary flag is
observable through `String#encoding` when mruby-encoding is in the
build, and inert otherwise.

## License

MIT
