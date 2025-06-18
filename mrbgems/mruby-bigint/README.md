# mruby-bigint

mruby-bigint is an mrbgem that provides multi-precision integer (BigInt) support for mruby. It allows you to work with integers that are larger than the standard Integer type can handle.

This extension uses fgmp, which is a public domain implementation of a subset of the GNU gmp library by Mark Henderson <markh@wimsey.bc.ca>.
But it's heavily modified to fit with mruby. You can get the original source code from <https://github.com/deischi/fgmp.git>.
You can read the original README for fgmp in [README-fgmp.md](README-fgmp.md).

If you want to create your own Multi-precision Integer GEM, see [examples/mrbgems/mruby-YOUR-bigint/TODO-HINT.md](../../examples/mrbgems/mruby-YOUR-bigint/TODO-HINT.md).

## Features

- Basic arithmetic operations: `+`, `-`, `*`, `/`, `%`
- Power operation: `**`
- Modular exponentiation
- Bitwise operations: `&`, `|`, `^`, `<<`, `>>`
- Comparison: `<=>`
- Conversion to and from strings: `to_s`, `String#to_i` (with base)
- Square root
- Greatest Common Divisor (GCD) (available if `MRB_USE_RATIONAL` is defined)

## Usage

Here are some simple examples of how to use mruby-bigint:

```ruby
# Creating BigInts
a = BigInt(12345678901234567890)
b = "98765432109876543210".to_i(10) # Specify base 10 for string conversion

# Arithmetic operations
c = a + b
puts c.to_s # Output: 111111111011111111100

d = a * 2
puts d.to_s # Output: 24691357802469135780

# Comparison
puts a <=> b # Output: -1
```

## fgmp Dependency

mruby-bigint depends on fgmp. For more information about fgmp, please see [README-fgmp.md](README-fgmp.md).
