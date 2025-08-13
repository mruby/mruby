# mruby-numeric-ext

## Purpose

This mrbgem extends the `Numeric` and `Integer` classes in mruby with additional methods for common numerical operations and checks.

## Functionality

This gem adds the following methods:

### Numeric Class

- `zero?`: Returns `true` if the number is zero, `false` otherwise.
- `nonzero?`: Returns the number itself if it's not zero, `nil` otherwise.
- `positive?`: Returns `true` if the number is greater than 0, `false` otherwise.
- `negative?`: Returns `true` if the number is less than 0, `false` otherwise.
- `integer?`: Returns `true` if the number is an `Integer` (this is overridden in the `Integer` class).

### Integer Class

- `allbits?(mask)`: Returns `true` if all bits of `self & mask` are 1.
- `anybits?(mask)`: Returns `true` if any bits of `self & mask` are 1.
- `nobits?(mask)`: Returns `true` if no bits of `self & mask` are 1.
- `bit_length`: Returns the number of bits of the absolute value of `self` in binary. `0.bit_length #=> 0`; `(-1).bit_length #=> 0`; `2.bit_length #=> 2`.
- `ceildiv(other)`: Returns the result of `self` divided by `other`, rounded up to the nearest integer.
- `integer?`: Returns `true` (overrides `Numeric#integer?`).
- `remainder(numeric)`: Returns the remainder of `self` divided by `numeric`. Equivalent to `x - y * (x / y).truncate`.
- `pow(numeric)`: Returns `self` raised to the power of `numeric`.
- `pow(integer, integer)`: Returns modular exponentiation (`(self ** exponent) % modulus`).
- `digits(base=10)`: Returns an array of integers representing the base-radix digits of `self`. The first element is the least significant digit.
- `size`: Returns the number of bytes in the machine representation of the integer.
- `odd?`: Returns `true` if the integer is odd, `false` otherwise.
- `even?`: Returns `true` if the integer is even, `false` otherwise.
- `Integer.sqrt(integer)`: (Class method) Returns the integer square root of the given non-negative integer.

## How to use

To use this mrbgem, add it to your `build_config.rb`:

```ruby
MRuby::Build.new do |conf|
  # ... (other configurations)
  conf.gem :core => 'mruby-numeric-ext'
end
```

Then you can use the extended methods:

```ruby
p 5.positive?  # => true
p (-3).negative? # => true
p 0.zero?      # => true
p 5.nonzero?   # => 5
p 0.nonzero?   # => nil

p 7.allbits?(3)  # => false (binary 111 & 011 is 011, not 011)
p 7.anybits?(3)  # => true
p 7.nobits?(8)   # => true (binary 111 & 1000 is 000)

p 10.ceildiv(3)   # => 4
p (-10).ceildiv(3) # => -3

p 10.remainder(3) # => 1
p (-10).remainder(3) # => -1

p 2.pow(3)        # => 8
p 2.pow(3, 5)     # => 3 ( (2**3) % 5 )

p 12345.digits    # => [5, 4, 3, 2, 1]
p 12345.digits(16) # => [9, 3, 0, 3]

p 42.size         # => (depends on machine, e.g., 4 or 8)
p 5.bit_length    # => 3
p 5.odd?          # => true
p 4.even?         # => true
p Integer.sqrt(16) # => 4
p Integer.sqrt(17) # => 4
```
