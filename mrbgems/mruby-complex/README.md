# mruby-complex

mruby-complex is an mrbgem that provides a `Complex` class for mruby, allowing for the representation and manipulation of complex numbers. It is designed to be compatible with the Complex class in standard Ruby.

## Functionality

The `Complex` class provided by this mrbgem supports:

- Creation of complex numbers from real and imaginary parts, or using the `i` suffix for numbers.
- Basic arithmetic operations: addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`).
- Methods to access the real and imaginary parts (`real`, `imaginary` or `imag`).
- Calculation of absolute value (`abs`, `magnitude`), argument/angle (`arg`, `angle`, `phase`), and square of the absolute value (`abs2`).
- Complex conjugate (`conjugate`, `conj`).
- Conversion to polar coordinates (`polar`).
- Conversion to other numeric types where appropriate (`to_f`, `to_i`, `to_r`, `to_c`).
- Inspection and string representation (`inspect`, `to_s`).

## Usage Examples

Here are some basic examples of how to use the `Complex` class:

```ruby
# Creating complex numbers
c1 = Complex(1, 2)      # => (1+2i)
c2 = 3 + 4i             # => (3+4i)
c3 = Complex.polar(5, Math::PI/2) # => (0.0+5.0i) (approximately)

# Arithmetic operations
puts c1 + c2  # => (4+6i)
puts c1 - c2  # => (-2-2i)
puts c1 * c2  # => (-5+10i)
puts c1 / c2  # => (0.44+0.08i)

# Accessing parts
puts c1.real      # => 1
puts c1.imaginary # => 2

# Other methods
puts c2.abs       # => 5.0
puts c2.arg       # => 0.9272952180016122 (radians)
puts c2.conjugate # => (3-4i)
puts c1.polar     # => [2.23606797749979, 1.1071487177940904]

# Numerics can be converted to Complex
puts 5.to_c       # => (5+0i)
puts 2.3.to_c     # => (2.3+0i)

# The `i` method on numerics creates a complex number with zero real part
puts 5i           # => (0+5i)
puts 2.3i         # => (0+2.3i)
```

## Note

This mrbgem aims to provide functionality similar to the `Complex` class found in the standard Ruby library. Refer to the Ruby documentation for `Complex` for more detailed information on the behavior of various methods.
