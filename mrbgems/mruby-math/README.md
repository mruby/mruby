# mruby-math

This mrbgem provides a comprehensive set of mathematical functions for mruby. It allows you to perform common mathematical operations within your mruby applications.

This gem is a standard component of mruby and provides functionalities similar to the `Math` module in standard Ruby.

## Available Functions

### Trigonometric Functions

- `Math.sin(x)`: Computes the sine of x (expressed in radians). Returns -1..1.
- `Math.cos(x)`: Computes the cosine of x (expressed in radians). Returns -1..1.
- `Math.tan(x)`: Returns the tangent of x (expressed in radians).

### Inverse Trigonometric Functions

- `Math.asin(x)`: Computes the arc sine of x. Returns computed value between `-(PI/2)` and `(PI/2)`.
- `Math.acos(x)`: Computes the arc cosine of x. Returns 0..PI.
- `Math.atan(x)`: Computes the arc tangent of x. Returns `-(PI/2) .. (PI/2)`.
- `Math.atan2(y, x)`: Computes the arc tangent given y and x. Returns -PI..PI.

### Hyperbolic Trigonometric Functions

- `Math.sinh(x)`: Computes the hyperbolic sine of x (expressed in radians).
- `Math.cosh(x)`: Computes the hyperbolic cosine of x (expressed in radians).
- `Math.tanh(x)`: Computes the hyperbolic tangent of x (expressed in radians).

### Inverse Hyperbolic Trigonometric Functions

- `Math.asinh(x)`: Computes the inverse hyperbolic sine of x.
- `Math.acosh(x)`: Computes the inverse hyperbolic cosine of x.
- `Math.atanh(x)`: Computes the inverse hyperbolic tangent of x.

### Exponential and Logarithmic Functions

- `Math.exp(x)`: Returns e\*\*x.
- `Math.log(numeric)` or `Math.log(num,base)`: Returns the natural logarithm of numeric. If additional second argument is given, it will be the base of logarithm.
- `Math.log2(numeric)`: Returns the base 2 logarithm of numeric.
- `Math.log10(numeric)`: Returns the base 10 logarithm of numeric.

### Other Functions

- `Math.sqrt(numeric)`: Returns the square root of numeric.
- `Math.cbrt(numeric)`: Returns the cube root of numeric.
- `Math.frexp(numeric)`: Returns a two-element array containing the normalized fraction (a Float) and exponent (a Integer) of numeric.
- `Math.ldexp(flt, int)`: Returns the value of flt\*(2\*\*int).
- `Math.hypot(x, y)`: Returns sqrt(x**2 + y**2), the hypotenuse of a right-angled triangle with sides x and y.
- `Math.erf(x)`: Calculates the error function of x.
- `Math.erfc(x)`: Calculates the complementary error function of x.

## Usage Examples

```ruby
# Basic trigonometric functions
puts Math.sin(Math::PI / 2) # Output: 1.0
puts Math.cos(0)            # Output: 1.0
puts Math.tan(Math::PI / 4) # Output: 1.0 (approximately, due to float precision)

# Square root
puts Math.sqrt(16)          # Output: 4.0
puts Math.sqrt(2)           # Output: 1.4142135623730951

# Cube root
puts Math.cbrt(8)           # Output: 2.0
puts Math.cbrt(27)          # Output: 3.0

# Exponential and Logarithm
puts Math.exp(0)            # Output: 1.0
puts Math.exp(1)            # Output: 2.718281828459045 (Math::E)
puts Math.log(Math::E)      # Output: 1.0
puts Math.log(100, 10)      # Output: 2.0
puts Math.log2(16)          # Output: 4.0
puts Math.log10(1000)       # Output: 3.0

# Hypotenuse
puts Math.hypot(3, 4)       # Output: 5.0
```
