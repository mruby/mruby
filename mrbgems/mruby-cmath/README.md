# mruby-cmath

This mrbgem provides complex number support for mruby, offering mathematical functions that can handle complex numbers, similar to Ruby's standard `cmath` library.

## Requirements

This gem utilizes C99 `_Complex` features. Therefore, you will need a C compiler that supports C99 or a later version to build and use this gem.

## Provided Functions

The `CMath` module includes the following mathematical functions:

- `exp(z)`: Computes the exponential of `z`.
- `log(z)`: Computes the natural logarithm of `z`.
- `log(z, b)`: Computes the logarithm of `z` with base `b`.
- `log2(z)`: Computes the base-2 logarithm of `z`.
- `log10(z)`: Computes the base-10 logarithm of `z`.
- `sqrt(z)`: Computes the square root of `z`.
- `sin(z)`: Computes the sine of `z`.
- `cos(z)`: Computes the cosine of `z`.
- `tan(z)`: Computes the tangent of `z`.
- `asin(z)`: Computes the arc sine of `z`.
- `acos(z)`: Computes the arc cosine of `z`.
- `atan(z)`: Computes the arc tangent of `z`.
- `sinh(z)`: Computes the hyperbolic sine of `z`.
- `cosh(z)`: Computes the hyperbolic cosine of `z`.
- `tanh(z)`: Computes the hyperbolic tangent of `z`.
- `asinh(z)`: Computes the inverse hyperbolic sine of `z`.
- `acosh(z)`: Computes the inverse hyperbolic cosine of `z`.
- `atanh(z)`: Computes the inverse hyperbolic tangent of `z`.

## Usage Example

Here's a simple example of how to use `CMath` to calculate the square root of a negative number:

```ruby
# Assuming mruby is built with mruby-cmath

# Calculate the square root of -4
result = CMath.sqrt(-4)

# result will be a Complex number (0+2i)
puts result.real      # Output: 0.0
puts result.imaginary # Output: 2.0
```
