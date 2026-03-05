# mruby-rational

This mrbgem provides a `Rational` class for mruby, allowing you to work with rational numbers (fractions).

## Usage

To use the `Rational` class, you first need to include the mrbgem in your mruby build.

### Include in build_config.rb

Add the following line to your `build_config.rb` file:

```ruby
conf.gem :core => 'mruby-rational'
```

### Creating Rational Objects

You can create `Rational` objects using the `Rational()` method:

```ruby
r1 = Rational(1, 2)  # Represents 1/2
r2 = Rational(3, 4)  # Represents 3/4
```

### Arithmetic Operations

The `Rational` class supports standard arithmetic operations:

```ruby
r_add = Rational(1, 2) + Rational(1, 3)  # (5/6)
r_sub = Rational(1, 2) - Rational(1, 3)  # (1/6)
r_mul = Rational(1, 2) * Rational(1, 3)  # (1/6)
r_div = Rational(1, 2) / Rational(1, 3)  # (3/2)
```

## Available Methods

The `Rational` class provides the following important methods:

- `numerator`: Returns the numerator of the rational number.
- `denominator`: Returns the denominator of the rational number.
- `to_f`: Converts the rational number to a `Float`.
- `to_i`: Converts the rational number to an `Integer` (truncates towards zero).
- `to_s`: Returns a string representation of the rational number (e.g., "1/2").
- `inspect`: Returns a string representation suitable for debugging (e.g., "(1/2)").
- `==`: Checks for equality with another number.
- `<=>`: Compares the rational number with another number.
- `positive?`: Returns `true` if the rational number is greater than zero.
- `negative?`: Returns `true` if the rational number is less than zero.

## Error Handling

The mrbgem handles common errors such as:

- **DivisionByZeroError**: Raised when attempting to create a rational number with a denominator of zero.
- **RangeError**: Raised in case of integer overflow during calculations.

## License

mruby-rational is licensed under the MIT License. See LICENSE for details.
