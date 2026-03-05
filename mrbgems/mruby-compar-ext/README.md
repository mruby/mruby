# mruby-compar-ext

Comparable module extension.

This mrbgem adds the `clamp` method to the `Comparable` module.

## `Comparable#clamp`

The `clamp` method restricts a value to a given range.

**Syntax:**

```ruby
obj.clamp(min, max) -> obj
obj.clamp(range)    -> obj
```

**Description:**

- In the `(min, max)` form, it returns:
  - `min` if `obj <=> min` is less than zero.
  - `max` if `obj <=> max` is greater than zero.
  - `obj` otherwise.
- In the `(range)` form, it returns:
  - `range.begin` if `obj <=> range.begin` is less than zero.
  - `range.end` if `obj <=> range.end` is greater than zero (and `range.end` is not `nil` and the range is inclusive).
  - `obj` otherwise.
- If `range.begin` is `nil`, it is considered smaller than `obj`.
- If `range.end` is `nil`, it is considered greater than `obj`.

**Examples:**

```ruby
# Using min and max arguments
12.clamp(0, 100)         #=> 12
523.clamp(0, 100)        #=> 100
-3.123.clamp(0, 100)     #=> 0

'd'.clamp('a', 'f')      #=> 'd'
'z'.clamp('a', 'f')      #=> 'f'

# Using a Range argument
12.clamp(0..100)         #=> 12
523.clamp(0..100)        #=> 100
-3.123.clamp(0..100)     #=> 0

'd'.clamp('a'..'f')      #=> 'd'
'z'.clamp('a'..'f')      #=> 'f'

# Using ranges with nil begin or end
-20.clamp(0..)           #=> 0
523.clamp(..100)         #=> 100
```

**Error Handling:**

- Raises an `ArgumentError` if an exclusive range (e.g., `0...100`) is provided as the `range` argument when `range.end` is not `nil`.
- Raises an `ArgumentError` if `min` and `max` arguments cannot be compared (e.g., a `String` and an `Integer`).
- Raises an `ArgumentError` if the `min` argument is greater than the `max` argument.

```ruby
100.clamp(0...100)       # ArgumentError: cannot clamp with an exclusive range
10.clamp("a", "z")       # ArgumentError: comparison of String with String failed (depending on mruby version, may also be other errors if types are incompatible)
100.clamp(10, 0)         # ArgumentError: min argument must be smaller than max argument
```
