# mruby-enum-lazy

## Overview

This mrbgem provides lazy evaluation for Enumerable objects in mruby. It introduces the `Enumerable#lazy` method, which returns an instance of `Enumerator::Lazy`. This allows for more efficient processing of collections, especially large or potentially infinite sequences, by evaluating elements only when they are needed.

## Functionality

When you call `.lazy` on an Enumerable object (like an Array or Range), you get back an `Enumerator::Lazy` object. This object behaves much like a regular Enumerator, but with a key difference: methods that transform the collection are deferred until the results are actually required.

The following methods are implemented to act lazily:

- `map` / `collect`
- `select` / `find_all`
- `reject`
- `grep`
- `grep_v`
- `drop`
- `drop_while`
- `take`
- `take_while`
- `flat_map` / `collect_concat`
- `zip`
- `uniq`

To trigger the evaluation of the lazy operations and retrieve all results (if finite), you can use methods like `force` or `to_a`.

### How it works

Operations on an `Enumerator::Lazy` object are chained together. The actual computation of each element is postponed until it's requested (e.g., by `force`, `to_a`, or iterating with `each`). This can lead to significant performance improvements by avoiding unnecessary computations and memory allocations, particularly when dealing with large data sets or when only a subset of results is needed.

## Usage Example

Here's a simple example demonstrating lazy evaluation:

```ruby
# Without lazy evaluation
# This would attempt to create an infinite array, which is not feasible.
# (1..Float::INFINITY).map { |x| x * x }.select { |x| x % 2 == 0 }.take(5).to_a

# With lazy evaluation
p (1..Float::INFINITY).lazy.map { |x| x * x }.select { |x| x % 2 == 0 }.take(5).force
# Output: [4, 16, 36, 64, 100]

# Another example:
a = [1, 2, 3, 4, 5]
lazy_sequence = a.lazy.map do |x|
  puts "mapping #{x}"
  x * 10
end.select do |x|
  puts "selecting #{x}"
  x > 20
end

puts "Applying force..."
result = lazy_sequence.force
# Output:
# mapping 1
# selecting 10
# mapping 2
# selecting 20
# mapping 3
# selecting 30
# mapping 4
# selecting 40
# mapping 5
# selecting 50
# Applying force...
p result # Output: [30, 40, 50]
```

## Dependencies

This gem depends on the following mruby core gems:

- `mruby-enumerator`
- `mruby-enum-ext`

## License

MIT License

## Author

mruby developers

## Acknowledgements

Based on <https://github.com/yhara/enumerable-lazy>
Inspired by <https://github.com/antimon2/enumerable_lz>
Reference: <http://jp.rubyist.net/magazine/?0034-Enumerable_lz> (ja)
