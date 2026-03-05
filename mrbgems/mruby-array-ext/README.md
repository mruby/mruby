# mruby-array-ext

This mrbgem extends the `Array` class in mruby, providing a rich set of additional methods for array manipulation. These extensions enhance mruby's built-in array capabilities, offering functionalities commonly found in standard Ruby.

## Functionality

The `mruby-array-ext` gem adds the following methods to the `Array` class:

### Set Operations

- `uniq`: Returns a new array by removing duplicate values.
- `uniq!`: Removes duplicate elements from self. Returns `nil` if no changes are made.
- `-` (difference): Returns a new array that is a copy of the original array, removing any items that also appear in another array.
- `|` (union): Returns a new array by joining this array with another array, removing duplicates.
- `&` (intersection): Returns a new array containing elements common to two arrays, with no duplicates.
- `difference`: Returns a new array that is a copy of the original array, removing all occurrences of any item that also appear in the specified other arrays.
- `union`: Returns a new array by joining this array with other arrays, removing duplicates.
- `intersection`: Returns a new array containing elements common to this array and specified other arrays, removing duplicates.
- `intersect?`: Returns `true` if the array and another array have at least one element in common.

### Element Manipulation and Access

- `flatten`: Returns a new array that is a one-dimensional flattening of self.
- `flatten!`: Flattens self in place. Returns `nil` if no modifications were made.
- `fetch`: Tries to return the element at a given position. Throws an `IndexError` if the index is out of bounds, unless a default value or block is provided.
- `fill`: Sets selected elements of self to a given object or the result of a block.
- `compact`: Returns a copy of self with all `nil` elements removed.
- `compact!`: Removes `nil` elements from self. Returns `nil` if no changes were made.
- `rotate(count=1)`: Returns a new array by rotating self so that the element at `count` is the first element.
- `rotate!(count=1)`: Rotates self in place.
- `insert(index, obj...)`: Inserts the given values before the element with the given index.
- `slice!(index)` / `slice!(start, length)` / `slice!(range)`: Deletes element(s) given by an index or range and returns the deleted object(s).
- `at(index)`: Returns the element at `index`. Returns `nil` if the index is out of range.
- `dig(idx, ...)`: Extracts a nested value specified by a sequence of indices.
- `fetch_values(idx, ...)`: Returns an array containing the values associated with the given indexes. Raises `IndexError` if an index is not found, unless a block is provided.
- `values_at(selector, ...)`: Returns an array containing the elements corresponding to the given selector(s).

### Searching and Comparison

- `assoc(obj)`: Searches through an array of arrays, comparing `obj` with the first element of each contained array. Returns the first matching contained array or `nil`.
- `rassoc(obj)`: Searches through an array of arrays, comparing `obj` with the second element of each contained array. Returns the first matching contained array or `nil`.
- `bsearch { |x| block }`: Finds a value from the array which meets the given condition using binary search.
- `bsearch_index { |x| block }`: Finds the index of a value from the array which meets the given condition using binary search.

### Iterators and Combinatorics

- `reverse_each`: Calls the given block for each element in reverse order.
- `permutation(n=self.size)`: Yields all permutations of length `n` of the elements of the array.
- `combination(n)`: Yields all combinations of length `n` of elements from the array.
- `product(*arys)`: Returns an array of all combinations of elements from self and the given arrays.
- `repeated_combination(n)`: Yields all repeated combinations of length `n`.
- `repeated_permutation(n)`: Yields all repeated permutations of length `n`.

### Conversions and Other Utilities

- `to_h`: Returns the result of interpreting the array as an array of `[key, value]` pairs.
- `transpose`: Assumes that self is an array of arrays and transposes the rows and columns.

### Filtering

- `delete_if { |item| block }`: Deletes every element of self for which the block evaluates to `true`.
- `reject! { |item| block }`: Equivalent to `delete_if`, but returns `nil` if no changes were made.
- `keep_if { |item| block }`: Deletes every element of self for which the given block evaluates to `false`.
- `select! { |item| block }`: Deletes elements for which the block returns a `false` value. Returns `self` if changes were made, otherwise `nil`.

### Aliases

- `append` (alias for `push`)
- `prepend` (alias for `unshift`)
- `filter!` (alias for `select!`)

## Usage

To use this gem, add it to your `build_config.rb` or `mrbgem.rake` file. For detailed examples of each method, please refer to the comments in the source code (`mrblib/array.rb` and `src/array.c`).

Example:

```ruby
a = [1, 2, 2, 3, nil, 4]
p a.compact #=> [1, 2, 2, 3, 4]
p a.uniq    #=> [1, 2, 3, nil, 4]

b = ["a", "b", "c"]
p b.rotate #=> ["b", "c", "a"]
```

This gem is part of the mruby project.
