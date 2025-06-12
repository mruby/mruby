# mruby-objectspace

This mrbgem provides the `ObjectSpace` module for mruby, allowing introspection of live objects within the mruby environment.

## Purpose

The `ObjectSpace` module offers methods to count and iterate over objects currently allocated by the mruby interpreter. This can be useful for debugging, memory profiling, or understanding the internal state of your mruby application.

## Functionality

### `ObjectSpace.count_objects([result_hash]) -> Hash`

Counts all objects currently alive in the mruby process, categorized by their types.

- **`result_hash` (optional `Hash`):** If provided, this hash will be cleared and populated with the results. This can be useful to avoid allocating a new hash during sensitive operations like memory profiling, potentially reducing probe effect.

- **Returns:** A `Hash` where keys are symbols representing object types (e.g., `:T_OBJECT`, `:T_CLASS`, `:T_STRING`) or, in some cases, internal type integers. The values are the counts of objects for each type. The hash also includes special keys:
  - `:TOTAL`: The total number of objects (including free slots).
  - `:FREE`: The number of free (unallocated) object slots.

**Example of the returned hash structure:**

```ruby
{
  :TOTAL=>10000,
  :FREE=>3011,
  :T_OBJECT=>6,
  :T_CLASS=>404,
  :T_STRING=>500,
  # ... other types
}
```

### `ObjectSpace.each_object([module]) {|obj| ... } -> Fixnum`

Iterates over each live object in the mruby process, calling the provided block once for each object.

- **`module` (optional `Module` or `Class`):** If this argument is provided, the block will only be called for objects that are instances of the given `module` or one of its subclasses.

- **Block (`{|obj| ... }`):** A block that will be executed with each object (`obj`) found.

- **Returns:** A `Fixnum` representing the total number of objects iterated over.

**Example Usage:**

```ruby
# Iterate over all objects
total_objects = ObjectSpace.each_object do |obj|
  # Do something with obj
  p obj
end
puts "Total objects found: #{total_objects}"

# Iterate only over Array objects
array_count = ObjectSpace.each_object(Array) do |arr|
  p arr
end
puts "Found #{array_count} Arrays."
```
