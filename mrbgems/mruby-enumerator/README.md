# mruby-enumerator

The `mruby-enumerator` mrbgem provides the `Enumerator` class, which allows for both internal and external iteration in mruby.

## Purpose and Functionality

Enumerators are useful when you want to work with collections of data in a flexible way. They allow you to:

- **Iterate externally:** Fetch elements one by one using methods like `next`.
- **Chain operations:** Combine multiple iteration steps (e.g., mapping and then selecting) without creating intermediate arrays.
- **Create custom iterators:** Define your own iteration logic.

## Creating an Enumerator

You can create an `Enumerator` in several ways:

1. **Using `Kernel#to_enum` or `Kernel#enum_for`:** This is the most common way. You can turn any object that has an `each` method (or a similar iteration method) into an Enumerator.

   ```ruby
   e = [1, 2, 3].to_enum  # Creates an enumerator for the array
   e = "hello".enum_for(:each_byte) # Creates an enumerator for iterating over bytes
   ```

2. **Using `Enumerator.new`:** You can create an Enumerator by providing a block that defines how values are yielded.

   ```ruby
   fib = Enumerator.new do |yielder|
     a = b = 1
     loop do
       yielder << a
       a, b = b, a + b
     end
   end

   fib.take(5) # => [1, 1, 2, 3, 5]
   ```

## Key Methods

The `Enumerator` class includes many helpful methods from the `Enumerable` module, as well as some of its own:

- **`next`**: Returns the next value from the enumerator. Raises `StopIteration` if the enumerator is at the end.
- **`peek`**: Returns the next value without advancing the iterator.
- **`rewind`**: Resets the enumerator to its beginning.
- **`with_index(offset = 0)`**: Iterates over elements, providing both the element and its index (with an optional offset). Returns a new Enumerator if no block is given.

  ```ruby
  ["a", "b"].each.with_index(1) do |char, index|
    puts "#{index}: #{char}"
  end
  # Output:
  # 1: a
  # 2: b
  ```

- **`each_with_index`**: Similar to `with_index(0)`.
- **`with_object(obj)`**: Iterates over elements, passing an arbitrary object along with each element. Returns the given object. Returns a new Enumerator if no block is given.

  ```ruby
  (1..3).each.with_object([]) do |i, arr|
    arr << i * 2
  end # => [2, 4, 6]
  ```

- **`each(*args)`**: Iterates over the enumerator. If arguments are provided, they are passed to the underlying iteration method.
- **`feed(value)`**: Sets a value to be returned by the `yield` inside the enumerator on its next call.

## Chaining Enumerators

One of the powerful features of Enumerators is the ability to chain operations:

```ruby
data = [1, 2, 3, 4, 5]

result = data.to_enum
            .with_index           # [[1, 0], [2, 1], [3, 2], [4, 3], [5, 4]]
            .select { |num, idx| num.even? } # [[2, 1], [4, 3]]
            .map { |num, idx| "#{idx}:#{num}" } # ["1:2", "3:4"]

p result # => ["1:2", "3:4"]
```

This avoids creating intermediate arrays for each step, making the code more efficient and readable for complex data transformations.

## Integration

This mrbgem integrates the `Enumerator` class into mruby, making it available for use in your mruby projects. It also extends `Kernel` with `to_enum` and `enum_for`, and `Enumerable` with methods like `zip`, `chunk`, and `chunk_while` that leverage `Enumerator`.

Refer to the source code and tests for more detailed examples and advanced usage.
