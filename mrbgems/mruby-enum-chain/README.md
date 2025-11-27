# mruby-enum-chain

## Description

This mrbgem provides the `Enumerator::Chain` class, which allows you to chain multiple enumerable objects together, treating them as a single, continuous enumerable.

This is useful when you have multiple collections (e.g., arrays, ranges, or other enumerators) and you want to iterate over all of them sequentially without first concatenating them into a single, larger collection.

## How to Use

There are three main ways to create an `Enumerator::Chain` instance:

### 1. Using `Enumerable#chain`

You can call the `chain` method on any object that includes the `Enumerable` module.

```ruby
a = [1, 2, 3]
b = (4..6)
c = {foo: 7, bar: 8}.each_key # Enumerator for keys

chained_enum = a.chain(b, c)
chained_enum.to_a # => [1, 2, 3, 4, 5, 6, :foo, :bar]
```

### 2. Using `Enumerator#+`

You can use the `+` operator on an `Enumerator` instance to chain it with another enumerable object.

```ruby
enum1 = [1, 2].each
enum2 = %w(a b).each

chained_enum = enum1 + enum2
chained_enum.to_a # => [1, 2, "a", "b"]

# You can chain multiple times
enum3 = (10..11).each
chained_enum_2 = enum1 + enum2 + enum3
chained_enum_2.to_a # => [1, 2, "a", "b", 10, 11]
```

### 3. Using `Enumerator::Chain.new`

You can directly instantiate `Enumerator::Chain` by passing enumerable objects to its constructor.

```ruby
arr = [10, 20]
rng = (30..31)

chained_enum = Enumerator::Chain.new(arr, rng)
chained_enum.to_a # => [10, 20, 30, 31]
```

## Key Features

- **`each(&block)`**: Iterates through each element of the chained enumerables in the order they were added. Returns an enumerator if no block is given.
- **`size`**: Returns the total number of elements in all chained enumerables. If any of the chained enumerables do not respond to `size` (e.g., an infinite enumerator or one with an unknown size), this method will return `nil`.

  ```ruby
  ([1, 2].chain([3, 4])).size # => 4
  ([1, 2].chain( (1..Float::INFINITY) )).size # => nil
  ```

- **`rewind`**: Rewinds all of the chained enumerables that respond to the `rewind` method. This resets the iteration state to the beginning.

  ```ruby
  e = [1,2].chain(3..4)
  e.next # => 1
  e.next # => 2
  e.next # => 3
  e.rewind
  e.next # => 1
  ```

- **`+(other)`**: Creates a new `Enumerator::Chain` by appending another enumerable to the current chain.

## License

MIT License
