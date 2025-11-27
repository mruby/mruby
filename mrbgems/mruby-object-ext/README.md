# mruby-object-ext

This mrbgem provides several extension methods to core objects in mruby, enhancing their functionality and providing useful utilities for common programming patterns.

## Core Extension Methods

This gem extends core mruby objects with the following methods:

### `Kernel#yield_self` (aliased as `then`)

Yields the receiver (self) to the given block and returns the result of the block. This is useful for chaining operations in a readable way.

**Signature:**

```ruby
obj.yield_self {|current_obj| ... } > an_object
obj.then       {|current_obj| ... } -> an_object
```

**Example:**

```ruby
result = "hello"
  .yield_self {|s| s.upcase }
  .then       {|s| s + " WORLD" }

puts result #=> "HELLO WORLD"

# Another example with a different data type
(1..5)
  .yield_self {|range| range.to_a }
  .then       {|arr| arr.select(&:even?) }
  .then       {|evens| evens.map {|n| n * n } }
  .then       {|squares| puts squares.inspect } #=> [4, 16]
```

### `Kernel#tap`

Yields the receiver (self) to the given block and then returns the receiver itself. This method is primarily used for "tapping into" a method chain to perform operations on intermediate results without affecting the final result of the chain.

**Signature:**

```ruby
obj.tap {|current_obj| ... } -> obj
```

**Example:**

```ruby
(1..10)
  .to_a
  .tap {|arr| puts "Array: #{arr.inspect}" }
  .select(&:even?)
  .tap {|evens| puts "Evens: #{evens.inspect}" }
  .map {|n| n * n }
  .tap {|squares| puts "Squares: #{squares.inspect}" }
# Output:
# Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# Evens: [2, 4, 6, 8, 10]
# Squares: [4, 16, 36, 64, 100]
```

### `Kernel#itself`

Returns the receiver (self). While simple, it can be useful in certain chaining or meta-programming scenarios.

**Signature:**

```ruby
obj.itself -> obj
```

**Example:**

```ruby
string = "my string"
puts string.itself.object_id == string.object_id #=> true

# Can be useful with methods expecting a callable
data = { a: 1, b: 2 }
transformed_data = data.transform_values(&:itself) # No change if values are already what you want
puts transformed_data #=> {:a=>1, :b=>2} (in mruby hash output format)
```

### `BasicObject#instance_exec`

Executes the given block within the context of the receiver (obj). This means that inside the block, `self` is set to `obj`, giving the code access to `obj`'s instance variables and private methods. Arguments passed to `instance_exec` are passed as block parameters.

**Signature:**

```ruby
obj.instance_exec(arg...) {|var...| block } -> result_of_block
```

**Example:**

```ruby
class MyClass
  def initialize(value)
    @secret = value
  end

  def reveal_secret(multiplier, &block)
    # self here is MyClass instance
    instance_exec(multiplier, &block)
  end
end

instance = MyClass.new(10)
result = instance.reveal_secret(5) {|m| @secret * m }
puts result #=> 50

# Example without a class context
num = 42
num.instance_exec("Number is: ") {|prefix| puts prefix + self.to_s } #=> Number is: 42
```

## `NilClass` Extensions

This gem also adds convenient type conversion methods to `NilClass`:

- **`nil.to_a`**: Returns an empty Array (`[]`).

  ```ruby
  p nil.to_a #=> []
  ```

- **`nil.to_f`**: Returns `0.0`.

  ```ruby
  p nil.to_f #=> 0.0
  ```

- **`nil.to_h`**: Returns an empty Hash (`{}`).

  ```ruby
  p nil.to_h #=> {}
  ```

- **`nil.to_i`**: Returns `0`.

  ```ruby
  p nil.to_i #=> 0
  ```
