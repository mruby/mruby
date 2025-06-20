# mruby-kernel-ext

This mrbgem extends the `Kernel` module in mruby with additional useful methods.

## Methods

### `fail(*args)`

Raises a `RuntimeError`. This is an alias for `raise`.

Example:

```ruby
fail "Something went wrong"
# Raises RuntimeError: Something went wrong
```

### `caller(start=1, length=nil) -> array | nil`

### `caller(range) -> array | nil`

Returns the current execution stack (backtrace).

- If `start` is provided, it indicates the number of frames to skip.
- If `length` is provided, it limits the number of frames returned.
- If a `range` is provided, it specifies the portion of the stack to return.

Returns `nil` if `start` is greater than or equal to the number of frames in the stack.

Example:

```ruby
def foo
  bar
end

def bar
  puts caller(0) # Show all frames starting from the current one
  puts caller(1) # Skip one frame
end

foo
```

### `__method__ -> symbol | nil`

Returns the name of the current method as a `Symbol`. If called outside of a method, it returns `nil`.

Example:

```ruby
class MyClass
  def my_method
    puts __method__
  end
end

MyClass.new.my_method
# Output: :my_method
```

### `__callee__ -> symbol | nil`

Returns the called name of the current method as a `Symbol`. If called outside of a method, it returns `nil`. This can be different from `__method__` when using aliases.

Example:

```ruby
class MyClass
  def original_method
    puts __callee__
  end

  alias aliased_method original_method
end

obj = MyClass.new
obj.original_method # Output: :original_method
obj.aliased_method  # Output: :aliased_method
```

### `Integer(arg, base=0) -> integer`

Converts `arg` to an `Integer`.

- Numeric types are converted directly (floating-point numbers are truncated).
- If `arg` is a `String`, `base` (0, or between 2 and 36) is used as the base for conversion.
  - If `base` is omitted or zero, radix indicators (`0`, `0b`, `0x`) in the string are honored.
- Strings must strictly conform to numeric representation, unlike `String#to_i`.
- Passing `nil` raises a `TypeError`.

Examples:

```ruby
Integer(123.999)    #=> 123
Integer("0x1a")     #=> 26
Integer("0930", 10) #=> 930
Integer("111", 2)   #=> 7
# Integer(nil)        #=> TypeError
# Integer("invalid")  #=> ArgumentError
```

### `Float(arg) -> float`

Converts `arg` to a `Float`.

- Numeric types are converted directly.
- Other types are converted using `arg.to_f`.
- Passing `nil` raises a `TypeError`.

Examples:

```ruby
Float(1)           #=> 1.0
Float(123.456)     #=> 123.456
Float("123.456")   #=> 123.456
# Float(nil)         #=> TypeError
# Float("invalid")   #=> ArgumentError
```

### `String(arg) -> string`

Converts `arg` to a `String` using its `to_s` method.

Examples:

```ruby
String(self)        #=> "main"
String(self.class)  #=> "Object"
String(123456)      #=> "123456"
String(:symbol)     #=> "symbol"
```

### `Array(arg) -> array`

Converts `arg` to an `Array`.

- If `arg` responds to `to_a`, it calls `to_a` to convert.
- Otherwise, it returns a new array containing `arg` as its single element.

Examples:

```ruby
Array(1..5)          #=> [1, 2, 3, 4, 5]
Array([1, 2, 3])     #=> [1, 2, 3]
Array("hello")       #=> ["hello"] # If String does not have to_a
Array({ a: 1, b: 2 }) #=> [[:a, 1], [:b, 2]] # If Hash has to_a
```

### `Hash(arg) -> hash`

Converts `arg` to a `Hash`.

- If `arg` is already a `Hash`, it is returned.
- If `arg` is `nil` or an empty `Array`, an empty `Hash` is returned.
- Otherwise, it raises a `TypeError`.

Examples:

```ruby
Hash({ key: :value }) #=> { key: :value }
Hash(nil)             #=> {}
Hash([])              #=> {}
# Hash([1, 2, 3])       #=> TypeError
```
