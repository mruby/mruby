# mruby-catch

This mrbgem provides `catch` and `throw` functionality similar to what is available in standard Ruby. It allows for non-local exits from blocks of code.

## `catch`

The `catch` method is used to establish a block that can be exited prematurely using `throw`.

```ruby
catch(tag) do |current_tag|
  # ... code ...
  if some_condition
    throw(tag, return_value)
  end
  # ... more code ...
end
```

- **With a tag:** When `catch` is called with a `tag` (any Ruby object), it executes the block. If `throw` is called with the same `tag` from within this block (or any method called from within it), the `catch` block immediately exits and returns the value provided to `throw`.
- **Block completion:** If the block executes to completion without `throw` being called with a matching tag, the `catch` block returns the result of the last expression evaluated in the block.
- **No tag:** If `catch` is called without a tag, a new unique `Object` is created and used as the tag. This tag is passed as an argument to the block.

```ruby
catch do |generated_tag|
  # generated_tag is a new Object
  throw(generated_tag, "hello")
end # => "hello"
```

## `throw`

The `throw` method is used to initiate a non-local exit to a corresponding `catch` block.

```ruby
throw(tag)
throw(tag, value)
```

- **With a tag and value:** `throw(tag, value)` jumps to the innermost active `catch` block that is waiting for `tag`. The `catch` block then returns `value`.
- **With only a tag:** `throw(tag)` is equivalent to `throw(tag, nil)`.
- **Uncaught throw:** If `throw` is called with a `tag` for which there is no matching `catch` block in the current call stack, an `UncaughtThrowError` is raised.

## `UncaughtThrowError`

This is a custom error class that inherits from `ArgumentError`. It is raised when `throw` is called for a tag that is not currently being caught.
It has two attributes:

- `tag`: The tag that was thrown.
- `value`: The value that was thrown with the tag.

## Example

```ruby
def check_value(val)
  puts "Checking: #{val}"
  if val < 0
    throw(:negative_value, val)
  elsif val == 0
    throw(:zero_value) # value will be nil
  end
  puts "#{val} is positive"
  val * 2
end

result = catch(:negative_value) do
  puts catch(:zero_value) do
    puts check_value(10)
    puts check_value(-5) # This will throw to :negative_value
    puts check_value(0)  # This would throw to :zero_value, but it's not reached
  end
  puts "This line is skipped if :zero_value is thrown."
end

puts "Result: #{result}"

# Output:
# Checking: 10
# 10 is positive
# 20
# Checking: -5
# Result: -5

puts "--- Next example --- "

result2 = catch do |tag_a|
  catch do |tag_b|
    puts "In tag_b block"
    throw(tag_a, "Exited from A via B")
    puts "This is not printed"
  end
  puts "This is not printed either"
end
puts result2 # => Exited from A via B
```
