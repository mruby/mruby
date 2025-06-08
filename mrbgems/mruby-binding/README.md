# mruby-binding

The `mruby-binding` mrbgem provides the `Binding` class for mruby. This class allows you to encapsulate the execution context (variables, methods, and `self`) at a particular point in your code, making it available for later use. It is similar in purpose to the `Binding` class in standard Ruby.

## Obtaining a Binding Object

You can obtain a `Binding` object using the `Kernel#binding` method:

### `Kernel#binding` -> Binding

Returns a `Binding` object that encapsulates the execution context (including local variables, `self`, and any active block) at the point of the call.

Example:

```ruby
def get_binding(param)
  local_var = 42
  binding # This will capture param, local_var, and self
end

b = get_binding("hello")
# b now holds the context from inside get_binding
```

**Note:** `Kernel#binding` cannot be called from a C function or a Proc defined in C. Attempting to do so will raise a `RuntimeError`.

## `Binding` Class Methods

A `Binding` object has the following methods:

### `local_variables` -> Array of Symbol

Returns an array of symbols representing the names of the local variables defined in the binding's context.

Example:

```ruby
def my_method
  a = 10
  b = 20
  binding.local_variables # => [:a, :b]
end
```

### `local_variable_get(symbol)` -> Object

Retrieves the value of the local variable named by `symbol`. Raises a `NameError` if the variable is not defined in the binding's context.

Example:

```ruby
def my_method
  a = 10
  b = binding
  b.local_variable_get(:a) # => 10
end
```

### `local_variable_set(symbol, value)` -> Object

Sets the local variable named by `symbol` to `value`. If the variable is not already defined, it will be defined in the binding's scope. Returns the `value` that was set.

Example:

```ruby
def my_method
  a = 10
  b = binding
  b.local_variable_set(:a, 20) # a is now 20
  b.local_variable_set(:c, 30) # c is now defined as 30 in this scope
  a # => 20
  c # => 30
end
```

### `local_variable_defined?(symbol)` -> Boolean

Returns `true` if the local variable named by `symbol` is defined in the binding's context, `false` otherwise.

Example:

```ruby
def my_method
  a = 10
  b = binding
  b.local_variable_defined?(:a) # => true
  b.local_variable_defined?(:c) # => false
end
```

### `receiver` -> Object

Returns the receiver object (`self`) of the binding.

Example:

```ruby
class MyClass
  def get_binding
    @x = "instance var"
    binding
  end
end

obj = MyClass.new
b = obj.get_binding
b.receiver # => obj (the instance of MyClass)
b.receiver.instance_variable_get(:@x) # => "instance var" (Not directly using eval)
```

### `source_location` -> [String, Integer] | nil

Returns a two-element array containing the filename and line number where the binding was created. Returns `nil` if the source location cannot be determined (e.g., for bindings created from C).

Example:

```ruby
# In a file named 'test.rb'
b = binding # Assuming this is line 2
b.source_location # => ["test.rb", 2] (approximately)
```

### `dup` / `clone` -> Binding

Creates a shallow copy of the binding. Modifications to local variables in one binding object can affect the other if the variables themselves are mutable objects, but setting a variable in one binding will not create it in the other after duplication. The internal state concerning local variable storage is also duplicated.

(The C code refers to `binding_initialize_copy`, which is what `dup` and `clone` would use.)

```ruby
def my_method
  x = 1
  original_binding = binding
  original_binding.local_variable_set(:y, 2)

  copied_binding = original_binding.dup

  original_binding.local_variable_set(:x, 10)
  original_binding.local_variable_set(:y, 20)
  original_binding.local_variable_set(:z, 30) # New variable in original

  puts copied_binding.local_variable_get(:x) # => 1 (Original value before duplication for variables existing at duplication time)
                                             # Correction: The tests show that changes to existing variables are reflected.
                                             # Let's re-verify test behavior for `dup`.

  # Re-checking test `Binding#dup`:
  # x = 5
  # bind1 = binding
  # bind1.local_variable_set(:y, 10)
  # bind2 = bind1.dup
  # assert_equal 5, bind2.local_variable_get(:x)
  # assert_equal 10, bind2.local_variable_get(:y)
  # x = 50 # x is changed in the original scope AFTER duplication
  # assert_equal 50, bind1.local_variable_get(:x)
  # assert_equal 50, bind2.local_variable_get(:x) # bind2 sees the change to x!
  # bind1.local_variable_set(:y, 20) # y is changed in bind1 AFTER duplication
  # assert_equal 20, bind1.local_variable_get(:y)
  # assert_equal 20, bind2.local_variable_get(:y) # bind2 sees the change to y!
  # bind1.local_variable_set(:z, 30) # z is added to bind1
  # assert_raise(NameError) { bind2.local_variable_get(:z) } # bind2 does not see new z
  # bind2.local_variable_set(:z, 40) # z is added to bind2
  # assert_equal 30, bind1.local_variable_get(:z)
  # assert_equal 40, bind2.local_variable_get(:z)

  # Corrected explanation for dup/clone:
  # Creates a copy of the binding. Both the original and copied bindings share the same
  # underlying environment for local variables that existed at the time of duplication.
  # This means:
  # - If a variable that existed when `dup` was called is modified (either in the original
  #   scope or via `local_variable_set` on either binding), the change is visible in both bindings.
  # - If a *new* local variable is added to one binding using `local_variable_set` *after*
  #   duplication, it is not visible in the other binding.
end
```

**Note on `eval`:** While the `Binding` object is often used with `eval` in standard Ruby to execute code within the binding's context, `mruby-binding` itself does not provide an `eval` method directly on the `Binding` object. You would typically use `Binding` with mruby's core `eval` method if you need to evaluate a string of code within a captured context. The `mruby-binding` gem provides the necessary infrastructure (like `mrb_binding_extract_proc` and `mrb_binding_extract_env` in C) that can be utilized by an `eval` implementation.

```ruby
# Conceptual example (actual eval might vary based on mruby core)
def my_method
  a = 10
  b = binding
  # eval("puts a", b) # => would print 10
  # eval("a = 20", b) # a in my_method's scope would become 20
end
```

## Usage Example

Here's a more complete example demonstrating some of the `Binding` object's capabilities:

```ruby
class Greeter
  def initialize(name)
    @name = name
  end

  def get_binding_for_greeting(additional_message)
    greeting_type = "Hello"
    # Capture the binding here
    binding
  end
end

# Create an instance and get a binding
greeter_instance = Greeter.new("World")
captured_binding = greeter_instance.get_binding_for_greeting("Have a nice day!")

# Access the receiver (self)
puts "Receiver: #{captured_binding.receiver}"
# => Receiver: #<Greeter:0x...> (actual object id will vary)
puts "Receiver's name: #{captured_binding.receiver.instance_variable_get(:@name)}"
# => Receiver's name: World

# List local variables in the binding
puts "Local variables: #{captured_binding.local_variables.inspect}"
# => Local variables: [:additional_message, :greeting_type] (order may vary)

# Get local variable values
puts "Greeting type: #{captured_binding.local_variable_get(:greeting_type)}"
# => Greeting type: Hello
puts "Additional message: #{captured_binding.local_variable_get(:additional_message)}"
# => Additional message: Have a nice day!

# Set a local variable within the binding's context
captured_binding.local_variable_set(:greeting_type, "Hi")
puts "New greeting type: #{captured_binding.local_variable_get(:greeting_type)}"
# => New greeting type: Hi

# Check if a variable is defined
puts "Is 'greeting_type' defined? #{captured_binding.local_variable_defined?(:greeting_type)}"
# => Is 'greeting_type' defined? true
puts "Is 'non_existent_var' defined? #{captured_binding.local_variable_defined?(:non_existent_var)}"
# => Is 'non_existent_var' defined? false

# Source location (will vary based on where the code is run)
location = captured_binding.source_location
if location
  puts "Binding created at: #{location[0]}:#{location[1]}"
else
  puts "Source location not available for this binding."
end
```

This example illustrates how a `Binding` object captures the state of local variables and `self` from the scope where it was created, and how these can be inspected and manipulated.

## Limitations and mruby-specific Considerations

- **Nesting Depth for Local Variables:** mruby has an internal limit on how deeply nested Procs (blocks) can be while still allowing the `Binding` object to access and manage their local variables. This limit is defined by the `BINDING_UPPER_MAX` constant (defaulting to 20, with a minimum of 10 and a maximum of 100, configurable at compile time via `MRB_BINDING_UPPER_MAX`). If you exceed this nesting depth, attempting to create or manipulate a binding that needs to access variables across too many Proc scopes might result in a `RuntimeError` ("too many upper procs for local variables").

- **`eval` Method:** As noted earlier, this gem provides the `Binding` object itself, not a `Binding#eval` method. You would use `Kernel.eval(string, binding)` if you need to evaluate code within the context of a binding, relying on mruby's core `eval` capabilities.

- **C Function Callers:** `Kernel#binding` cannot create a `Binding` object if the direct caller is a C function. It must be called from Ruby code.

```

```
