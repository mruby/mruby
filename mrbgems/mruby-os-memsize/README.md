# mruby-os-memsize

## Description

The `mruby-os-memsize` mrbgem extends the `ObjectSpace` module in mruby, providing tools to inspect the approximate amount of heap memory allocated for objects. This can be useful for debugging memory usage and understanding the memory footprint of your mruby application.

## Usage / API

This gem adds two class methods to the `ObjectSpace` module:

### `ObjectSpace.memsize_of(obj) -> Numeric`

Returns the approximate amount of heap memory allocated for the given `obj` in bytes.

- The returned value is platform-dependent, as it's based on the `size_t` type of the underlying C implementation.
- Immediate values (e.g., integers, booleans, symbols) and some small, embedded objects (like short strings or small arrays that fit directly into an object pointer) will typically report a size of 0 or a very small, fixed size representing the object pointer itself.
- The accuracy of the reported size can vary depending on the object's type and internal structure.

**Example:**

```ruby
str = "This is a test string"
array = [1, 2, 3, 4, 5]

puts "Size of string: #{ObjectSpace.memsize_of(str)} bytes"
puts "Size of array: #{ObjectSpace.memsize_of(array)} bytes"
puts "Size of 123: #{ObjectSpace.memsize_of(123)}" # Likely 0
puts "Size of :symbol: #{ObjectSpace.memsize_of(:symbol)}" # Likely 0

class MyClass
  def initialize
    @data = "some internal data"
  end
end
instance = MyClass.new
puts "Size of MyClass instance: #{ObjectSpace.memsize_of(instance)}"
```

### `ObjectSpace.memsize_of_all([klass]) -> Numeric`

Returns the total approximate heap memory allocated for all living objects in the mruby environment.

- If an optional `klass` argument (a Class object) is provided, it returns the total memory size only for instances of that specific class.

**Example:**

```ruby
# Get total memory size of all objects
total_memory = ObjectSpace.memsize_of_all
puts "Total heap memory used by all objects: #{total_memory} bytes"

# Get total memory size for all String objects
total_string_memory = ObjectSpace.memsize_of_all(String)
puts "Total heap memory used by Strings: #{total_string_memory} bytes"

class Person
  def initialize(name)
    @name = name
  end
end

p1 = Person.new("Alice")
p2 = Person.new("Bob")

total_person_memory = ObjectSpace.memsize_of_all(Person)
puts "Total heap memory used by Person instances: #{total_person_memory} bytes"
```

## License

This mrbgem is released under the MIT License. (See `mrbgem.rake` for details within the mruby distribution).

## Author

mruby developers
