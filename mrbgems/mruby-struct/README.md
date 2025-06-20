# mruby-struct

This mrbgem provides the `Struct` class, a convenient way to bundle a number of attributes together, much like Ruby's core `Struct` class. It allows you to create simple classes (structs) with a defined set of accessor methods for these attributes.

## Functionality

- Define new Struct classes with a specific set of members.
- Create instances of these Structs.
- Access and assign to struct members using accessor methods or by index/symbol.
- Iterate over members and values.
- Convert structs to Arrays or Hashes.

## Basic Usage

```ruby
# Define a new Struct class
Point = Struct.new(:x, :y)

# Create an instance of the Point struct
origin = Point.new(0, 0)

# Access attributes
puts origin.x  # Output: 0
puts origin[:y] # Output: 0

# Set attributes
origin.x = 10
origin[:y] = 20

puts origin.inspect # Output: #<struct Point x=10, y=20>

# Another example
Customer = Struct.new("Customer", :name, :address)
joe = Customer.new("Joe Smith", "123 Maple, Anytown NC")

puts joe.name    # Output: Joe Smith
puts joe.address # Output: 123 Maple, Anytown NC
```

## Available Methods

Instances of classes created with `Struct.new` have several useful methods, including:

- `members`: Returns an array of symbols representing the names of the instance variables.
- `each`: Calls a block for each attribute, passing the value.
- `each_pair`: Calls a block for each attribute, passing the name (symbol) and value.
- `select`: Returns an array containing values for which the block returns true.
- `to_a` / `values`: Returns an array containing the values of the struct.
- `to_h`: Returns a hash mapping member names (symbols) to their values.
- `length` / `size`: Returns the number of members in the struct.
- `dig`: Extracts a nested value specified by a sequence of keys.
- `==`, `eql?`: For comparing struct instances.

For more details on specific methods, please refer to the mruby documentation or the core Ruby `Struct` class documentation, as `mruby-struct` aims for compatibility.
