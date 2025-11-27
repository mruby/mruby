# mruby-struct

This mrbgem provides the `Struct` class, a convenient way to bundle a number of attributes together, much like Ruby's core `Struct` class. It allows you to create simple classes (structs) with a defined set of accessor methods for these attributes.

## Functionality

- Define new Struct classes with a specific set of members.
- Create instances of these Structs with positional or keyword arguments.
- Support for keyword initialization mode (`keyword_init` option).
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

## Keyword Initialization

As of mruby 3.4, Struct supports keyword initialization through the `keyword_init` option:

```ruby
# Create a Struct with keyword initialization enabled
Person = Struct.new(:name, :age, keyword_init: true)

# Must use keyword arguments when keyword_init: true
person = Person.new(name: "Alice", age: 30)
puts person.name  # Output: Alice
puts person.age   # Output: 30

# Can create with partial keywords (missing values are nil)
person2 = Person.new(name: "Bob")
puts person2.name  # Output: Bob
puts person2.age   # Output: nil

# Empty initialization is allowed
person3 = Person.new
puts person3.name  # Output: nil

# Positional arguments will raise an error when keyword_init: true
# Person.new("Charlie", 25)  # ArgumentError: wrong arguments, expected keyword arguments
```

### Keyword Initialization Modes

The `keyword_init` option supports three modes:

1. **`keyword_init: true`** - Only keyword arguments are accepted
2. **`keyword_init: false`** - Only positional arguments are accepted (hashes are treated as values)
3. **`keyword_init: nil` (default)** - Flexible mode: accepts both positional arguments and keyword arguments (single hash)

```ruby
# Flexible mode (default behavior)
FlexPoint = Struct.new(:x, :y)
p1 = FlexPoint.new(1, 2)           # Positional arguments
p2 = FlexPoint.new(x: 3, y: 4)     # Keyword arguments (single hash)

# Keyword-only mode
KeywordPoint = Struct.new(:x, :y, keyword_init: true)
p3 = KeywordPoint.new(x: 5, y: 6)  # Only keyword arguments allowed

# Positional-only mode
PositionalPoint = Struct.new(:x, :y, keyword_init: false)
p4 = PositionalPoint.new(7, 8)     # Only positional arguments
p5 = PositionalPoint.new({x: 9, y: 10})  # Hash is treated as first value
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
