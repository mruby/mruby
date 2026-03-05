# mruby-data

The `mruby-data` mrbgem provides a way to define simple classes in mruby that act as data structures, similar to Ruby's `Struct` class. It allows you to bundle a number of attributes together and access them using accessor methods.

## Purpose

`mruby-data` is useful when you need to create lightweight objects that primarily serve to hold and provide access to a set of data attributes without the need to write a full class definition manually.

## Functionality

### Defining a Data Class

You can define a new data class using the `Data.define` class method. It accepts one or more symbols as arguments, which will become the attribute names for the instances of the generated class.

```ruby
# Defines a new class 'Point' with attributes :x and :y
Point = Data.define(:x, :y)

# Defines a new class 'Customer' with attributes :name, :address, and :zip
Customer = Data.define(:name, :address, :zip)
```

### Creating Instances

Once a data class is defined, you can create instances of it using the `new` method, providing values for each attribute in the order they were defined.

```ruby
# Create an instance of Point
point1 = Point.new(10, 20)

# Create an instance of Customer
customer1 = Customer.new("John Doe", "123 Main St", 12345)
```

Alternatively, you can pass keyword arguments (available if mruby is compiled with `MRB_KW_ARGS`):

```ruby
customer2 = Customer.new(name: "Jane Doe", address: "456 Oak Ave", zip: 67890)
```

### Accessing Attributes

Instances of data classes have accessor methods for each defined attribute.

```ruby
puts point1.x      # Output: 10
puts point1.y      # Output: 20

puts customer1.name  # Output: "John Doe"
```

### Instance Methods

Instances of classes created by `Data.define` have several useful methods:

- **`members`**: Returns an array of symbols representing the names of the attributes.

  ```ruby
  p customer1.members  # Output: [:name, :address, :zip]
  ```

- **`== (other)`**: Returns `true` if `other` is an instance of the same data class and all attribute values are equal.

  ```ruby
  point2 = Point.new(10, 20)
  point3 = Point.new(0, 0)

  puts point1 == point2  # Output: true
  puts point1 == point3  # Output: false
  ```

- **`eql?(other)`**: Similar to `==`, checks if `other` is an instance of the same data class and all attribute values are `eql?`.

- **`to_h`**: Converts the data instance into a hash where keys are the attribute symbols and values are their corresponding values.

  ```ruby
  p customer1.to_h
  # Output: {:name=>"John Doe", :address=>"123 Main St", :zip=>12345}
  ```

- **`to_s` / `inspect`**: Returns a string representation of the data instance.

  ```ruby
  puts customer1 # Output: #<data Customer name="John Doe", address="123 Main St", zip=12345>
  p customer1    # Output: #<data Customer name="John Doe", address="123 Main St", zip=12345>
  ```

## Freezing

By default, instances of data classes are frozen after initialization, meaning their attributes cannot be modified after creation.

```ruby
point = Point.new(1, 2)
# The following would raise an error as the object is frozen:
# point.x = 100
```

## Example Usage

```ruby
# Define a class for 2D vectors
Vector2D = Data.define(:x, :y)

# Create some vector instances
v1 = Vector2D.new(3, 4)
v2 = Vector2D.new(1, 5)

# Access attributes
puts "Vector v1: (#{v1.x}, #{v1.y})"
puts "Vector v2: (#{v2.x}, #{v2.y})"

# Use instance methods
puts "v1 members: #{v1.members}"
puts "v1 as hash: #{v1.to_h}"

# Comparison
v3 = Vector2D.new(3, 4)
puts "v1 == v2: #{v1 == v2}" # false
puts "v1 == v3: #{v1 == v3}" # true
```

This mrbgem simplifies the creation of simple value objects in mruby.

```

```
