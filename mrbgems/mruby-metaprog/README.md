# mruby-metaprog

This mrbgem provides a collection of methods for metaprogramming in mruby. Metaprogramming is the ability of a program to inspect, modify, and create its own code at runtime.

## Functionality

The `mruby-metaprog` mrbgem offers methods for:

- **Instance Variable Manipulation:**
  - `instance_variable_defined?(symbol)`: Checks if an instance variable is defined in an object.
  - `instance_variable_get(symbol)`: Retrieves the value of an instance variable.
  - `instance_variable_set(symbol, value)`: Sets the value of an instance variable.
  - `instance_variables()`: Returns an array of instance variable names in an object.
- **Method Inspection:**
  - `methods(regular=true)`: Returns a list of public and protected methods of an object.
  - `private_methods(all=true)`: Returns a list of private methods accessible to an object.
  - `protected_methods(all=true)`: Returns a list of protected methods accessible to an object.
  - `public_methods(all=true)`: Returns a list of public methods accessible to an object.
  - `singleton_methods(all=true)`: Returns an array of singleton method names for an object.
- **Singleton Method Definition:**
  - `define_singleton_method(name, &block)`: Defines a singleton method for an object.
- **Class Variable Manipulation:**
  - `class_variable_defined?(symbol)`: Checks if a class variable is defined in a module.
  - `class_variable_get(symbol)`: Retrieves the value of a class variable.
  - `class_variable_set(symbol, value)`: Sets the value of a class variable.
  - `class_variables(inherit=true)`: Returns an array of class variable names in a module.
  - `remove_class_variable(symbol)`: Removes a class variable from a module.
- **Module Inspection:**
  - `included_modules()`: Returns a list of modules included in a module or class.
  - `instance_methods(include_super=true)`: Returns an array of public and protected instance methods in a module or class.
  - `public_instance_methods(include_super=true)`: Returns an array of public instance methods in a module or class.
  - `private_instance_methods(include_super=true)`: Returns an array of private instance methods in a module or class.
  - `protected_instance_methods(include_super=true)`: Returns an array of protected instance methods in a module or class.
  - `undefined_instance_methods()`: Returns an array of undefined instance methods in a module/class.
- **Method Removal:**
  - `remove_method(symbol)`: Removes a method from a class.
- **Constant Inspection:**
  - `constants(inherit=true)`: Returns an array of constant names defined in a module.
- **Module Nesting:**
  - `Module.nesting()`: Returns an array representing the current module nesting.
- **Message Sending:**
  - `send(symbol, *args, &block)`: Invokes a method on an object.
  - `public_send(symbol, *args, &block)`: Invokes a public method on an object.
- **Variable Inspection:**
  - `global_variables()`: Returns an array of global variable names.
  - `local_variables()`: Returns an array of local variable names in the current scope.

## Examples

### Working with Instance Variables

```ruby
class MyClass
  def initialize(value)
    @my_var = value
  end
end

obj = MyClass.new(10)

p obj.instance_variable_defined?(:@my_var)  #=> true
p obj.instance_variable_get(:@my_var)     #=> 10
obj.instance_variable_set(:@another_var, "hello")
p obj.instance_variables                   #=> [:@my_var, :@another_var]
```

### Inspecting Methods

```ruby
class AnotherClass
  def public_method; end
  protected
  def protected_method; end
  private
  def private_method; end
end

obj = AnotherClass.new
p obj.public_methods(false)    #=> [:public_method]
p obj.protected_methods(false) #=> [:protected_method]
p obj.private_methods(false)   #=> [:private_method]

def obj.singleton_method_example; end
p obj.singleton_methods        #=> [:singleton_method_example]
```

### Defining Singleton Methods

```ruby
obj = Object.new
obj.define_singleton_method(:greet) do |name|
  "Hello, #{name}!"
end
p obj.greet("World")  #=> "Hello, World!"
```

### Working with Class Variables

```ruby
class ParentClass
  @@parent_cvar = 100
end

class ChildClass < ParentClass
  @@child_cvar = 200

  def self.get_parent_cvar
    class_variable_get(:@@parent_cvar)
  end
end

p ChildClass.class_variable_defined?(:@@child_cvar)  #=> true
p ChildClass.class_variable_get(:@@child_cvar)     #=> 200
ChildClass.class_variable_set(:@@new_cvar, 300)
p ChildClass.class_variables.sort                   #=> [:@@child_cvar, :@@new_cvar, :@@parent_cvar] (order may vary)
p ChildClass.get_parent_cvar                        #=> 100
ChildClass.remove_class_variable(:@@new_cvar)
p ChildClass.class_variables.sort                   #=> [:@@child_cvar, :@@parent_cvar] (order may vary)
```

## License

This mrbgem is licensed under the MIT License.
