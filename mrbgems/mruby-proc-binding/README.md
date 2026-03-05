# mruby-proc-binding

This mrbgem provides a `binding` method for `Proc` objects in mruby. This method returns a `Binding` object, which allows you to inspect and manipulate the lexical scope (local variables and `self`) of the `Proc` from which it was obtained.

## Usage Examples

Here's how you can use `mruby-proc-binding`:

### Getting a Binding

First, obtain a `Binding` object from a `Proc`:

```ruby
def create_proc
  a = 10
  b = 20
  ->(c) { a + b + c } # A sample proc
end

my_proc = create_proc
# Create a proc that takes one argument (c) and closes over a and b.
# Note: For the binding to capture local variables like 'a' and 'b',
# they must be referenced by a Proc defined in the same scope.
# A simple `proc {}` or `->{}` might not capture them as expected
# unless they are used within that proc.
# To ensure 'a' and 'b' are part of the environment captured by the binding
# from `my_proc.binding`, we ensure `my_proc` itself uses them or is defined
# in a scope where they are present and then `binding` is called on a proc
# that has access to this environment.

# Let's define a new proc specifically for getting a binding
# that captures the desired local variables.
local_var_scope_proc = nil
a_val = 1
b_val = 2
local_var_scope_proc = -> {
  # 'a_val' and 'b_val' are now in this proc's lexical scope
}
bind = local_var_scope_proc.binding
```

### Inspecting Local Variables

You can list the names of local variables within the proc's scope:

```ruby
# Continuing from the previous example:
# Variables 'a_val', 'b_val', and 'local_var_scope_proc' are expected.
p bind.local_variables
# Expected output (order may vary):
# => [:a_val, :b_val, :local_var_scope_proc, :bind]
# Note: The binding itself and other variables defined in the
# same scope might also be listed.
```

### Getting Local Variable Values

Retrieve the value of a specific local variable:

```ruby
p bind.local_variable_get(:a_val)  # => 1
p bind.local_variable_get(:b_val)  # => 2
```

### Setting Local Variable Values

You can also modify the values of local variables within the binding's scope:

```ruby
bind.local_variable_set(:a_val, 100)
p bind.local_variable_get(:a_val)  # => 100

# This also affects evaluation within the binding
p bind.eval("a_val + b_val")          # => 102 (100 + 2)
```

### Evaluating Code

Execute arbitrary Ruby code within the binding's context:

```ruby
# Define some variables in a scope
def get_binding_for_eval
  x = 5
  y = 10
  proc {}.binding # Create a binding in this scope
end

eval_bind = get_binding_for_eval
p eval_bind.eval("x * y")      # => 50
p eval_bind.eval("self")       # Shows the receiver of the proc
```

### Getting Source Location

If `mruby-proc-ext` is also available, you can get the source location of a proc:

```ruby
# Assuming __FILE__ is "test.rb" and this line is line 50
my_lambda = -> { } # This proc is defined at [__FILE__, __LINE__]
location_binding = my_lambda.binding

# This requires mruby-proc-ext
if location_binding.respond_to?(:source_location)
  p location_binding.source_location # => ["test.rb", 50] (approximately)
else
  puts "Binding#source_location not available. Is mruby-proc-ext included?"
end
```

A more robust example for capturing variables for `Proc#binding`:

```ruby
def execution_binding
  name = "Ruby"
  # The proc must be created in the scope where variables exist
  # and it doesn't necessarily need to *use* them for them to be available in the binding.
  # However, the most reliable way to ensure they are part of the environment
  # is if the proc is created in that environment.
  binding_proc = proc {} # This proc is created in the current scope
  binding_proc.binding   # Returns a binding for this scope
end

b = execution_binding
p b.local_variables.sort  # => [:b, :binding_proc, :name] (or similar)
p b.local_variable_get(:name) # => "Ruby"
b.local_variable_set(:name, "mruby")
p b.eval("name") # => "mruby"
```

## Dependencies

This mrbgem has the following dependencies:

- **`mruby-binding`**: Provides the core `Binding` object functionality. This gem is a prerequisite.
- **`mruby-proc-ext`**: Required for the `Binding#source_location` method. If `mruby-proc-ext` is not included in your mruby build, the `source_location` method may not be available on binding objects, or it might return `nil`.

## License

MIT
