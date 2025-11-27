# mruby-toplevel-ext

mruby-toplevel-ext is a gem that modifies the toplevel in mruby by making the `include` method private. This means modules must be included directly into `Object` (e.g., `Object.include MyModule`) rather than at the toplevel.

## Usage

After installing this gem, the `include` method at the toplevel (main) becomes private. This is the primary change provided by this gem.

Consider the following example:

```ruby
module MyModule
  def my_method
    puts "Hello from MyModule!"
  end
end

# This will result in a NoMethodError because include is private:
# include MyModule
# my_method

# Instead, you must include the module directly into Object:
Object.include MyModule
my_method
# => Hello from MyModule!
```

This encourages a more explicit way of extending the global object space.

## License

The gem is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).
