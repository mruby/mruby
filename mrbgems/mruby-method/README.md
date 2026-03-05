# mruby-method

The `mruby-method` mrbgem provides implementations for the `Method` and `UnboundMethod` classes. These classes allow for powerful introspection and manipulation of methods in mruby. This includes obtaining information like method name, owner, receiver, parameters, and source location, as well as features like creating method objects from existing methods, binding unbound methods to objects, and unbinding methods from their receivers.

```ruby
p Enumerable.instance_method(:find_all).source_location
#=> ["mruby/mruby/mrblib/enum.rb", 148]
```

# Note

You need to enable debug option in your build configuration to use
`source_location` method in this gem, for example:

```ruby
MRuby::Build.new do |conf|
  conf.enable_debug
end
```

# Supported Methods

## Kernel

- `Kernel#method(sym)`: Returns a Method object corresponding to the method identified by `sym` for the receiver.
- `Kernel#singleton_method(sym)`: Returns a Method object corresponding to the singleton method identified by `sym` for the receiver.

## Module

- `Module#instance_method(sym)`: Returns an UnboundMethod object corresponding to the instance method identified by `sym` in the module/class.

## Method class

- `Method#name`: Returns the name of the method.
- `Method#owner`: Returns the class or module that defines the method.
- `Method#receiver`: Returns the object to which the method is bound.
- `Method#arity`: Returns an integer indicating the number of arguments accepted by the method. A negative number indicates optional arguments.
- `Method#parameters`: Returns an array of arrays, each describing a parameter (e.g., `[[:req, :foo]]`).
- `Method#source_location`: Returns a two-element array `[filename, line_number]` indicating where the method was defined in Ruby code, or `nil` if not available. (Requires debug mode).
- `Method#call(*args, **opts, &block)`: Invokes the method with the given arguments and block, returning the result.
- `Method#[](*args, **opts, &block)`: Alias for `call`. Invokes the method.
- `Method#super_method`: Returns a Method object representing the superclass's version of this method. Returns `nil` if no superclass method exists.
- `Method#unbind`: Returns an UnboundMethod object based on this method.
- `Method#to_proc`: Converts the Method object into a Proc object that can be called.
- `Method#<< (other_method_or_proc)`: Returns a new Proc that represents the composition of this method and another method or proc (`self(other(...))`).
- `Method#>> (other_method_or_proc)`: Returns a new Proc that represents the composition of another method or proc and this method (`other(self(...))`).
- `Method#== (other)` / `Method#eql?(other)`: Returns `true` if the method is the same as `other` (same receiver, owner, and proc/name).
- `Method#inspect`: Returns a string representation of the Method object, including its class, name, owner, and source location if available. (Alias for `to_s`).

## UnboundMethod class

- `UnboundMethod#name`: Returns the name of the method.
- `UnboundMethod#owner`: Returns the class or module that originally defined the method.
- `UnboundMethod#arity`: Returns an integer indicating the number of arguments accepted by the method. A negative number indicates optional arguments.
- `UnboundMethod#parameters`: Returns an array of arrays, each describing a parameter (e.g., `[[:req, :foo]]`).
- `UnboundMethod#source_location`: Returns a two-element array `[filename, line_number]` indicating where the method was defined in Ruby code, or `nil` if not available. (Requires debug mode).
- `UnboundMethod#bind(obj)`: Binds the UnboundMethod to `obj` and returns a Method object. Raises `TypeError` if `obj` is not an instance of the method's owner or its descendants.
- `UnboundMethod#bind_call(obj, *args, **opts, &block)`: Binds the UnboundMethod to `obj` and then calls it with the given arguments and block.
- `UnboundMethod#super_method`: Returns an UnboundMethod object representing the superclass's version of this method. Returns `nil` if no superclass method exists.
- `UnboundMethod#== (other)` / `UnboundMethod#eql?(other)`: Returns `true` if the unbound method is the same as `other` (same owner and proc/name).
- `UnboundMethod#inspect`: Returns a string representation of the UnboundMethod object, including its class, name, owner, and source location if available. (Alias for `to_s`).

# See also

- <https://ruby-doc.org/core-2.3.3/Method.html>
- <https://ruby-doc.org/core-2.3.3/UnboundMethod.html>
