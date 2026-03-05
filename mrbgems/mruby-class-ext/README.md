# mruby-class-ext

This mrbgem extends the `Module` and `Class` classes in mruby, providing additional methods for reflection and class manipulation.

## Module Methods

### `mod < other`

Returns `true` if `mod` is a subclass/submodule of `other`. Returns `false` if `mod` is the same as `other`. Returns `nil` if there's no relationship between the two. (Think of the relationship in terms of the class definition: "class A < B" implies "A < B".)

### `mod <= other`

Returns `true` if `mod` is a subclass/submodule of `other` or is the same as `other`. Returns `nil` if there's no relationship between the two. (Think of the relationship in terms of the class definition: "class A < B" implies "A <= B".)

### `mod > other`

Returns `true` if `mod` is an ancestor of `other`. Returns `false` if `mod` is the same as `other`. Returns `nil` if there's no relationship between the two. (Think of the relationship in terms of the class definition: "class A < B" implies "B > A".)

### `mod >= other`

Returns `true` if `mod` is an ancestor of `other`, or the two modules are the same. Returns `nil` if there's no relationship between the two. (Think of the relationship in terms of the class definition: "class A < B" implies "B >= A".)

### `module <=> other_module`

Comparison---Returns -1, 0, +1 or `nil` depending on whether `module` includes `other_module`, they are the same, or if `module` is included by `other_module`.

Returns `nil` if `module` has no relationship with `other_module`, if `other_module` is not a module, or if the two values are incomparable.

### `name`

Returns the name of the module.

### `singleton_class?`

Returns `true` if the module is a singleton class.

### `module_exec(arg...) {|var...| block } -> obj`

### `class_exec(arg...) {|var...| block } -> obj`

Evaluates the given block in the context of the class/module. The method defined in the block will belong to the receiver. Any arguments passed to the method will be passed to the block. This can be used if the block needs to access instance variables.

```ruby
class Thing
end
Thing.class_exec{
  def hello() "Hello there!" end
}
puts Thing.new.hello()
```

## Class Methods

### `subclasses -> array`

Returns an array of classes where the receiver is the direct superclass of the class, excluding singleton classes. The order of the returned array is not defined.

```ruby
class A; end
class B < A; end
class C < B; end
class D < A; end

A.subclasses        #=> [D, B]
B.subclasses        #=> [C]
C.subclasses        #=> []
```

### `attached_object -> object`

Returns the object for which the receiver is the singleton class. Raises a `TypeError` if the class is not a singleton class.

```ruby
class Foo; end

Foo.singleton_class.attached_object        #=> Foo
Foo.attached_object                        #=> TypeError: not a singleton class
Foo.new.singleton_class.attached_object    #=> #<Foo:0x000000010491a370>
TrueClass.attached_object                  #=> TypeError: not a singleton class
NilClass.attached_object                   #=> TypeError: not a singleton class
```
