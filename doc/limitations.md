<!-- summary: About the Limitations of mruby -->

# Limitations and Differences

The philosophy of mruby is to be a lightweight implementation of
the Ruby ISO standard. These two objectives are partially contradicting.
Ruby is an expressive language with complex implementation details which
are difficult to implement in a lightweight manner. To cope with this,
limitations to the "Ruby Compatibility" are defined.

This document is collecting these limitations.

## Integrity

This document does not contain a complete list of limitations.
Please help to improve it by submitting your findings.

## `Kernel.raise` in rescue clause

`Kernel.raise` without arguments does not raise the current exception within
a rescue clause.

```ruby
begin
  1 / 0
rescue
  raise
end
```

#### CRuby

`ZeroDivisionError` is raised.

#### mruby

`RuntimeError` is raised instead of `ZeroDivisionError`. To re-raise the exception, you have to do:

```ruby
begin
  1 / 0
rescue => e
  raise e
end
```

## Fiber execution can't cross C function boundary

mruby's `Fiber` is implemented similarly to Lua's co-routine. This
results in the consequence that you can't switch context within C functions.
Only exception is `mrb_fiber_yield` at return.

## `Array` does not support instance variables

To reduce memory consumption `Array` does not support instance variables.

```ruby
class Liste < Array
  def initialize(str = nil)
    @field = str
  end
end

p Liste.new "foobar"
```

#### CRuby

`[]`

#### mruby

`ArgumentError` is raised.

## `defined?`

The `defined?` keyword is considered too complex to be fully
implemented. It is recommended to use `const_defined?` and
other reflection methods instead.

```ruby
defined?(Foo)
```

#### CRuby

```
nil
```

#### mruby

`NameError` is raised.

## `alias` on global variables

Aliasing a global variable works in CRuby but is not part
of the ISO standard.

```ruby
alias $a $__a__
```

#### CRuby

`nil`

#### mruby

Syntax error

## Operator modification

Operators on some of the primitive classes cannot be overridden, as they are
optimized in the VM.

```ruby
class String
  def +
  end
end

'a' + 'b'
```

#### CRuby

`ArgumentError` is raised.
The re-defined `+` operator does not accept any arguments.

#### mruby

`'ab'`
Behavior of the operator wasn't changed.

## `Kernel#binding` is not supported without mruby-binding gem

`Kernel#binding` method requires the `mruby-binding` gem (included
in the `metaprog` gembox). Without this gem, `binding` is not
available.

## `nil?` redefinition in conditional expressions

Redefinition of `nil?` is ignored in conditional expressions.

```ruby
a = "a"
def a.nil?
  true
end
puts(a.nil? ? "truthy" : "falsy")
```

Ruby outputs `truthy`. mruby outputs `falsy`.

## Argument Destructuring

```ruby
def m(a,(b,c),d); p [a,b,c,d]; end
m(1,[2,3],4)  # => [1,2,3,4]
```

Destructured arguments (`b` and `c` in above example) cannot be accessed
from the default expression of optional arguments and keyword arguments,
since actual assignment is done after the evaluation of those default
expressions. Thus:

```ruby
def f(a,(b,c),d=b)
  p [a,b,c,d]
end
f(1,[2,3])
```

CRuby gives `[1,2,3,nil]`. mruby raises `NoMethodError` for `b`.

Keyword argument expansion has similar restrictions. The following example, gives `[1, 1]` for CRuby, mruby raises `NoMethodError` for `b`.

```ruby
def g(a: 1, b: a)
  p [a,b]
end
g(a:1)
```

## No Double Dispatch in Module Loading

To make implementation simpler, mruby does not use double dispatching in module loading (`include`/`prepend`/`extend`).
Those method internally called corresponding actual load methods (`append_features`/`prepend_features`/`extend_object`).
But they are rarely overloaded, consumes more memory, and make loading little bit slower. As a Ruby implementation for the smaller device,
we decided mruby simpler.

```ruby
module M
  def self.append_features(mod)
     p :append
  end
end

class C
  include M
end
```

#### CRuby

Prints `:append`.

#### mruby

Nothing printed (since `include` does not call `append_features` internally).

## No `#hash` call for small hashes

For performance reasons, mruby avoids calling the `#hash` method on keys when a hash table is small. This means that custom `#hash` methods on key objects may not be executed.

## Pattern Matching

Pattern matching is only partially supported in mruby. Currently, only the rightward assignment operator (`=>`) with simple variable binding is implemented.

```ruby
expr => var  # Supported: assigns expr to var
```

#### CRuby

Full pattern matching with `case/in` syntax and various pattern types:

```ruby
case [1, 2, 3]
in [a, b, c]
  puts "#{a}, #{b}, #{c}"  # => "1, 2, 3"
end

case {name: "Alice", age: 30}
in {name:, age:}
  puts "#{name} is #{age}"  # => "Alice is 30"
end
```

#### mruby

Only rightward assignment with simple variable binding:

```ruby
[1, 2, 3] => x
puts x  # => [1, 2, 3]
```

The following are **not supported**:

- `case/in` syntax
- Array patterns: `in [a, b, c]`
- Hash patterns: `in {name:, age:}`
- Guard clauses: `in pattern if condition`
- Pin operator: `in ^variable`
- Find patterns: `in [*, x, *]`
- Alternative patterns: `in pattern1 | pattern2`
- Boolean pattern check: `value in pattern`

Note: mruby does provide `Array#deconstruct` and `Hash#deconstruct_keys` methods for future pattern matching compatibility.

## No Refinements

Module refinements (`refine`, `using`) are not supported in mruby.

## No `Encoding` Class

mruby does not have an `Encoding` class. Strings are treated as
byte sequences by default. UTF-8 aware string operations can be
enabled with the `MRB_UTF8_STRING` compile flag.

## Integer Precision Varies by Boxing Mode

Integer size depends on the value boxing configuration:

| Configuration                 | Integer range    |
| ----------------------------- | ---------------- |
| Word boxing, 64-bit (default) | roughly +/- 2^62 |
| Word boxing, 32-bit (default) | roughly +/- 2^30 |
| NaN boxing (64-bit only)      | -2^31 to 2^31-1  |

Code relying on 64-bit integer precision may behave differently
across configurations. The `mruby-bigint` gem provides
arbitrary-precision integers when included.

## No `ObjectSpace.each_object` by Default

`ObjectSpace` is only available via the `mruby-objectspace` gem
(included in the `stdlib` gembox). Even with the gem,
`ObjectSpace.each_object` has limited functionality compared
to CRuby.
