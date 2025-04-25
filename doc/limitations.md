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

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

`ZeroDivisionError` is raised.

#### mruby [3.1.0 (2022-05-12)]

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

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

`[]`

#### mruby [3.1.0 (2022-05-12)]

`ArgumentError` is raised.

## `defined?`

The `defined?` keyword is considered too complex to be fully
implemented. It is recommended to use `const_defined?` and
other reflection methods instead.

```ruby
defined?(Foo)
```

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

```
nil
```

#### mruby [3.1.0 (2022-05-12)]

`NameError` is raised.

## `alias` on global variables

Aliasing a global variable works in CRuby but is not part
of the ISO standard.

```ruby
alias $a $__a__
```

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

`nil`

#### mruby [3.1.0 (2022-05-12)]

Syntax error

## Operator modification

An operator can't be overwritten by the user.

```ruby
class String
  def +
  end
end

'a' + 'b'
```

#### Ruby [ruby 2.0.0p645 (2015-04-13 revision 50299)]

`ArgumentError` is raised.
The re-defined `+` operator does not accept any arguments.

#### mruby [3.1.0 (2022-05-12)]

`'ab'`
Behavior of the operator wasn't changed.

## `Kernel#binding` is not supported until [3.0.0 (2021-03-05)]

`Kernel#binding` method is not supported.

#### Ruby [ruby 2.5.1p57 (2018-03-29 revision 63029)]

```shell
$ ruby -e 'puts Proc.new {}.binding'
#<Binding:0x00000e9deabb9950>
```

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

#### Ruby [ruby 3.5.0dev (2025-04-21 85bab61565))]

Prints `:append`.

#### mruby [3.5.0 (2025-04-28)]

Nothing printed (since `include` does not call `append_features` internally).
