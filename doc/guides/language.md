<!-- summary: mruby Language Features and Ruby Compatibility -->

# mruby Language Features

This guide describes the Ruby language features supported by mruby 4.0.
mruby implements a subset of the Ruby language, optimized for embedded use.
For a list of specific behavioral differences, see
[limitations.md](../limitations.md).

## Syntax

### Keywords

mruby supports the following keywords:

`BEGIN`, `END`, `alias`, `and`, `begin`, `break`, `case`, `class`,
`def`, `do`, `else`, `elsif`, `end`, `ensure`, `false`, `for`, `if`,
`in`, `module`, `next`, `nil`, `not`, `or`, `redo`, `rescue`,
`retry`, `return`, `self`, `super`, `then`, `true`, `undef`,
`unless`, `until`, `when`, `while`, `yield`

Magic variables: `__FILE__`, `__LINE__`, `__ENCODING__`, `__method__`

**Not supported:** `defined?` (use `respond_to?`, `const_defined?`,
etc. instead), `refinements` (`using`, `refine`).

### Classes and Modules

```ruby
class Animal
  attr_accessor :name

  def initialize(name)
    @name = name
  end

  def speak
    "..."
  end
end

class Dog < Animal
  def speak
    "Woof!"
  end
end

module Greetable
  def greet
    "Hello, I'm #{name}"
  end
end

class Dog
  include Greetable
end
```

All standard class and module features are supported: inheritance,
`include`, `prepend`, `extend`, `attr_reader`/`attr_writer`/
`attr_accessor`, `public`/`private`/`protected` visibility, class
variables (`@@var`), class methods, and `super`.

### Methods

```ruby
# Required, optional, rest, post-rest, keyword, and block arguments
def example(a, b = 1, *rest, last, key:, opt_key: nil, **kwargs, &block)
end

# Endless method definition
def double(x) = x * 2
```

### Blocks and Procs

```ruby
[1, 2, 3].each { |n| puts n }

[1, 2, 3].each do |n|
  puts n
end

square = Proc.new { |x| x * x }
square = proc { |x| x * x }
double = lambda { |x| x * 2 }
double = ->(x) { x * 2 }
```

### Control Flow

```ruby
# if/unless (both statement and modifier forms)
if condition
  # ...
elsif other
  # ...
else
  # ...
end

result = value if condition
result = value unless condition

# case/when
case obj
when String then "string"
when Integer then "integer"
else "other"
end

# Loops
while condition
  # ...
end

until condition
  # ...
end

for item in collection
  # ...
end

# Loop control
break       # exit loop
next        # skip to next iteration
redo        # restart current iteration
retry       # restart begin/rescue block
```

### Exception Handling

```ruby
begin
  risky_operation
rescue ArgumentError => e
  handle_arg_error(e)
rescue StandardError => e
  handle_error(e)
ensure
  cleanup
end

raise "something went wrong"
raise ArgumentError, "bad argument"
```

**Note:** `raise` without arguments in a `rescue` clause does not
re-raise the current exception. Capture and re-raise explicitly:

```ruby
begin
  risky_operation
rescue => e
  log(e)
  raise e  # explicit re-raise required
end
```

### Strings

```ruby
"double-quoted with #{interpolation}"
'single-quoted literal'
heredoc = <<~HEREDOC
  indented heredoc
  with #{interpolation}
HEREDOC
```

### Regular Expressions

Regular expressions require an external gem such as `mruby-regexp-pcre`
or `mruby-onig-regexp`. Without a regexp gem, `Regexp` literals
(`/pattern/`) are not available.

### Pattern Matching (Limited)

Only rightward assignment with simple variable binding is supported:

```ruby
expr => var   # assigns expr to var
```

`case/in` syntax, array/hash patterns, guard clauses, pin operator,
find patterns, and alternative patterns are **not** supported.

## Numeric Types

mruby's numeric type sizes depend on the boxing mode and platform.

### Integer

| Configuration | Range |
| ------------- | ----- |
| 64-bit word boxing (default on 64-bit) | roughly +/- 2^62 |
| 32-bit word boxing (default on 32-bit) | roughly +/- 2^30 |
| NaN boxing (64-bit only) | -2^31 to 2^31-1 |

Integer overflow raises a `RangeError` unless the `mruby-bigint` gem
is included, in which case integers automatically promote to
arbitrary precision.

### Float

By default, `Float` uses 64-bit `double`. Compile-time options:

- `MRB_USE_FLOAT32`: use 32-bit `float` instead
- `MRB_NO_FLOAT`: disable floating-point entirely

With word boxing on 64-bit, many float values are stored inline
(without heap allocation) using a rotation encoding.

### Additional Numeric Types (via gems)

- **Rational** (`mruby-rational`): exact rational arithmetic
- **Complex** (`mruby-complex`): complex number support
- **Bigint** (`mruby-bigint`): arbitrary-precision integers

## Core Classes

These classes are always available in mruby (no gem required):

| Class | Notes |
| ----- | ----- |
| Object | Base class for all objects |
| Module | Module definition and mixin |
| Class | Class definition and instantiation |
| NilClass | Singleton `nil` |
| TrueClass | Singleton `true` |
| FalseClass | Singleton `false` |
| Integer | Fixed-precision integer |
| Float | Floating-point (unless `MRB_NO_FLOAT`) |
| Symbol | Interned identifier |
| String | Mutable byte string |
| Array | Ordered collection |
| Hash | Key-value mapping |
| Range | Interval representation |
| Proc | Closure / callable object |
| Exception | Exception hierarchy root |
| StandardError | Common error base |

### Core Modules

| Module | Notes |
| ------ | ----- |
| Kernel | Core methods (`puts`, `p`, `raise`, etc.) |
| Comparable | Comparison operators via `<=>` |
| Enumerable | Collection iteration methods |

## Standard Library (via gemboxes)

mruby's standard library is organized into gemboxes. The `default`
gembox includes all of the below. Use this table to find which
gembox provides the class or feature you need:

### Classes and Modules

| Class/Module | Gembox | Gem |
| ------------ | ------ | --- |
| Fiber | stdlib | mruby-fiber |
| Enumerator | stdlib | mruby-enumerator |
| Enumerator::Lazy | stdlib | mruby-enum-lazy |
| Set | stdlib | mruby-set |
| ObjectSpace | stdlib | mruby-objectspace |
| Time | stdlib-ext | mruby-time |
| Struct | stdlib-ext | mruby-struct |
| Data | stdlib-ext | mruby-data |
| Random | stdlib-ext | mruby-random |
| IO, File | stdlib-io | mruby-io |
| Socket | stdlib-io | mruby-socket |
| Dir | stdlib-io | mruby-dir |
| Errno | stdlib-io | mruby-errno |
| Math | math | mruby-math |
| Rational | math | mruby-rational |
| Complex | math | mruby-complex |
| Bigint | math | mruby-bigint |
| Method, UnboundMethod | metaprog | mruby-method |

### Methods and Features

| Feature | Gembox | Gem |
| ------- | ------ | --- |
| `catch`/`throw` | stdlib | mruby-catch |
| `Kernel#sprintf`, `String#%` | stdlib-ext | mruby-sprintf |
| `Array#pack`, `String#unpack` | stdlib-ext | mruby-pack |
| `Kernel#rand` | stdlib-ext | mruby-random |
| `Kernel#eval` | metaprog | mruby-eval |
| `Kernel#binding` | metaprog | mruby-binding |
| `Proc#binding` | metaprog | mruby-proc-binding |
| Runtime compiler | metaprog | mruby-compiler |

### Core Class Extensions

The `stdlib` gembox also extends built-in classes with additional
methods. These are included by default:

| Extension | Examples |
| --------- | -------- |
| Array extensions | `#dig`, `#union`, `#difference` |
| Hash extensions | `#dig`, `#transform_keys`, `#transform_values` |
| String extensions | `#encode`, `#bytes`, `#chars` |
| Numeric extensions | `Integer#digits`, `Integer#pow` |
| Comparable extensions | `#clamp` |
| Enumerable extensions | `#sort_by`, `#min_by`, `#max_by`, `#tally` |
| Range extensions | `#size`, `#cover?` |
| Proc extensions | `#<<`, `#>>` (composition) |
| Symbol extensions | `#to_proc` |
| Object extensions | `#then`, `#yield_self` |
| Kernel extensions | `#__method__` |
| Class/Module extensions | `Module#name` |

### Gembox Summary

| Gembox | Contents | Notes |
| ------ | -------- | ----- |
| `stdlib` | Core class extensions, Fiber, Enumerator, Set | Works with `MRB_NO_STDIO` and `MRB_NO_FLOAT` |
| `stdlib-ext` | Time, Struct, Data, Random, sprintf, pack | Works with `MRB_NO_STDIO` and `MRB_NO_FLOAT` |
| `stdlib-io` | IO, File, Dir, Socket, Errno | Requires stdio |
| `math` | Math, Rational, Complex, Bigint | Works with `MRB_NO_STDIO` |
| `metaprog` | eval, binding, Method, compiler | Works with `MRB_NO_STDIO` and `MRB_NO_FLOAT` |
| `default` | All of the above + CLI tools | Full installation |

## Key Differences from CRuby

### No Runtime Loading

mruby has no `require` or `load`. All code (gems, libraries) is
linked at build time. To add functionality, include the appropriate
gem in your build configuration:

```ruby
MRuby::Build.new do |conf|
  conf.gem :core => "mruby-time"
end
```

### No `defined?` Keyword

The `defined?` keyword raises `NameError` instead of returning a
type string or `nil`. Use alternatives:

```ruby
# Instead of: defined?(Foo)
Object.const_defined?(:Foo)

# Instead of: defined?(@var)
instance_variable_defined?(:@var)

# Instead of: defined?(method_name)
respond_to?(:method_name)
```

### Fiber Limitations

Fibers cannot cross C function boundaries. You cannot yield from a
fiber inside a C-implemented method. Only `mrb_fiber_yield` at
function return is supported.

### Array Subclasses

`Array` does not support instance variables to reduce memory. This
means subclassing `Array` and adding `@fields` will raise an error.

### Operator Overriding

Operators cannot be overridden by user code. Redefining `+` on
`String` has no effect on the behavior of the `+` operator.

### Module Loading Hooks

`include`/`prepend`/`extend` do not call `append_features`/
`prepend_features`/`extend_object` hooks. The module is included
directly.

### Small Hash Optimization

For small hashes, `#hash` is not called on keys. Custom `#hash`
methods may not execute for small hash tables.

### No Refinements

Module refinements (`refine`, `using`) are not supported.

### No Encoding Class

There is no `Encoding` class. String encoding is either pure bytes
or UTF-8 (opt-in via `MRB_UTF8_STRING` compile flag).

### `nil?` in Conditionals

Redefining `nil?` has no effect on conditional expressions.
The VM uses direct nil checks for performance.

### Integer Precision

Integer size varies by boxing mode (see [Numeric Types](#numeric-types)
above). Code relying on 64-bit integer precision may behave
differently on 32-bit or NaN boxing configurations.

## Build-Time Configuration

Key compile-time macros that affect language behavior:

| Macro | Effect |
| ----- | ------ |
| `MRB_NO_FLOAT` | Remove all float support |
| `MRB_USE_FLOAT32` | Use 32-bit float instead of double |
| `MRB_UTF8_STRING` | Enable UTF-8 string handling |
| `MRB_INT32` | Force 32-bit integer |
| `MRB_INT64` | Force 64-bit integer |
| `MRB_STR_LENGTH_MAX` | Max string length (default 1MB) |
| `MRB_ARY_LENGTH_MAX` | Max array length (default 2^17) |

See [mrbconf.md](mrbconf.md) for the complete list of configuration
macros.
