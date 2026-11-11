# mruby-coerce

This mrbgem lets an Integer or Float operator take an operand of a class a
program defines, through the operand's `coerce`, as CRuby does:

```ruby
class Length
  attr_reader :n
  def initialize(n) = @n = n
  def coerce(other) = [Length.new(other), self]
  def *(o) = Length.new(@n * (o.is_a?(Length) ? o.n : o))
end

(2 * Length.new(3)).n  # => 6
```

When an operator of Integer or Float (`+ - * / % ** div quo fdiv divmod
<=> < <= > >=`) is given an operand that is none of mruby's own numbers and
answers `coerce`, it calls `operand.coerce(self)` for a pair `[a, b]` and
answers `a op b`. `coerce` must answer a two-element Array, and a comparison
whose answer is nil raises ArgumentError. An operand without `coerce` fails
as it does without this gem. The gem also adds `Integer#coerce` and
`Float#coerce`.

## Cost

The gem defines `MRB_USE_COERCE` for the build. Without it, the calls in
`src/numeric.c` are not compiled at all, so a build that leaves the gem out
pays nothing in code size or speed. With it, the call is made only where an
operator would otherwise give up on its operand; arithmetic between mruby's
own numbers takes the same path as before.

The gem is in `full-core` only. Add it with:

```ruby
conf.gem :core => 'mruby-coerce'
```
