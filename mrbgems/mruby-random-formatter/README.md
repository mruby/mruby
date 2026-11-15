# mruby-random-formatter

`Random::Formatter` with `random_number` and `rand`, as in CRuby. It is an
opt-in gem:

```ruby
conf.gem core: 'mruby-random-formatter'
```

`Random` includes and extends the module, and so can any class that has a
`bytes(n)` method returning a String of `n` random bytes:

```ruby
class Dice
  include Random::Formatter

  def bytes(n)
    Random.bytes(n)
  end
end

p Dice.new.random_number(1..6)  # => 4
```

Its `random_number` and `rand` are `Random#rand`, except that a limit that
gives no number, `nil`, a negative number or an empty range, gives a Float in
[0, 1) where `Random#rand` raises or returns `nil`:

```ruby
p Random.random_number(10)  # => 7
p Random.random_number(-1)  # => 0.5418346412525913
p Random.new.rand(-1)       # ArgumentError, Random's own rand
```

Every draw reads the receiver's `bytes`: `Random#bytes` for a `Random`,
`Random.bytes` for `Random` itself. An integer limit that fits in 32 bits
reads 4 bytes a try, and a float 8. An infinite or NaN limit raises
`FloatDomainError`, where CRuby raises `Errno::EDOM`.
