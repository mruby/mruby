# mruby-random-urandom

`Random.urandom`, for a build that asks for it:

```ruby
conf.gem core: 'mruby-random-urandom'
```

```ruby
key = Random.urandom(32)  # => 32 random bytes, a binary String
```

It reads the platform's source of random bytes, not a seeded generator as
`Random.bytes` does, so its bytes suit keys and nonces. It raises
`RuntimeError` when the platform cannot give them, and `ArgumentError` for a
negative size.

## Ports

| port    | reads             |
| ------- | ----------------- |
| `posix` | `/dev/urandom`    |
| `win`   | `BCryptGenRandom` |

A build without either, such as a board's, gives the bytes from its own
`mrb_hal_urandom()` (see `include/urandom_hal.h`), from a hardware generator
for instance, in a `hal-urandom-<conf>` gem.
