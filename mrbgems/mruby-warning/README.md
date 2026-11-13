# mruby-warning

The `Warning` module, for a build that asks for it:

```ruby
conf.gem core: 'mruby-warning'
```

It turns the categories of warnings on and off, and writes warnings:

| method                           | does                                               |
| -------------------------------- | -------------------------------------------------- |
| `Warning.categories`             | the names of the categories                        |
| `Warning[category]`              | whether warnings in the category are written       |
| `Warning[category] = flag`       | turns warnings in the category on or off           |
| `Warning.warn(msg, category: c)` | writes the message to `$stderr`, unless `c` is off |

The categories are CRuby's, each on or off as CRuby starts: `:deprecated`
(off), `:experimental` (on), `:performance` (off) and `:strict_unused_block`
(off). `Warning[]` raises `TypeError` for a category that is not a Symbol, and
`ArgumentError` for one it does not know.

With this gem, `Kernel#warn` (mruby-io) drops a warning whose `category:` is
off, and hands the rest to `Warning.warn` rather than writing to `$stderr`
itself, so redefining `Warning.warn` changes where warnings go:

```ruby
def Warning.warn(message, category: nil)
  LOG << message
end
```

`Kernel#warn` passes `category:` only for a warning that has one, so a
`Warning.warn` that takes the message alone sees every warning with no
category.

Warnings that mruby writes from C, with `mrb_warn()`, go to stderr as they
did.
