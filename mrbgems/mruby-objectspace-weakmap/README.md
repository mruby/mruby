# mruby-objectspace-weakmap

`ObjectSpace::WeakMap` and `ObjectSpace::WeakKeyMap`, for a build that asks
for them:

```ruby
conf.gem core: 'mruby-objectspace-weakmap'
```

## `ObjectSpace::WeakMap`

A map whose keys and values are held weakly: an entry is dropped when its
key or its value is collected. Keys are compared by identity.

```ruby
m = ObjectSpace::WeakMap.new
k = "key"
m[k] = "value"
m[k]          # => "value"
m["key"]      # => nil, another object
```

It has `[]`, `[]=`, `delete`, `key?` (`include?`, `member?`), `keys`,
`values`, `size` (`length`), `each` (`each_pair`), `each_key`,
`each_value` and `inspect`, and includes Enumerable. An immediate value,
such as an Integer or a Symbol, is never collected, and an entry of one
stays.

## `ObjectSpace::WeakKeyMap`

A map whose keys are held weakly, compared by `eql?` and `hash`, and whose
values are held as long as their keys are. A key that is never collected
raises `ArgumentError`.

```ruby
m = ObjectSpace::WeakKeyMap.new
k = "key"
m[k] = 1
m["key"]          # => 1
m.getkey("key")   # => k
```

It has `[]`, `[]=`, `delete`, `getkey`, `key?`, `clear` and `inspect`.

As in CRuby:

- entries are in the order they were added;
- a copy made by `dup` or `clone` is empty;
- a frozen map still takes entries;
- WeakMap's `each` also yields the entries its block adds;
- a value that refers to its key keeps the key, and its entry, alive.

## How

The maps keep their entries in tables the GC does not mark through, and
link each table into the GC with `mrb_gc_weak_add()` (`mruby/gc.h`). Once
marking is complete, before the sweep, the GC calls the table back, and it
drops each entry `mrb_gc_weak_dead_p()` reports. WeakKeyMap's values are
kept in an Array the map holds.
