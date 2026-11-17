# mruby-objectspace-finalizer

`ObjectSpace.define_finalizer` and `ObjectSpace.undefine_finalizer`, for a
build that asks for them:

```ruby
conf.gem core: 'mruby-objectspace-finalizer'
```

```ruby
o = Object.new
ObjectSpace.define_finalizer(o, proc { |id| puts "#{id} is gone" })
o = nil
GC.start      # prints "... is gone"
```

As in CRuby:

- a finalizer is called with the object_id of the object, once the object
  is collected;
- `define_finalizer` answers `[0, callable]`, and adds a callable an object
  already has only once;
- an object that is never collected, such as an Integer or a Symbol, raises
  `ArgumentError`, and a frozen one `FrozenError`;
- a finalizer that raises is reported, and the others still run;
- at exit, after the `at_exit` blocks, the finalizers of the objects still
  alive run too.

A finalizer that refers to its object keeps the object alive, so it runs
only at exit.

Unlike CRuby:

- the GC cannot run Ruby code, so the finalizers of an object collected by
  the GC on its own run at the next `define_finalizer`, `GC.start`, or at
  exit, not as soon as the object is gone;
- a copy made by `dup` or `clone` has no finalizers;
- an `at_exit` block that calls `exit` or raises ends the process there
  (mruby-exit), so the finalizers left to the exit do not run.

## How

The objects that have finalizers are kept in a table the GC does not mark
through, linked into the GC with `mrb_gc_weak_add()` (`mruby/internal.h`).
Once marking is complete, before the sweep, the GC calls the table back,
and it takes out each object `mrb_gc_weak_dead_p()` reports and queues its
finalizers. The gem defines `MRB_USE_GC_WEAK`, and `GC.start` runs the
queue after it collects.
