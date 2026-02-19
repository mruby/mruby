<!-- summary: ROM Method Tables for Memory-Efficient Method Registration -->

# ROM Method Tables

ROM method tables allow C methods to be registered using static data
stored in ROM (read-only memory) rather than heap-allocated RAM. This
saves significant memory on embedded systems where RAM is scarce.

## Motivation

In a default mruby build, `mrb_open()` builds ~40 classes with ~700+
method entries at startup. Each method entry is heap-allocated via
individual `mrb_define_method_id()` calls. On a constrained MCU, this
consumes ~14KB of RAM for method table metadata alone.

ROM method tables eliminate this cost by placing method metadata in
static `const` data at compile time. Only runtime mutations (e.g.,
reopening a class to add methods) trigger heap allocation.

## Architecture

### Chained Layers

Each class has a method table (`mt`) pointer to a linked list of
`mrb_mt_tbl` layers:

```
String.mt -> [mutable layer] -> [string_ext ROM] -> [string_core ROM] -> NULL
```

**Lookup** walks the chain front-to-back, returning the first match.
The method cache makes repeated lookups O(1), so the chain walk
only occurs on cache misses.

**Mutation** uses copy-on-write (COW): if the top layer is read-only,
a new mutable layer is created in front of it. The ROM data is never
modified.

```
Before: String.mt -> [string_ext ROM] -> [string_core ROM] -> NULL

After String.define_method(:foo):
         String.mt -> [mutable: foo] -> [string_ext ROM] -> [string_core ROM] -> NULL
```

### Memory Layout

Each `mrb_mt_tbl` stores method entries as parallel arrays in a single
contiguous block:

```
ptr -> [ vals[0] vals[1] ... vals[N-1] | keys[0] keys[1] ... keys[N-1] ]
       |<--- union mrb_mt_ptr array ------>|<--- mrb_sym (encoded) array -->|
```

Values are `union mrb_mt_ptr` (function pointer or proc pointer). Keys are
`mrb_sym` with flags packed into the lower bits using `MRB_MT_KEY()`.

Keys must be sorted by symbol ID for binary search. The
`mrb_mt_init_rom()` function handles sorting at startup, so the
source code order does not matter.

## How to Define a ROM Method Table

### Step 1: Define the Static Data

Include `<mruby/internal.h>` (which provides `mrb_mt_tbl`, `union mrb_mt_ptr`,
`MRB_MT_KEY()`, and flag constants) and define the ROM data structure:

```c
#include <mruby/internal.h>
#include <mruby/presym.h>

#define MY_ROM_MT_SIZE 3
static struct {
  union mrb_mt_ptr vals[MY_ROM_MT_SIZE];
  mrb_sym keys[MY_ROM_MT_SIZE];
} my_rom_data = {
  .vals = {
    { .func = my_method_a },
    { .func = my_method_b },
    { .func = my_method_c },
  },
  .keys = {
    MRB_MT_KEY(MRB_SYM(method_a), MRB_MT_FUNC|MRB_MT_PUBLIC),
    MRB_MT_KEY(MRB_SYM(method_b), MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC),
    MRB_MT_KEY(MRB_OPSYM(eq),     MRB_MT_FUNC|MRB_MT_PUBLIC),
  }
};
static mrb_mt_tbl my_rom_mt = {
  MY_ROM_MT_SIZE, MY_ROM_MT_SIZE,
  (union mrb_mt_ptr*)&my_rom_data, NULL
};
```

### Step 2: Register in the Init Function

Replace `mrb_define_method_id()` calls with a single
`mrb_mt_init_rom()` call:

```c
void
mrb_mruby_mygem_gem_init(mrb_state *mrb)
{
  struct RClass *c = mrb_define_class_id(mrb, MRB_SYM(MyClass), mrb->object_class);
  mrb_mt_init_rom(c, &my_rom_mt);
}
```

`mrb_mt_init_rom()` sorts the keys by symbol ID, sets the readonly
flag, and pushes the ROM layer onto the class's method table chain.

### Step 3: Verify

Build and run the test suite. ROM tables are semantically transparent
to Ruby code.

## Reference

### Data Types

Defined in `include/mruby/internal.h`:

```c
union mrb_mt_ptr {
  const struct RProc *proc;
  mrb_func_t func;
};

typedef struct mrb_mt_tbl {
  int             size;
  int             alloc;    /* bit 30: MRB_MT_READONLY_BIT */
  union mrb_mt_ptr   *ptr;
  struct mrb_mt_tbl  *next;     /* next (lower-priority) layer, or NULL */
} mrb_mt_tbl;
```

### Key Encoding

```c
#define MRB_MT_KEY(sym, flags)  ((sym) << MRB_MT_KEY_SHIFT | (flags))
```

### Flags

| Flag             | Value | Description                                   |
| ---------------- | ----- | --------------------------------------------- |
| `MRB_MT_FUNC`    | 8     | Entry is a C function pointer (not an RProc)  |
| `MRB_MT_NOARG`   | 4     | Method takes no arguments (optimization hint) |
| `MRB_MT_PUBLIC`  | 0     | Public visibility                             |
| `MRB_MT_PRIVATE` | 1     | Private visibility                            |

Most ROM entries use `MRB_MT_FUNC|MRB_MT_PUBLIC` or `MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC`.

**How to choose flags:**

- **`MRB_MT_FUNC`**: Always set for C function methods. Omit only for
  RProc-based methods (rare in ROM tables).
- **`MRB_MT_NOARG`**: Set when the original `mrb_define_method_id()` used
  `MRB_ARGS_NONE()`. This enables an optimized call path in the VM.
- **`MRB_MT_PUBLIC` / `MRB_MT_PRIVATE`**: Match the intended visibility. Almost
  all methods are public.

### Symbol Macros

Use the presym macros for keys. See `doc/guides/symbol.md` for the
full list:

```c
MRB_SYM(size)       /* size */
MRB_SYM_B(chomp)    /* chomp! */
MRB_SYM_Q(frozen)   /* frozen? */
MRB_SYM_E(name)     /* name= */
MRB_OPSYM(add)      /* + */
MRB_OPSYM(eq)       /* == */
MRB_OPSYM(aref)     /* [] */
MRB_OPSYM(aset)     /* []= */
MRB_OPSYM(cmp)      /* <=> */
MRB_IVSYM(name)     /* @name */
```

### API

```c
void mrb_mt_init_rom(struct RClass *c, mrb_mt_tbl *rom);
```

Sorts the ROM table, sets the readonly flag, and pushes it onto the
class's method table chain. Multiple calls push additional layers,
which is how extension gems add methods to core classes.

## Vals and Keys Correspondence

Each `vals[i]` corresponds to `keys[i]`. The function pointer in
`vals[i]` is the C implementation of the method identified by
`keys[i]`. Their order in the source code does not matter (they are
sorted at init time), but keeping them in the same order improves
readability.

**Method aliases** (two names for the same function) are expressed as
separate entries sharing the same function pointer:

```c
.vals = {
  { .func = mrb_str_size },   /* size */
  { .func = mrb_str_size },   /* length (alias) */
},
.keys = {
  MRB_MT_KEY(MRB_SYM(size),   MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC),
  MRB_MT_KEY(MRB_SYM(length), MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC),
}
```

## Conditional Methods

Methods that depend on build configuration (e.g., `MRB_NO_FLOAT`) can
be handled in two ways:

**Option A: Separate ROM table under `#ifdef`** (preferred for large
blocks):

```c
#ifndef MRB_NO_FLOAT
#define FLOAT_ROM_MT_SIZE 29
static struct { ... } float_rom_data = { ... };
static mrb_mt_tbl float_rom_mt = { ... };
#endif

void mrb_init_numeric(mrb_state *mrb) {
  mrb_mt_init_rom(integer, &integer_rom_mt);
#ifndef MRB_NO_FLOAT
  mrb_mt_init_rom(fl, &float_rom_mt);
#endif
}
```

**Option B: Keep as `mrb_define_method_id()`** (preferred for a few
conditional methods):

```c
void mrb_init_numeric(mrb_state *mrb) {
  mrb_mt_init_rom(integer, &integer_rom_mt);
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, integer, MRB_SYM(to_f), int_to_f, MRB_ARGS_NONE());
#endif
}
```

Both approaches work correctly. The ROM layer and the
`mrb_define_method_id()` calls coexist: method lookup walks the
mutable layer first, then the ROM chain.

## Extension Gems

Extension gems use exactly the same pattern. Since gems are
initialized after core, calling `mrb_mt_init_rom()` pushes the gem's
ROM layer in front of the core ROM layer:

```c
/* mrbgems/mruby-string-ext/src/string.c */

#define STRING_EXT_ROM_MT_SIZE 53
static struct { ... } string_ext_rom_data = { ... };
static mrb_mt_tbl string_ext_rom_mt = { ... };

void mrb_mruby_string_ext_gem_init(mrb_state *mrb)
{
  struct RClass *s = mrb->string_class;
  mrb_mt_init_rom(s, &string_ext_rom_mt);
}
```

After initialization, String's method table chain looks like:

```
String.mt -> [string_ext ROM, 53 methods]
          -> [string_core ROM, 46 methods]
          -> NULL
```

A gem may also define ROM tables for multiple classes:

```c
void mrb_mruby_mygem_gem_init(mrb_state *mrb)
{
  mrb_mt_init_rom(mrb->string_class, &string_mygem_rom_mt);
  mrb_mt_init_rom(mrb->integer_class, &integer_mygem_rom_mt);
}
```

## Methods That Cannot Use ROM Tables

Some methods must remain as `mrb_define_method_id()` calls:

- **Class methods** (`mrb_define_class_method_id()`): ROM tables
  register instance methods only.
- **Module functions** (`mrb_define_module_function_id()`): Same
  reason.
- **Methods requiring `mrb_state*` at definition time**: For example,
  methods that create frozen RProc objects during init.
- **Methods on dynamically created classes**: Classes created at
  init time (not stored in `mrb->xxx_class`) that require
  `mrb_define_class()` to obtain the class pointer.

These methods are added after `mrb_mt_init_rom()` and go into the
mutable layer that sits in front of the ROM chain.

## Runtime Behavior

### Open Classes (COW)

Ruby's open classes work transparently. When a Ruby program or C code
adds a method to a class with a ROM table, the COW mechanism creates a
mutable layer:

```ruby
class String
  def my_custom_method
    42
  end
end
"hello".my_custom_method  #=> 42
"hello".size              #=> 5 (still found in ROM layer)
```

### Method Removal

`remove_method` works on ROM methods using a tombstone marker. When a
method in a ROM layer is removed, a special entry (`MRB_MT_FUNC` flag with
`func=NULL`) is inserted into the mutable layer. The `mt_get()` lookup
treats this marker as "not found" and stops searching the chain,
effectively hiding the ROM entry. Unlike `undef_method` (which blocks
superclass lookup), `remove_method`'s tombstone allows the superclass
method to be found.

`undef_method` uses a different tombstone (`proc=NULL` without
`MRB_MT_FUNC`), which is returned by `mt_get()` so the caller raises
NoMethodError without searching the superclass.

### Class Duplication

`Class.dup` shares the ROM chain. The duplicated class gets an empty
mutable layer pointing to the same ROM layers as the original. No ROM
data is copied.

### Garbage Collection

ROM layers are skipped during GC mark and sweep phases. Only mutable
layers are scanned for live RProc references and freed when the class
is collected. This reduces GC overhead.

### Memory Measurement

`mrb_class_mt_memsize()` reports only mutable layer memory. ROM layers
are not counted since they do not consume heap memory.

## Converting Existing Code

To convert existing `mrb_define_method_id()` calls to a ROM table:

1. **Count** the number of method definitions that can be converted.

2. **Create** the ROM data structure with `#define MY_ROM_MT_SIZE N`.

3. **Move** each `mrb_define_method_id()` call into the ROM table:
   - The second-to-last argument (function pointer) goes into `.vals`.
   - The third argument (symbol) goes into `.keys` via `MRB_MT_KEY()`.
   - Map the `MRB_ARGS_*` macro to flags:
     - `MRB_ARGS_NONE()` -> `MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC`
     - Anything else -> `MRB_MT_FUNC|MRB_MT_PUBLIC`

4. **Replace** the calls with `mrb_mt_init_rom(c, &my_rom_mt)`.

5. **Keep** any methods that cannot be converted (see above) as
   individual `mrb_define_method_id()` calls after the ROM init.

6. **Build and test**: `rake CONFIG=host-debug -j24 all test:run:serial`

### Before

```c
void mrb_mruby_foo_gem_init(mrb_state *mrb) {
  struct RClass *foo = mrb_define_class_id(mrb, MRB_SYM(Foo), mrb->object_class);
  mrb_define_method_id(mrb, foo, MRB_SYM(bar), foo_bar, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, foo, MRB_SYM(baz), foo_baz, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, foo, MRB_OPSYM(eq), foo_eq,  MRB_ARGS_REQ(1));
}
```

### After

```c
#define FOO_ROM_MT_SIZE 3
static struct {
  union mrb_mt_ptr vals[FOO_ROM_MT_SIZE];
  mrb_sym keys[FOO_ROM_MT_SIZE];
} foo_rom_data = {
  .vals = {
    { .func = foo_bar },
    { .func = foo_baz },
    { .func = foo_eq },
  },
  .keys = {
    MRB_MT_KEY(MRB_SYM(bar),   MRB_MT_FUNC|MRB_MT_PUBLIC),
    MRB_MT_KEY(MRB_SYM(baz),   MRB_MT_FUNC|MRB_MT_NOARG|MRB_MT_PUBLIC),
    MRB_MT_KEY(MRB_OPSYM(eq),  MRB_MT_FUNC|MRB_MT_PUBLIC),
  }
};
static mrb_mt_tbl foo_rom_mt = {
  FOO_ROM_MT_SIZE, FOO_ROM_MT_SIZE,
  (union mrb_mt_ptr*)&foo_rom_data, NULL
};

void mrb_mruby_foo_gem_init(mrb_state *mrb) {
  struct RClass *foo = mrb_define_class_id(mrb, MRB_SYM(Foo), mrb->object_class);
  mrb_mt_init_rom(foo, &foo_rom_mt);
}
```
