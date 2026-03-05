<!-- summary: About Memory Allocator Customization and Heap Regions -->

# Memory Allocation

In mruby, you can customize how memory is allocated in two ways:

1. **Provide your own `malloc()`/`realloc()`/`free()`**
2. **Override `mrb_basic_alloc_func()`**

---

## 1. Provide your own `malloc()`/`realloc()`/`free()`

On platforms without a full C standard library —such as many microcontrollers— you may need to supply your own implementations of `malloc()`, `realloc()`, and `free()`. mruby’s allocator calls directly into these functions, so replacing them lets you control **every** allocation and deallocation performed by your entire program, including any third‑party libraries you link against.

Keep in mind:

- Calling `realloc(NULL, size)` must behave like `malloc(size)`.
- Calling `free(NULL)` must be a no‑op.

Simply define these three functions in your code (or link against a library that provides them), and mruby — along with all other code in your process — will use your versions automatically.

## 2. Override `mrb_basic_alloc_func()`

Inside mruby, all of its own memory allocations go through a single function called mrb_basic_alloc_func() (formerly mrb_default_allocf()). By defining this function in your application before linking, you can intercept and handle **only** the memory operations initiated by mruby itself without affecting other libraries or parts of your program.

```c
// Example signature:
// void* mrb_basic_alloc_func(void* ptr, size_t size);
```

Implement mrb_basic_alloc_func() in your code, and mruby will invoke it for every internal allocation, reallocation, and free request.

### Expected behavior

- `mrb_basic_alloc_func(NULL, size)` should allocate `size` bytes, just like `malloc(size)`.
- `mrb_basic_alloc_func(ptr, size)` should resize the existing block at `ptr` to `size` bytes, just like `realloc(ptr, size)`.
- `mrb_basic_alloc_func(ptr, 0)` should free the block at `ptr`, just like `free(ptr)`.

---

## Summary of effects:

- **Custom `malloc`/`realloc`/`free`**: replaces allocation behavior globally (mruby + all other code and third‑party libraries).

- **Custom `mrb_basic_alloc_func()`**: replaces allocation behavior only for mruby’s internal use, leaving other libraries’ allocations untouched.

## Migration note

If you are moving from the old API:

1. **Removal of `mrb_open_allocf()`**
   - \_Old:

     ```c
     mrb_state *mrb = mrb_open_allocf(my_allocf, ud);
     ```

   - \_New:

     ```c
     // No allocf parameter; set up your hook via mrb_basic_alloc_func definition.
     mrb_state *mrb = mrb_open_core();
     ```

2. **`mrb_open_core()` takes no arguments**
   - Simply drop any allocf or user-data arguments, and redefine `mrb_basic_alloc_func` as you need.

3. **No more `mrb_allocf` type**
   - Definitions using the `mrb_allocf` typedef can be removed; implement `mrb_basic_alloc_func()` with the signature below:

     ```c
     void* mrb_basic_alloc_func(void *ptr, size_t size);
     ```

4. **`mrb_basic_alloc_func` signature change**
   - _Old:_

     ```c
     void* mrb_default_allocf(mrb_state *mrb, void *ptr, size_t size, void *ud);
     ```

   - _New:_

     ```c
     void* mrb_basic_alloc_func(void *ptr, size_t size);
     ```

---

### Code examples

- **Old style**:

  ```c
  static void*
  my_allocf(mrb_state *mrb, void *ud, void *ptr, size_t size)
  {
    // ...custom logic...
  }

  mrb_state *mrb = mrb_open_allocf(my_allocf, some_ud);
  ```

- **New style**:

  ```c
  // Define your hook before creating the state:
  void*
  mrb_basic_alloc_func(void *ptr, size_t size)
  {
    // ...custom logic...
  }

  mrb_state *mrb = mrb_open_core();
  ```

---

## 3. Heap Regions: Contiguous Memory for GC

By default, mruby allocates GC heap pages individually via `malloc()`.
On embedded targets with multiple memory banks (e.g., STM32 CCM+SRAM,
ESP32 PSRAM+IRAM), you may want to place heap pages in a specific
memory region. `mrb_gc_add_region()` lets you provide a contiguous
buffer that mruby carves into heap pages.

### API

```c
#include <mruby/gc.h>

int mrb_gc_add_region(mrb_state *mrb, void *start, size_t size);
```

- **`start`**: pointer to a contiguous memory buffer.
- **`size`**: size of the buffer in bytes.
- **Returns**: number of heap pages carved from the buffer, or 0 if
  the buffer is too small.

The buffer is aligned internally to pointer size. Each page is
approximately 40 KB on 64-bit systems (24 KB on 32-bit). The caller
retains ownership of the buffer and must keep it valid for the
lifetime of the `mrb_state`.

### Example: Static buffer

```c
#include <mruby.h>
#include <mruby/gc.h>

/* 256 KB static buffer -- about 6 pages on 64-bit */
static char heap_buf[256 * 1024];

int main(void)
{
  mrb_state *mrb = mrb_open();
  int pages = mrb_gc_add_region(mrb, heap_buf, sizeof(heap_buf));
  /* pages are immediately available for object allocation */

  /* ... use mrb ... */

  mrb_close(mrb);  /* region pages are cleaned up; buffer is not freed */
  return 0;
}
```

### Example: MCU with multiple RAM banks

```c
/* STM32 with 64 KB CCM and 128 KB SRAM */
extern char __ccm_start[], __ccm_end[];   /* linker symbols */
extern char __sram_start[], __sram_end[];

mrb_state *mrb = mrb_open();
mrb_gc_add_region(mrb, __ccm_start, __ccm_end - __ccm_start);
mrb_gc_add_region(mrb, __sram_start, __sram_end - __sram_start);
```

### How it works

When `mrb_gc_add_region()` is called, mruby:

1. Aligns the buffer start to pointer size.
2. Divides the buffer into `mrb_heap_page`-sized chunks.
3. Initializes each page's freelist and links it into the GC heap.
4. Records the region in a descriptor for O(1) pointer-to-page mapping.

Region pages participate in the normal GC cycle (mark-and-sweep) like
any other heap page. The only differences are:

- **Never freed**: the GC will not call `free()` on region pages, even
  if all objects on a page are dead. The page stays in the heap with an
  empty freelist, ready for reuse.
- **Fallback**: when all region pages are full, mruby falls back to
  `malloc()` for new pages as usual.
- **Cleanup**: `mrb_close()` frees the internal region descriptor but
  does not free the buffer itself.

### Sizing

The page size is controlled by `MRB_HEAP_PAGE_SIZE` (default: 1024 slots).
Each page occupies:

| Platform | Slot size | Page size (approx)  |
|----------|-----------|---------------------|
| 64-bit   | 40 bytes  | ~41 KB              |
| 32-bit   | 24 bytes  | ~25 KB              |

To estimate pages for a given buffer: `pages = buffer_size / sizeof(mrb_heap_page)`.
Each page provides `MRB_HEAP_PAGE_SIZE` object slots.
