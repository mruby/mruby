<!-- summary: About Memory Allocator Customization -->

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
