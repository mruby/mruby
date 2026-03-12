<!-- summary: Virtual Machine Internals -->

# Virtual Machine Internals

This document describes mruby's virtual machine for developers
working on `src/vm.c` and related code.

**Read this if you are:** debugging method dispatch or call frame
issues, working on exception handling, implementing new opcodes,
modifying fiber/coroutine behavior, or optimizing the dispatch loop.

For the instruction set, see [opcode.md](opcode.md). For the
compiler that generates bytecode, see [compiler.md](compiler.md).

## Execution Model

mruby uses a **register-based VM**. Local variables and temporaries
occupy fixed register slots determined at compile time. Each method
call gets its own register window on a shared value stack.

## Execution Context

The VM state is stored in `mrb_context`:

```text
mrb_context
+-- stbase..stend    value stack (mrb_value[])
+-- cibase..ciend    call info stack (mrb_callinfo[])
+-- ci               current call frame pointer
+-- status           fiber state
+-- prev             previous context (fiber chain)
+-- vmexec           VM execution state flag
```

The value stack and call info stack grow independently. Each fiber
has its own `mrb_context`.

### Stack Sizing

- Initial value stack: 128 entries (`STACK_INIT_SIZE`)
- Initial call info stack: 32 entries (`CALLINFO_INIT_SIZE`)
- Growth factor: 1.5x (or 2x with `MRB_STACK_EXTEND_DOUBLING`)
- Minimum growth: 128 entries (`MRB_STACK_GROWTH`)
- Max stack depth: `MRB_STACK_MAX` (0x40000 - 128)
- Max call depth: `MRB_CALL_LEVEL_MAX` (512, or 128 with ASAN)

Exceeding either limit raises `SystemStackError`.

When the value stack is reallocated, all `REnv` objects and
`mrb_callinfo` stack pointers are adjusted by the delta
(`envadjust` function).

## Call Frames

Each method or block call pushes a `mrb_callinfo` frame:

```text
mrb_callinfo
+-- n:4          positional argument count (0-14, 15 = varargs)
+-- nk:4         keyword argument count (0-14, 15 = varargs)
+-- cci          call context info (NONE, DIRECT, SKIP, RESUMED)
+-- vis          visibility flags (public/private/protected)
+-- mid          method symbol
+-- proc         current RProc
+-- blk          block argument (RProc*)
+-- stack        pointer into value stack
+-- pc           program counter (bytecode position)
+-- u.env        closure environment (REnv*)
+-- u.target_class  receiver's class
```

### Stack Layout Per Frame

```text
ci->stack:
  [0]      self (receiver)
  [1..n]   positional arguments
  [n+1..]  keyword argument pairs (key, value, key, value, ...)
  [bidx]   block argument
  [bidx+1..] local variables and temporaries
```

### Argument Count Encoding

The `n` and `nk` fields are 4 bits each (0-15). When `n == 15`,
positional arguments are packed into a single Array in register 1.
When `nk == 15`, keyword arguments are packed into a single Hash.

The block index is calculated by `mrb_bidx(n, nk)`:

```text
if n == 15: n = 1 (array)
if nk == 15: n += 1 (hash)
else: n += nk * 2 (key-value pairs)
return n + 1 (skip self)
```

### Call Context Info (cci)

| Value | Name            | Meaning                               |
| ----- | --------------- | ------------------------------------- |
| 0     | `CINFO_NONE`    | Normal VM-to-VM call                  |
| 1     | `CINFO_DIRECT`  | Explicit VM call (block, lambda.call) |
| 2     | `CINFO_SKIP`    | Skip frame in stack traces            |
| 3     | `CINFO_RESUMED` | Fiber resumed (stop execution)        |

## Dispatch Loop

The main loop in `mrb_vm_run()` decodes and dispatches opcodes.
Two dispatch strategies are available:

- **Computed goto** (default on GCC/Clang): a jump table of label
  addresses (`optable[]`) for direct dispatch. Faster due to
  better branch prediction.
- **Switch-based** (`MRB_USE_VM_SWITCH_DISPATCH`): a standard
  `switch(insn)` statement. Default on MSVC and other compilers.

The dispatch loop is wrapped in `MRB_TRY`/`MRB_CATCH` for exception
handling (see [Exception Handling](#exception-handling)).

## Method Dispatch

When `OP_SEND` (or `OP_SSEND`, `OP_SUPER`) executes:

### 1. Prepare Arguments

Determine argument layout. If argument count < 15, the fast path
uses inline registers. Otherwise, arguments are packed into an
Array (varargs mode).

### 2. Push Call Frame

```c
ci = cipush(mrb, a, CINFO_DIRECT, NULL, NULL, blk, mid, argc);
```

The new frame's stack starts at the previous frame's stack + `a`
(the receiver's register index).

### 3. Method Lookup

The lookup sequence:

1. **Method cache check**: hash table lookup by `(class, mid)`.
   Default cache size: `MRB_METHOD_CACHE_SIZE` (256 entries).
2. **Method table walk**: if cache misses, search the receiver's
   class method table (`mt`), then walk the superclass chain.
3. **Cache store**: on successful lookup, store in the cache.

The method cache is invalidated when classes are modified
(`mrb_mc_clear_by_class`).

### 4. Invoke

- **Ruby method** (irep-based): extend the stack to `irep->nregs`,
  set `ci->pc` to `irep->iseq`, and jump to the new bytecode.
- **C function**: call `func(mrb, recv)` directly, then pop the
  call frame and store the return value.

### 5. Visibility Check

Private methods are only callable without an explicit receiver.
Protected methods are callable from the same class hierarchy.
Violations raise `NoMethodError`.

## Exception Handling

### setjmp/longjmp

By default, mruby uses `setjmp`/`longjmp` for exception control
flow:

```c
MRB_TRY(&c_jmp) {
  mrb->jmp = &c_jmp;
  /* dispatch loop */
}
MRB_CATCH(&c_jmp) {
  /* handle exception */
}
MRB_END_EXC(&c_jmp);
```

With `MRB_USE_CXX_EXCEPTION`, C++ `try`/`catch` is used instead.

### Handler Table

Each irep contains a catch handler table (appended after iseq in
memory) with entries for `rescue` and `ensure` blocks:

```text
mrb_irep_catch_handler
+-- type       RESCUE (0) or ENSURE (1)
+-- begin[4]   start PC of protected range
+-- end[4]     end PC of protected range
+-- target[4]  jump target when handler matches
```

### Unwinding Process

When an exception occurs:

1. Search the current irep's catch handler table (reverse order)
   for a handler covering the current PC
2. If an `ensure` handler is found: execute it (may re-raise)
3. If a `rescue` handler is found: jump to handler code
4. If no handler: pop the call frame (`cipop`) and repeat with
   the parent frame
5. `CINFO_DIRECT` frames are destroyed during propagation

## Block and Closure Handling

### REnv (Environment)

Closures capture their enclosing scope's variables through `REnv`:

```text
REnv
+-- stack      pointer to captured variable values
+-- cxt        owning context (NULL if detached from stack)
+-- mid        method symbol
+-- flags      length, block index, visibility
```

While the defining scope is active, `REnv::stack` points directly
into the VM value stack (shared). This avoids copying.

### Environment Unsharing

When a closure outlives its defining scope, `mrb_env_unshare()`
copies the captured variables from the stack to a heap-allocated
buffer:

```c
mrb_env_unshare(mrb, env, noraise);
```

After unsharing, `MRB_ENV_CLOSE(env)` sets `cxt = NULL` to indicate
the environment is detached. A write barrier is issued for GC
correctness.

### Proc Types

| Flag                | Meaning                        |
| ------------------- | ------------------------------ |
| `MRB_PROC_CFUNC_FL` | C function (not irep-based)    |
| `MRB_PROC_STRICT`   | Lambda (strict argument check) |
| `MRB_PROC_ORPHAN`   | No environment attachment      |
| `MRB_PROC_ENVSET`   | Has captured environment       |
| `MRB_PROC_SCOPE`    | Defines a new variable scope   |

## Fiber Switching

Fibers are lightweight coroutines. Each fiber has its own
`mrb_context` with separate value and call info stacks.

### Fiber States

```text
CREATED --> RUNNING --> SUSPENDED --> TERMINATED
                |           ^
                +-----------+
                  (yield/resume)
            TRANSFERRED (via Fiber#transfer)
```

### Context Switch

On `Fiber#resume`:

1. Save current context state
2. Set `mrb->c` to the fiber's context
3. Push arguments onto the fiber's stack
4. Continue execution in the fiber

On `Fiber.yield`:

1. Save fiber context
2. Restore the parent context (`mrb->c = c->prev`)
3. Return yield values to the parent

### Fiber Termination

When a fiber completes (`fiber_terminate`):

1. Unshare any environments that reference the fiber's stack
2. Set status to `TERMINATED`
3. Free the fiber's stacks
4. Switch to the previous context

### C Function Boundary

Fibers cannot yield across C function boundaries. You cannot call
`Fiber.yield` from within a C-implemented method (except via
`mrb_fiber_yield` at return). This is because C call frames cannot
be suspended and resumed.

## GC Integration

The VM saves the arena index at the start of the dispatch loop:

```c
int ai = mrb_gc_arena_save(mrb);
```

After each C function call, the arena is shrunk back:

```c
mrb_gc_arena_shrink(mrb, ai);
```

This prevents temporary objects created by C functions from
accumulating in the arena.

Write barriers are issued when environments are detached or closed,
ensuring the incremental GC correctly tracks live references.

## Source Files

| File                    | Contents                                       |
| ----------------------- | ---------------------------------------------- |
| `src/vm.c`              | Dispatch loop, method invocation (~1900 lines) |
| `include/mruby.h`       | `mrb_state`, `mrb_callinfo`, `mrb_context`     |
| `include/mruby/proc.h`  | `RProc`, `REnv` structures                     |
| `include/mruby/throw.h` | `MRB_TRY`/`MRB_CATCH` macros                   |
