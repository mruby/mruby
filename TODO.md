# Things to Do in the future

# After mruby 3.4

- parser and code generator independent from `mrb_state` (picoruby?)
- pattern matching (case/in syntax, array/hash patterns, guards, etc.)
- iv/hash entry cache
- method inline caching improvements (cache method lookup results)
- more peephole optimization (if possible)
- built-in profiler (method call tracing, stack profiling, detailed memory analysis)
- improved REPL (mirb) features (multi-line, syntax highlighting, completion)
- configurable memory pools (per-object-type, memory-constrained devices)
- suspend/resume VM state (serialize/deserialize for power cycling)
- CMake build support (better IDE integration, standard C tooling)

# Things to do (Things we need to consider)

- special variables ($1,$2..)
