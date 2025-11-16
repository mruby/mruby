# Security Policy

## Reporting a Vulnerability

To report a security vulnerability, please email the mruby team at <matz@ruby.or.jp>. We appreciate your efforts to disclose your findings responsibly.

## Scope

mruby is an embeddable Ruby implementation. Its security model is designed for integration into a host application, which is responsible for sandboxing and resource management. This policy defines what we consider a security vulnerability within the mruby interpreter itself.

### High Priority Security Vulnerabilities

We consider the following issues to be **high priority security vulnerabilities**:

- **Remote Code Execution (RCE)**: The ability to execute arbitrary machine code or shell commands from within a Ruby script, beyond the intended execution scope of the script itself.

### Lower Priority: Crashes (Preferably Report as Bugs)

We **accept but deprioritize** the following issues. We recommend reporting them as **bug reports** on our issue tracker rather than security reports:

- **VM Crash on Valid Ruby Code**: Segmentation faults, assertion failures, or other interpreter crashes triggered by syntactically and semantically valid Ruby scripts.
  - _Recommendation_: Please report these as bugs on our issue tracker.
  - _Rationale_: While we will fix these issues, they typically only result in denial of service (DoS), not arbitrary code execution. They are lower priority than RCE vulnerabilities.
  - _Note_: This does not include standard Ruby exceptions like `TypeError` or `ZeroDivisionError`, which are expected behavior.
  - _Example_: A segmentation fault when running `[1, 2, 3].map { |x| x * 2 }` is best reported as a bug.

### Out of Scope: Not Considered Security Vulnerabilities

We do **not** consider the following issues to be security vulnerabilities:

- **Resource Exhaustion**: Infinite loops, excessive memory allocation, or high CPU usage originating from a Ruby script.
  - _Rationale_: The host application is responsible for implementing resource limits, sandboxing, and execution timeouts. mruby provides the execution engine; the host provides the constraints.
  - _Example_: `loop {}` or `"a" * (2**30)` are not vulnerabilities, even if they lead to memory or CPU exhaustion.

- **Crashes from Malformed Bytecode**: Crashes resulting from loading or executing corrupted or intentionally malformed `.mrb` files.
  - _Rationale_: mruby's bytecode format is not a security boundary. Applications should only execute bytecode from trusted sources.
  - _Example_: A crash discovered by fuzzing `.mrb` files is not considered a vulnerability.

- **Crashes from C API Misuse**: Crashes caused by incorrect usage of mruby's C API from the embedding application.
  - _Rationale_: The C API is a trusted interface for developers. The caller is responsible for adhering to the API contract (e.g., not passing `NULL` pointers, managing object lifetimes correctly).
  - _Example_: Calling `mrb_funcall()` with an invalid `mrb_state*` pointer is not a vulnerability.

- **Theoretical Undefined Behavior (UB)**: Issues reported by tools like ASAN, UBSan, or Valgrind that do not lead to a demonstrable crash or exploitable behavior in practice.
  - _Rationale_: While we strive for clean, well-defined code, our focus is on practical security impact. We prioritize fixing UB that is exploitable over issues that are purely theoretical.
  - _Example_: An integer overflow in an intermediate calculation that gets handled correctly before affecting program output or control flow.

- **Warnings on Large Memory Allocations**: Tooling warnings related to large memory allocations that do not result in a crash.
  - _Rationale_: mruby is designed to handle `malloc(3)` returning `NULL` on large allocation requests. This is considered graceful error handling, not a vulnerability.

### Summary

- **High Priority Security Reports**: Remote code execution vulnerabilities.
- **Accepted (but preferably as bug reports)**: VM crashes from valid Ruby code.
- **Not Accepted as Security Issues**: Resource exhaustion, malformed bytecode, C API misuse, theoretical undefined behavior, or allocation warnings.
