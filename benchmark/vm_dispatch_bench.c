/*
 * VM Dispatch Micro-benchmark
 *
 * This benchmark measures the raw dispatch overhead of the mruby VM
 * by executing minimal bytecode sequences.
 *
 * Compile:
 *   cc -O2 -I include -I build/host/include \
 *      benchmark/vm_dispatch_bench.c \
 *      build/host/lib/libmruby.a -lm -o vm_dispatch_bench
 *
 * Run:
 *   ./vm_dispatch_bench
 */

#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/string.h>
#include <mruby/proc.h>
#include <stdio.h>
#include <time.h>

#define ITERATIONS 10

static double
get_time_ms(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
}

static void
run_benchmark(mrb_state *mrb, const char *name, const char *code, int iterations)
{
  double times[ITERATIONS];
  double total = 0.0;
  double min_time = 1e9;
  double max_time = 0.0;

  /* Compile once */
  mrbc_context *cxt = mrbc_context_new(mrb);
  struct mrb_parser_state *p = mrb_parse_string(mrb, code, cxt);
  if (!p || p->nerr > 0) {
    fprintf(stderr, "Failed to parse: %s\n", name);
    if (p) mrb_parser_free(p);
    mrbc_context_free(mrb, cxt);
    return;
  }
  struct RProc *proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  mrbc_context_free(mrb, cxt);

  if (!proc) {
    fprintf(stderr, "Failed to compile: %s\n", name);
    return;
  }

  /* Warm up */
  for (int i = 0; i < 3; i++) {
    mrb_top_run(mrb, proc, mrb_top_self(mrb), 0);
    mrb->exc = NULL;
  }

  /* Measure */
  for (int i = 0; i < iterations; i++) {
    mrb_gc_arena_save(mrb);
    mrb_full_gc(mrb);

    double t0 = get_time_ms();
    mrb_top_run(mrb, proc, mrb_top_self(mrb), 0);
    double t1 = get_time_ms();

    times[i] = t1 - t0;
    total += times[i];
    if (times[i] < min_time) min_time = times[i];
    if (times[i] > max_time) max_time = times[i];

    mrb->exc = NULL;
    mrb_gc_arena_restore(mrb, 0);
  }

  double avg = total / iterations;
  printf("%-30s avg: %8.2f ms  min: %8.2f ms  max: %8.2f ms\n",
         name, avg, min_time, max_time);
}

int
main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  if (!mrb) {
    fprintf(stderr, "Failed to create mrb_state\n");
    return 1;
  }

  printf("========================================\n");
  printf("mruby VM Dispatch Micro-benchmarks\n");
  printf("========================================\n\n");

  /* 1. Pure dispatch overhead */
  printf("--- Dispatch Overhead ---\n");

  run_benchmark(mrb, "empty_loop_1M",
    "i = 0; while i < 1000000; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "empty_loop_10M",
    "i = 0; while i < 10000000; i += 1; end", ITERATIONS);

  /* 2. Arithmetic operations */
  printf("\n--- Arithmetic ---\n");

  run_benchmark(mrb, "int_add_1M",
    "x = 0; i = 0; while i < 1000000; x = x + 1; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "int_mul_1M",
    "x = 1; i = 0; while i < 1000000; x = x * 1; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "float_add_1M",
    "x = 0.0; i = 0; while i < 1000000; x = x + 1.0; i += 1; end", ITERATIONS);

  /* 3. Method calls */
  printf("\n--- Method Calls ---\n");

  run_benchmark(mrb, "empty_method_100K",
    "class X; def m; end; end; "
    "o = X.new; i = 0; while i < 100000; o.m; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "method_1arg_100K",
    "class Y; def m(a); a; end; end; "
    "o = Y.new; i = 0; while i < 100000; o.m(1); i += 1; end", ITERATIONS);

  run_benchmark(mrb, "method_2arg_100K",
    "class Z; def m(a,b); a+b; end; end; "
    "o = Z.new; i = 0; while i < 100000; o.m(1,2); i += 1; end", ITERATIONS);

  /* 4. Array access */
  printf("\n--- Array/Hash ---\n");

  run_benchmark(mrb, "array_read_1M",
    "a = [0,1,2,3,4,5,6,7,8,9]; "
    "i = 0; s = 0; while i < 1000000; s += a[i % 10]; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "array_write_1M",
    "a = [0,0,0,0,0,0,0,0,0,0]; "
    "i = 0; while i < 1000000; a[i % 10] = i; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "hash_read_100K",
    "h = {0=>0,1=>1,2=>2,3=>3,4=>4,5=>5,6=>6,7=>7,8=>8,9=>9}; "
    "i = 0; s = 0; while i < 100000; s += h[i % 10]; i += 1; end", ITERATIONS);

  /* 5. Comparison and branching */
  printf("\n--- Comparison/Branch ---\n");

  run_benchmark(mrb, "lt_compare_1M",
    "i = 0; c = 0; while i < 1000000; c += 1 if i < 500000; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "eq_compare_1M",
    "i = 0; c = 0; while i < 1000000; c += 1 if i == 500000; i += 1; end", ITERATIONS);

  /* 6. Block calls */
  printf("\n--- Blocks ---\n");

  run_benchmark(mrb, "times_100K",
    "s = 0; 100000.times { |i| s += i }", ITERATIONS);

  run_benchmark(mrb, "each_100K",
    "a = (0...1000).to_a; s = 0; 100.times { a.each { |x| s += x } }", ITERATIONS);

  /* 7. Recursion */
  printf("\n--- Recursion ---\n");

  run_benchmark(mrb, "fib_25",
    "def fib(n); n < 2 ? n : fib(n-1) + fib(n-2); end; fib(25)", ITERATIONS);

  run_benchmark(mrb, "fib_30",
    "def fib(n); n < 2 ? n : fib(n-1) + fib(n-2); end; fib(30)", ITERATIONS);

  /* 8. Local variable access */
  printf("\n--- Local Variables ---\n");

  run_benchmark(mrb, "few_vars_1M",
    "i = 0; a = 0; b = 0; "
    "while i < 1000000; a += 1; b += 1; i += 1; end", ITERATIONS);

  run_benchmark(mrb, "many_vars_1M",
    "i = 0; a = 0; b = 0; c = 0; d = 0; e = 0; f = 0; g = 0; h = 0; "
    "while i < 1000000; a += 1; b += 1; c += 1; d += 1; e += 1; "
    "f += 1; g += 1; h += 1; i += 1; end", ITERATIONS);

  printf("\n========================================\n");
  printf("Benchmark complete\n");
  printf("========================================\n");

  mrb_close(mrb);
  return 0;
}
