/*
** codegen.c - mruby code generator
**
** See Copyright Notice in mruby.h
*/

/*
 * ## Code Generator
 *
 * This file implements the mruby code generator, a crucial component of the mruby
 * compilation pipeline. Its primary responsibility is to translate the Abstract
 * Syntax Tree (AST), produced by the parser, into mruby bytecode (Instruction
 * Sequence - iseq).
 *
 * ### Key Operational Aspects:
 *
 * - **AST Traversal:** The generator walks through the AST nodes, processing each
 *   node type and emitting corresponding bytecode instructions.
 * - **Scope Management:** It manages lexical scopes, keeping track of local
 *   variables, upvalues (variables from enclosing scopes), and register
 *   allocation within each scope. This is vital for correct variable access
 *   and lifetime.
 * - **Opcode Generation:** For different AST node types (e.g., literals,
 *   arithmetic operations, control flow statements, method calls, variable
 *   assignments), specific opcodes are generated. This involves selecting the
 *   appropriate instruction and its operands.
 * - **Loop Handling:** It provides mechanisms to correctly generate bytecode for
 *   various loop constructs (e.g., `while`, `for`, `until`), including managing
 *   `break`, `next`, and `redo` statements by patching jump addresses.
 * - **Instruction Sequence (iseq):** The output of this process is an `mrb_irep`
 *   structure, which contains the generated instruction sequence (iseq), literal
 *   pools, symbol tables, and other metadata required for execution by the
 *   mruby virtual machine.
 * - **Error Handling:** Includes mechanisms for reporting errors encountered
 *   during code generation, such as syntax errors not caught by the parser or
 *   semantic errors.
 *
 * This code generator is essential for transforming human-readable mruby code
 * into a format that the mruby VM can execute efficiently.
 */

#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/dump.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include <mruby/debug.h>
#include <mruby/presym.h>
#include "node.h"
#include <mruby/opcode.h>
#include <mruby/re.h>
#include <mruby/throw.h>
#include <ctype.h>
#include <string.h>
#include <mruby/internal.h>

/* Wrappers for mruby's memory management functions. */
#define mrbc_malloc(s) mrb_basic_alloc_func(NULL,(s))  /* Allocates memory. */
#define mrbc_realloc(p,s) mrb_basic_alloc_func((p),(s)) /* Reallocates memory. */
#define mrbc_free(p) mrb_basic_alloc_func((p),0)     /* Frees memory. */

#ifndef MRB_CODEGEN_LEVEL_MAX
/* Maximum recursion depth for the codegen function to prevent stack overflows. */
#define MRB_CODEGEN_LEVEL_MAX 256
#endif

/* Maximum number of arguments for some opcodes like OP_SUPER or OP_ARGARY. */
#define MAXARG_S (1<<16)

/* Macro to detect (0 . 0) separators in literal arrays */
#define IS_LITERAL_DELIM(node) \
  ((node) && (node)->car && \
   (node)->car->car == NULL && \
   (node)->car->cdr == NULL)

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;

/* Represents the different kinds of loops or blocks encountered during code generation. */
enum looptype {
  LOOP_NORMAL,  /* A standard loop construct like `while` or `until`. */
  LOOP_BLOCK,   /* A block or lambda. */
  LOOP_FOR,     /* A `for` loop. */
  LOOP_BEGIN,   /* A `begin...end` block (often with `rescue` or `ensure`). */
  LOOP_RESCUE,  /* The `rescue` part of a `begin...rescue...end` block. */
};

/* Information about a loop currently being compiled, used for `break`, `next`, `redo`, etc. */
struct loopinfo {
  enum looptype type;           /* Type of the loop, using `enum looptype`. */
  uint32_t pc0;                 /* Jump destination for `next`, or start of loop for `retry` in `rescue`. */
  uint32_t pc1;                 /* Jump destination for `redo`. */
  uint32_t pc2;                 /* Jump destination for `break`. */
  int reg;                      /* Register to store the loop's return value (e.g., from `break val`), or -1 if no value. */
  struct loopinfo *prev;        /* Pointer to the previous `loopinfo` in a linked list (for nested loops). */
};

/* Represents the state of the code generator for a particular lexical scope. */
typedef struct scope {
  mrb_state *mrb;               /* Pointer to the mruby state. */
  mempool *mpool;               /* Pointer to the memory pool for this scope's allocations. */

  struct scope *prev;           /* Pointer to the previous (enclosing) scope. */

  node *lv;                     /* AST node representing the list of local variables in this scope. */

  uint16_t sp;                  /* Current stack pointer (register index) within this scope. */
  uint32_t pc;                  /* Current program counter (instruction index) for the ISEQ being generated. */
  uint32_t lastpc;              /* Program counter of the previously emitted instruction (used for peephole optimization). */
  uint32_t lastlabel;           /* Program counter of the last label emitted (inhibits some peephole optimizations). */
  uint16_t ainfo:15;            /* Argument information bitfield (counts for req, opt, rest, post, key, kdict, block). */
  mrb_bool mscope:1;            /* Boolean flag: true if this is a method/module/class scope (not a block). */

  struct loopinfo *loop;        /* Pointer to the current innermost `loopinfo` structure for this scope. */
  mrb_sym filename_sym;         /* `mrb_sym` representing the current filename. */
  uint16_t lineno;              /* Current line number being processed. */

  mrb_code *iseq;               /* Pointer to the dynamically growing array of `mrb_code` (instructions). */
  uint16_t *lines;              /* Array to store line numbers corresponding to each instruction (for debugging). */
  uint32_t icapa;               /* Current capacity of the `iseq` and `lines` arrays. */

  mrb_irep *irep;               /* Pointer to the `mrb_irep` (instruction sequence representation) being built. */
  mrb_irep_pool *pool;          /* Pointer to the literal pool for the `irep`. */
  mrb_sym *syms;                /* Pointer to the symbol list for the `irep`. */
  mrb_irep **reps;              /* Pointer to the array of child `irep`s (for nested blocks/methods). */
  struct mrb_irep_catch_handler *catch_table; /* Pointer to the table of catch handlers for this scope. */
  uint32_t pcapa, scapa, rcapa; /* Current capacities of the `pool`, `syms`, and `reps` arrays respectively. */

  uint16_t nlocals;             /* Number of local variables in this scope. */
  uint16_t nregs;               /* Number of registers used in this scope (maximum value of `sp`). */
  int ai;                       /* Arena index for mruby's garbage collector. */

  int debug_start_pos;          /* Starting ISEQ position for the current debug file information. */
  uint16_t filename_index;      /* Index of the current filename in the parser's filename table. */
  parser_state* parser;         /* Pointer to the `mrb_parser_state`. */

  int rlev;                     /* Recursion level counter for `codegen` calls, to prevent stack overflow. */
} codegen_scope;

static codegen_scope* scope_new(mrb_state *mrb, codegen_scope *prev, node *lv);
static void scope_finish(codegen_scope *s);
static struct loopinfo *loop_push(codegen_scope *s, enum looptype t);
static void loop_break(codegen_scope *s, node *tree);
static void loop_pop(codegen_scope *s, int val);

/*
 * The search for catch handlers starts at the end of the table in mrb_vm_run().
 * Therefore, the next handler to be added must meet one of the following conditions.
 * - Larger start position
 * - Same start position but smaller end position
 */
static int catch_handler_new(codegen_scope *s);
static void catch_handler_set(codegen_scope *s, int ent, enum mrb_catch_type type, uint32_t begin, uint32_t end, uint32_t target);

static void gen_massignment(codegen_scope *s, node *tree, int sp, int val);
static void codegen_masgn(codegen_scope *s, node *varnode, node *rhs, int sp, int val);
static void gen_assignment(codegen_scope *s, node *tree, node *rhs, int sp, int val);
static void codegen_call_assign(codegen_scope *s, node *varnode, node *rhs, int sp, int val);

static void codegen(codegen_scope *s, node *tree, int val);
static void raise_error(codegen_scope *s, const char *msg);

/* Forward declarations for helper functions */
static struct mrb_ast_var_header* get_var_header(node *n);

/* NULL-safe node type accessor macro */
#define node_type(n) ((n) ? NODE_TYPE(n) : (enum node_type)0)

/*
 * Reports a compilation error encountered during code generation.
 *
 * This function formats an error message, typically including the filename
 * and line number where the error occurred. It then triggers a longjmp
 * to unwind the compilation process, effectively halting further code generation.
 *
 * @param s The current code generation scope.
 * @param message The error message string.
 */
static void
codegen_error(codegen_scope *s, const char *message)
{
  if (!s) return;
#ifndef MRB_NO_STDIO
  if (s->filename_sym && s->lineno) {
    const char *filename = mrb_sym_name_len(s->mrb, s->filename_sym, NULL);
    fprintf(stderr, "%s:%d: %s\n", filename, s->lineno, message);
  }
  else {
    fprintf(stderr, "%s\n", message);
  }
#endif
  while (s->prev) {
    codegen_scope *tmp = s->prev;
    if (s->irep) {
      mrbc_free(s->iseq);
      for (int i=0; i<s->irep->plen; i++) {
        mrb_irep_pool *p = &s->pool[i];
        if ((p->tt & 0x3) == IREP_TT_STR || p->tt == IREP_TT_BIGINT) {
          mrbc_free((void*)p->u.str);
        }
      }
      mrbc_free(s->pool);
      mrbc_free(s->syms);
      mrbc_free(s->catch_table);
      if (s->reps) {
        /* copied from mrb_irep_free() in state.c */
        for (int i=0; i<s->irep->rlen; i++) {
          if (s->reps[i])
            mrb_irep_decref(s->mrb, (mrb_irep*)s->reps[i]);
        }
        mrbc_free(s->reps);
      }
      mrbc_free(s->lines);
    }
    mempool_close(s->mpool);
    s = tmp;
  }
  MRB_THROW(s->mrb->jmp);
}

/*
 * Allocates memory from the memory pool associated with the current codegen_scope.
 *
 * This function is used for allocations that are expected to have the same
 * lifetime as the current scope. The memory allocated via this function will be
 * freed automatically when the scope is finished and its memory pool is closed.
 * It calls `codegen_error` if allocation fails.
 *
 * @param s The current code generation scope.
 * @param len The number of bytes to allocate.
 * @return A pointer to the allocated memory.
 */
static void*
codegen_palloc(codegen_scope *s, size_t len)
{
  void *p = mempool_alloc(s->mpool, len);

  if (!p) codegen_error(s, "pool memory allocation");
  return p;
}

/*
 * Checks if instruction operands `a` or `b` exceed 8 bits (0xff).
 *
 * If the parser option `no_ext_ops` is set (disallowing OP_EXT1/2/3),
 * and either operand is larger than 0xff, this function calls `codegen_error`
 * to report that an extended opcode would be required.
 *
 * @param s The current code generation scope.
 * @param a The first operand.
 * @param b The second operand.
 */
static void
check_no_ext_ops(codegen_scope *s, uint16_t a, uint16_t b)
{
  if (s->parser->no_ext_ops && (a | b) > 0xff) {
    codegen_error(s, "need OP_EXTs instruction (currently OP_EXTs are prohibited)");
  }
}

/*
 * Creates a new label by returning the current program counter (pc)
 * and updating `s->lastlabel` to this value.
 *
 * Marking a PC as a label (`s->lastlabel = s->pc`) can inhibit certain
 * peephole optimizations that might otherwise modify instructions at this label.
 *
 * @param s The current code generation scope.
 * @return The current program counter, which is now marked as a label.
 */
static int
new_label(codegen_scope *s)
{
  return s->lastlabel = s->pc;
}

/*
 * Emits a single byte (`i`) into the instruction sequence (`s->iseq`)
 * at the specified program counter (`pc`).
 *
 * This function handles dynamic resizing of the `iseq` buffer and the
 * associated `lines` array (if line number tracking is enabled).
 * It also records the current line number (`s->lineno`) for the emitted
 * instruction in `s->lines[pc]`.
 *
 * @param s The current code generation scope.
 * @param pc The program counter where the byte should be emitted.
 * @param i The byte to emit.
 */
static void
emit_B(codegen_scope *s, uint32_t pc, uint8_t i)
{
  if (pc >= s->icapa) {
    if (pc == UINT32_MAX) {
      codegen_error(s, "too big code block");
    }
    if (pc >= UINT32_MAX / 2) {
      pc = UINT32_MAX;
    }
    else {
      s->icapa *= 2;
    }
    s->iseq = (mrb_code*)mrbc_realloc(s->iseq, sizeof(mrb_code)*s->icapa);
    if (s->lines) {
      s->lines = (uint16_t*)mrbc_realloc(s->lines, sizeof(uint16_t)*s->icapa);
    }
  }
  if (s->lines) {
    if (s->lineno > 0 || pc == 0)
      s->lines[pc] = s->lineno;
    else
      s->lines[pc] = s->lines[pc-1];
  }
  s->iseq[pc] = i;
}

/*
 * Emits a 2-byte short integer (`i`) into the instruction sequence at `pc`.
 * The short is emitted in big-endian format (most significant byte first).
 * This is achieved by calling `emit_B` twice.
 *
 * @param s The current code generation scope.
 * @param pc The program counter where the short should be emitted.
 * @param i The 2-byte short to emit.
 */
static void
emit_S(codegen_scope *s, int pc, uint16_t i)
{
  uint8_t hi = i>>8;
  uint8_t lo = i&0xff;

  emit_B(s, pc,   hi);
  emit_B(s, pc+1, lo);
}

/*
 * Generates (emits) a single byte (`i`) at the current program counter (`s->pc`)
 * and then increments `s->pc` by 1.
 *
 * @param s The current code generation scope.
 * @param i The byte to emit.
 */
static void
gen_B(codegen_scope *s, uint8_t i)
{
  emit_B(s, s->pc, i);
  s->pc++;
}

/*
 * Generates (emits) a 2-byte short integer (`i`) at the current program
 * counter (`s->pc`) and then increments `s->pc` by 2.
 *
 * @param s The current code generation scope.
 * @param i The 2-byte short to emit.
 */
static void
gen_S(codegen_scope *s, uint16_t i)
{
  emit_S(s, s->pc, i);
  s->pc += 2;
}

/*
 * Generates an opcode `i` that takes no operands.
 * Updates `s->lastpc` to the current `s->pc` before emitting.
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 */
static void
genop_0(codegen_scope *s, mrb_code i)
{
  s->lastpc = s->pc;
  gen_B(s, i);
}

/*
 * Generates an opcode `i` with a single 16-bit operand `a`.
 * If `a` is larger than 0xFF (255), it prepends `OP_EXT1` and emits `a` as a short.
 * Otherwise, it emits `a` as a single byte.
 * Updates `s->lastpc`.
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The 16-bit operand.
 */
static void
genop_1(codegen_scope *s, mrb_code i, uint16_t a)
{
  s->lastpc = s->pc;
  check_no_ext_ops(s, a, 0);
  if (a > 0xff) {
    gen_B(s, OP_EXT1);
    gen_B(s, i);
    gen_S(s, a);
  }
  else {
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
  }
}

/*
 * Generates an opcode `i` with two 16-bit operands `a` and `b`.
 * It handles operand extensions (`OP_EXT1`, `OP_EXT2`, `OP_EXT3`)
 * based on whether `a` and/or `b` are larger than 0xFF.
 * Updates `s->lastpc`.
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The first 16-bit operand.
 * @param b The second 16-bit operand.
 */
static void
genop_2(codegen_scope *s, mrb_code i, uint16_t a, uint16_t b)
{
  s->lastpc = s->pc;
  check_no_ext_ops(s, a, b);
  if (a > 0xff && b > 0xff) {
    gen_B(s, OP_EXT3);
    gen_B(s, i);
    gen_S(s, a);
    gen_S(s, b);
  }
  else if (b > 0xff) {
    gen_B(s, OP_EXT2);
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
    gen_S(s, b);
  }
  else if (a > 0xff) {
    gen_B(s, OP_EXT1);
    gen_B(s, i);
    gen_S(s, a);
    gen_B(s, (uint8_t)b);
  }
  else {
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
    gen_B(s, (uint8_t)b);
  }
}

/*
 * Generates an opcode `i` with three operands `a`, `b`, and `c`.
 * It uses `genop_2` to emit `i`, `a`, and `b` (handling extensions for `a` and `b`),
 * and then emits `c` as a single byte using `gen_B`. `c` is assumed to fit in a byte.
 * Updates `s->lastpc` (via `genop_2`).
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The first 16-bit operand.
 * @param b The second 16-bit operand.
 * @param c The third 16-bit operand (emitted as a byte).
 */
static void
genop_3(codegen_scope *s, mrb_code i, uint16_t a, uint16_t b, uint16_t c)
{
  genop_2(s, i, a, b);
  gen_B(s, (uint8_t)c);
}

/*
 * Generates an opcode `i` with a 16-bit operand `a` and a 16-bit operand `b`.
 * Operand `a` is emitted using `genop_1` (which handles `OP_EXT1` if needed).
 * Operand `b` is emitted as a 2-byte short using `gen_S`.
 * Updates `s->lastpc` (via `genop_1`).
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The first 16-bit operand.
 * @param b The second 16-bit operand (emitted as a short).
 */
static void
genop_2S(codegen_scope *s, mrb_code i, uint16_t a, uint16_t b)
{
  genop_1(s, i, a);
  gen_S(s, b);
}

/*
 * Generates an opcode `i` with a 16-bit operand `a` and a 32-bit operand `b`.
 * Operand `a` is emitted using `genop_1` (handling `OP_EXT1`).
 * Operand `b` is emitted as two 2-byte shorts (high word then low word).
 * Updates `s->lastpc` (via `genop_1`).
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The first 16-bit operand.
 * @param b The 32-bit operand (emitted as two shorts).
 */
static void
genop_2SS(codegen_scope *s, mrb_code i, uint16_t a, uint32_t b)
{
  genop_1(s, i, a);
  gen_S(s, b>>16);
  gen_S(s, b&0xffff);
}

/*
 * Generates an opcode `i` followed by a 3-byte "wide" operand `a`.
 * The 3-byte operand is emitted as three separate bytes (a1, a2, a3).
 * Updates `s->lastpc`.
 *
 * @param s The current code generation scope.
 * @param i The opcode to generate.
 * @param a The 32-bit operand, of which the lower 24 bits are used.
 */
static void
genop_W(codegen_scope *s, mrb_code i, uint32_t a)
{
  uint8_t a1 = (a>>16) & 0xff;
  uint8_t a2 = (a>>8) & 0xff;
  uint8_t a3 = a & 0xff;

  s->lastpc = s->pc;
  gen_B(s, i);
  gen_B(s, a1);
  gen_B(s, a2);
  gen_B(s, a3);
}

/* Indicates whether a codegen function should produce a value on the stack (VAL) or not (NOVAL). */
#define NOVAL  0
#define VAL    1

static mrb_bool
no_optimize(codegen_scope *s)
{
  if (s && s->parser && s->parser->no_optimize)
    return TRUE;
  return FALSE;
}

/*
 * Decodes a mruby bytecode instruction starting at the given program counter `pc`.
 *
 * It reads the opcode and its operands from the bytecode stream and populates
 * a `mrb_insn_data` structure. This function handles standard opcodes as well
 * as extended opcodes (OP_EXT1, OP_EXT2, OP_EXT3) to correctly parse operands
 * of varying sizes. This is primarily used by the peephole optimizer and
 * instruction analysis utilities.
 *
 * @param pc Pointer to the start of the instruction in the bytecode.
 * @return A `mrb_insn_data` struct containing the decoded instruction,
 *         its operands (a, b, c), and the original address.
 */
struct mrb_insn_data
mrb_decode_insn(const mrb_code *pc)
{
  struct mrb_insn_data data = { 0 };
  if (pc == 0) return data;
  data.addr = pc;
  mrb_code insn = READ_B();
  uint16_t a = 0;
  uint16_t b = 0;
  uint16_t c = 0;

  switch (insn) {
#define FETCH_Z() /* empty */
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x (); break;
#include <mruby/ops.h>
#undef OPCODE
  }
  switch (insn) {
  case OP_EXT1:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _1 (); break;
#include <mruby/ops.h>
#undef OPCODE
    }
    break;
  case OP_EXT2:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _2 (); break;
#include <mruby/ops.h>
#undef OPCODE
    }
    break;
  case OP_EXT3:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _3 (); break;
#include <mruby/ops.h>
#undef OPCODE
    }
    break;
  default:
    break;
  }
  data.insn = insn;
  data.a = a;
  data.b = b;
  data.c = c;
  return data;
}

#undef OPCODE
#define Z 1
#define S 3
#define W 4
#define OPCODE(_,x) x,
/* instruction sizes */
static uint8_t mrb_insn_size[] = {
#define B 2
#define BB 3
#define BBB 4
#define BS 4
#define BSS 6
#include <mruby/ops.h>
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
};
/* EXT1 instruction sizes */
static uint8_t mrb_insn_size1[] = {
#define B 3
#define BB 4
#define BBB 5
#define BS 5
#define BSS 7
#include <mruby/ops.h>
#undef B
#undef BS
#undef BSS
};
/* EXT2 instruction sizes */
static uint8_t mrb_insn_size2[] = {
#define B 2
#define BS 4
#define BSS 6
#include <mruby/ops.h>
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
};
/* EXT3 instruction sizes */
#define B 3
#define BB 5
#define BBB 6
#define BS 5
#define BSS 7
static uint8_t mrb_insn_size3[] = {
#include <mruby/ops.h>
};
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
#undef OPCODE

/*
 * Finds the program counter (PC) of the instruction immediately preceding
 * the instruction at the given `pc`.
 *
 * It iterates backward through the already generated instruction sequence (`s->iseq`)
 * from its beginning up to `pc`, decoding each instruction to determine its size
 * and thus find the start of the previous instruction.
 *
 * @param s The current code generation scope.
 * @param pc Pointer to an instruction in `s->iseq`.
 * @return Pointer to the start of the instruction preceding the one at `pc`,
 *         or NULL if `pc` is at the beginning of `s->iseq`.
 */
static const mrb_code*
mrb_prev_pc(codegen_scope *s, const mrb_code *pc)
{
  const mrb_code *prev_pc = NULL;
  const mrb_code *i = s->iseq;

  mrb_assert(pc < s->iseq + s->icapa);
  while (i<pc) {
    prev_pc = i;
    switch (i[0]) {
    case OP_EXT1:
      i += mrb_insn_size1[i[1]] + 1;
      break;
    case OP_EXT2:
      i += mrb_insn_size2[i[1]] + 1;
      break;
    case OP_EXT3:
      i += mrb_insn_size3[i[1]] + 1;
      break;
    default:
      i += mrb_insn_size[i[0]];
      break;
    }
  }
  return prev_pc;
}

/* Gets the memory address of the current instruction pointed to by the program counter (pc). */
#define pc_addr(s) &((s)->iseq[(s)->pc])
/* Converts an instruction memory address to a program counter (pc) offset. */
#define addr_pc(s, addr) (uint32_t)((addr) - s->iseq)
/* Resets the program counter (pc) to the address of the previously generated instruction. Used in peephole optimizations. */
#define rewind_pc(s) s->pc = s->lastpc

/*
 * Decodes and returns the last instruction that was emitted into the
 * instruction sequence (`s->iseq`).
 * It uses `mrb_decode_insn` on the instruction at `s->iseq[s->lastpc]`.
 * If no instructions have been emitted (`s->pc == 0`), it returns a NOP.
 *
 * @param s The current code generation scope.
 * @return A `mrb_insn_data` struct for the last emitted instruction.
 */
static struct mrb_insn_data
mrb_last_insn(codegen_scope *s)
{
  if (s->pc == 0) {
    struct mrb_insn_data data = { OP_NOP, 0 };
    return data;
  }
  return mrb_decode_insn(&s->iseq[s->lastpc]);
}

/*
 * Determines if peephole optimizations should be disabled for the current instruction.
 * Peephole optimization is disabled if:
 * - General optimization is off (`no_optimize(s)` is true).
 * - The current program counter (`s->pc`) is the target of a label (`s->lastlabel == s->pc`).
 * - It's the beginning of the bytecode (`s->pc == 0`).
 * - The current PC is the same as the PC of the last emitted instruction (`s->pc == s->lastpc`),
 *   which can happen after a `rewind_pc`.
 *
 * @param s The current code generation scope.
 * @return TRUE if peephole optimizations should be skipped, FALSE otherwise.
 */
static mrb_bool
no_peephole(codegen_scope *s)
{
  return no_optimize(s) || s->lastlabel == s->pc || s->pc == 0 || s->pc == s->lastpc;
}

/* Sentinel value for jump offsets that are not yet determined and need to be linked later. */
#define JMPLINK_START UINT32_MAX

/*
 * Generates the 2-byte signed offset for a jump instruction.
 *
 * The `pc` argument is the absolute target program counter for the jump.
 * The function calculates the relative offset from the instruction *after*
 * the current jump instruction (i.e., `s->pc + 2` for the jump opcode and its offset)
 * to the target `pc`. This offset is then emitted as a 16-bit signed integer.
 * If the offset is too large to fit in 16 bits, it calls `codegen_error`.
 * If `pc` is `JMPLINK_START`, it emits an offset of 0 (placeholder for later patching).
 *
 * @param s The current code generation scope.
 * @param pc The absolute target program counter for the jump.
 */
static void
gen_jmpdst(codegen_scope *s, uint32_t pc)
{

  if (pc == JMPLINK_START) {
    pc = 0;
  }
  uint32_t pos2 = s->pc+2;
  int32_t off = pc - pos2;

  if (off > INT16_MAX || INT16_MIN > off) {
    codegen_error(s, "too big jump offset");
  }
  gen_S(s, (uint16_t)off);
}

/*
 * Generates an unconditional jump instruction `i` (e.g., OP_JMP)
 * that jumps to the absolute target program counter `pc`.
 *
 * It first emits the jump opcode `i` using `genop_0`, then emits
 * the calculated jump offset using `gen_jmpdst`.
 *
 * @param s The current code generation scope.
 * @param i The jump opcode to generate (e.g., OP_JMP).
 * @param pc The absolute target program counter.
 * @return The program counter where the jump offset was written. This is used for jump linking.
 */
static uint32_t
genjmp(codegen_scope *s, mrb_code i, uint32_t pc)
{
  uint32_t pos;

  genop_0(s, i);
  pos = s->pc;
  gen_jmpdst(s, pc);
  return pos;
}

#define genjmp_0(s,i) genjmp(s,i,JMPLINK_START)

/*
 * Generates a conditional jump instruction `i` (e.g., OP_JMPNOT, OP_JMPIF)
 * based on the value in register `a`, targeting the absolute program counter `pc`.
 *
 * This function includes several peephole optimizations:
 * - If the last instruction was a MOVE to register `a` from another temporary register,
 *   it rewinds and uses the source of the MOVE as the condition register.
 * - If the last instruction loaded a constant (nil, false, true, integer) into register `a`,
 *   it may optimize the jump:
 *     - If the condition is known at compile time (e.g., JMPNOT after LOADF), it can
 *       transform the conditional jump into an unconditional OP_JMP.
 *     - If the condition is known and makes the jump always/never taken, it can
 *       remove the jump entirely (returning JMPLINK_START to signify this).
 * The `val` parameter influences these optimizations: if `val` is false (NOVAL),
 * it implies the preceding instruction producing `a` might be removable if the jump
 * itself is optimized away.
 *
 * @param s The current code generation scope.
 * @param i The conditional jump opcode.
 * @param a The register index holding the condition value.
 * @param pc The absolute target program counter for the jump.
 * @param val Indicates if the value in register `a` from a previous instruction is needed
 *            beyond this conditional jump.
 * @return The program counter where the jump offset was written, or `JMPLINK_START` if the
 *         jump was optimized away.
 */
static uint32_t
genjmp2(codegen_scope *s, mrb_code i, uint16_t a, uint32_t pc, int val)
{
  uint32_t pos;

  if (!no_peephole(s) && !val) {
    struct mrb_insn_data data = mrb_last_insn(s);

    switch (data.insn) {
    case OP_MOVE:
      if (data.a == a && data.a > s->nlocals) {
        rewind_pc(s);
        a = data.b;
      }
      break;
    case OP_LOADNIL:
    case OP_LOADFALSE:
      if (data.a == a || data.a > s->nlocals) {
        s->pc = addr_pc(s, data.addr);
        if (i == OP_JMPNOT || (i == OP_JMPNIL && data.insn == OP_LOADNIL)) {
          return genjmp(s, OP_JMP, pc);
        }
        else {                  /* OP_JMPIF */
          return JMPLINK_START;
        }
      }
      break;
    case OP_LOADTRUE: case OP_LOADI8: case OP_LOADINEG: case OP_LOADI__1:
    case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
    case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
      if (data.a == a || data.a > s->nlocals) {
        s->pc = addr_pc(s, data.addr);
        if (i == OP_JMPIF) {
          return genjmp(s, OP_JMP, pc);
        }
        else {                  /* OP_JMPNOT and OP_JMPNIL */
          return JMPLINK_START;
        }
      }
      break;
    }
  }

  if (a > 0xff) {
    check_no_ext_ops(s, a, 0);
    gen_B(s, OP_EXT1);
    genop_0(s, i);
    gen_S(s, a);
  }
  else {
    genop_0(s, i);
    gen_B(s, (uint8_t)a);
  }
  pos = s->pc;
  gen_jmpdst(s, pc);
  return pos;
}

#define genjmp2_0(s,i,a,val) genjmp2(s,i,a,JMPLINK_START,val)

static mrb_bool get_int_operand(codegen_scope *s, struct mrb_insn_data *data, mrb_int *ns);
static void gen_int(codegen_scope *s, uint16_t dst, mrb_int i);

/*
 * Generates an OP_MOVE instruction to copy the value from register `src` to register `dst`.
 *
 * This function incorporates several peephole optimizations to avoid redundant moves or
 * to combine the move with preceding operations:
 * - If `dst` and `src` are the same, the function does nothing.
 * - If the previous instruction was also an `OP_MOVE` involving `src` or `dst`,
 *   it might combine or reorder them to eliminate redundant operations.
 * - If the previous instruction loaded a literal (nil, self, true, false, integer,
 *   symbol, string, etc.) into `src`, and `src` is a temporary register,
 *   this function can rewind the program counter and generate the load operation
 *   directly into `dst`, effectively eliminating the `OP_MOVE`.
 * - It can also perform constant folding for `OP_ADDI`/`OP_SUBI` if a sequence of
 *   `LOADI`, `MOVE`, `ADDI`/`SUBI` can be resolved at compile time.
 *
 * The `nopeep` parameter, if true, disables these peephole optimizations, forcing
 * the generation of a direct `OP_MOVE` instruction.
 *
 * @param s The current code generation scope.
 * @param dst The destination register index.
 * @param src The source register index.
 * @param nopeep If non-zero, disables peephole optimizations for this move.
 */
static void
gen_move(codegen_scope *s, uint16_t dst, uint16_t src, int nopeep)
{
  if (dst == src) return;
  if (!(nopeep || no_peephole(s))) {
    struct mrb_insn_data data = mrb_last_insn(s);

    switch (data.insn) {
    case OP_MOVE:
      if (dst == src) return;   /* remove useless MOVE */
      if (data.a == src) {
        if (data.b == dst)      /* skip swapping MOVE */
          return;
        if (data.a < s->nlocals) break;
        rewind_pc(s);
        s->lastpc = addr_pc(s, mrb_prev_pc(s, data.addr));
        gen_move(s, dst, data.b, FALSE);
        return;
      }
      if (dst == data.a) {      /* skip overwritten move */
        rewind_pc(s);
        s->lastpc = addr_pc(s, mrb_prev_pc(s, data.addr));
        gen_move(s, dst, src, FALSE);
        return;
      }
      break;
    case OP_LOADNIL: case OP_LOADSELF: case OP_LOADTRUE: case OP_LOADFALSE:
    case OP_LOADI__1:
    case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
    case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
      if (data.a != src || data.a < s->nlocals) break;
      rewind_pc(s);
      genop_1(s, data.insn, dst);
      return;
    case OP_HASH:
      if (data.b != 0) break;
      /* fall through */
    case OP_LOADI8: case OP_LOADINEG:
    case OP_LOADL: case OP_LOADSYM:
    case OP_GETGV: case OP_GETSV: case OP_GETIV: case OP_GETCV:
    case OP_GETCONST: case OP_STRING:
    case OP_LAMBDA: case OP_BLOCK: case OP_METHOD: case OP_BLKPUSH:
      if (data.a != src || data.a < s->nlocals) break;
      rewind_pc(s);
      genop_2(s, data.insn, dst, data.b);
      return;
    case OP_LOADI16:
      if (data.a != src || data.a < s->nlocals) break;
      rewind_pc(s);
      genop_2S(s, data.insn, dst, data.b);
      return;
    case OP_LOADI32:
      if (data.a != src || data.a < s->nlocals) break;
      else {
        uint32_t i = (uint32_t)data.b<<16|data.c;
        rewind_pc(s);
        genop_2SS(s, data.insn, dst, i);
      }
      return;
    case OP_ARRAY:
      if (data.a != src || data.a < s->nlocals || data.a < dst) break;
      rewind_pc(s);
      if (data.b == 0 || dst == data.a)
        genop_2(s, OP_ARRAY, dst, 0);
      else
        genop_3(s, OP_ARRAY2, dst, data.a, data.b);
      return;
    case OP_ARRAY2:
      if (data.a != src || data.a < s->nlocals || data.a < dst) break;
      rewind_pc(s);
      genop_3(s, OP_ARRAY2, dst, data.b, data.c);
      return;
    case OP_AREF:
    case OP_GETUPVAR:
      if (data.a != src || data.a < s->nlocals) break;
      rewind_pc(s);
      genop_3(s, data.insn, dst, data.b, data.c);
      return;
    case OP_ADDI: case OP_SUBI:
      if (addr_pc(s, data.addr) == s->lastlabel || data.a != src || data.a < s->nlocals) break;
      else {
        struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
        if (data0.insn != OP_MOVE || data0.a != data.a || data0.b != dst) break;
        if (addr_pc(s, data0.addr) != s->lastlabel) {
          /* constant folding */
          struct mrb_insn_data data1 = mrb_decode_insn(mrb_prev_pc(s, data0.addr));
          mrb_int n;
          if (data1.a == dst && get_int_operand(s, &data1, &n)) {
            if ((data.insn == OP_ADDI && !mrb_int_add_overflow(n, data.b, &n)) ||
                (data.insn == OP_SUBI && !mrb_int_sub_overflow(n, data.b, &n))) {
              s->pc = addr_pc(s, data1.addr);
              gen_int(s, dst, n);
              return;
            }
          }
        }
        /* ADDILV/SUBILV fusion: MOVE temp local; ADDI temp imm; MOVE local temp */
        /* -> ADDILV local temp imm (temp is working space for method fallback) */
        s->pc = addr_pc(s, data0.addr);
        genop_3(s, data.insn == OP_ADDI ? OP_ADDILV : OP_SUBILV, dst, data.a, data.b);
        return;
      }
      break;
    default:
      break;
    }
  }

  genop_2(s, OP_MOVE, dst, src);
  return;
}

/*
 * Searches for a local variable `id` in outer lexical scopes (upvalues).
 *
 * It first traverses the chain of enclosing `codegen_scope` structures
 * (linked by `s->prev`). If not found, it then traverses the chain of
 * `upper` RProc structures stored in the parser state.
 *
 * If the variable `id` is found in an outer scope:
 * - It returns `lv`, the number of lexical levels (scopes) to go up
 *   to find the variable.
 * - It sets the `*idx` output parameter to the variable's index within
 *   that outer scope's local variable table.
 *
 * If the variable is not found in any outer scope, it calls `codegen_error`
 * to report an error (e.g., "No anonymous block parameter", "Can't find local variables").
 *
 * @param s The current code generation scope from which the search begins.
 * @param id The `mrb_sym` (symbol) of the local variable to search for.
 * @param idx Output parameter: pointer to an integer where the index of the
 *            variable in its defining scope will be stored.
 * @return The lexical distance (number of scopes upwards) to the variable's
 *         defining scope.
 */
static int search_upvar(codegen_scope *s, mrb_sym id, int *idx);

/*
 * Generates an `OP_GETUPVAR` instruction to retrieve an upvalue.
 *
 * The upvalue `id` is first located using `search_upvar` to determine its
 * lexical level (`lv`) and index (`idx`) within that outer scope.
 * Then, an `OP_GETUPVAR` instruction is generated to load this upvalue
 * into the destination register `dst`.
 *
 * Peephole Optimization:
 * - If the immediately preceding instruction was an `OP_SETUPVAR` for the
 *   same upvalue (`id`), lexical level (`lv`), and destination register (`dst`),
 *   this `OP_GETUPVAR` is skipped as the value is already in the target register.
 *
 * @param s The current code generation scope.
 * @param dst The destination register index where the upvalue will be loaded.
 * @param id The `mrb_sym` (symbol) of the upvalue to retrieve.
 */
static void
gen_getupvar(codegen_scope *s, uint16_t dst, mrb_sym id)
{
  int idx;
  int lv = search_upvar(s, id, &idx);

  if (!no_peephole(s)) {
    struct mrb_insn_data data = mrb_last_insn(s);
    if (data.insn == OP_SETUPVAR && data.a == dst && data.b == idx && data.c == lv) {
      /* skip GETUPVAR right after SETUPVAR */
      return;
    }
  }
  genop_3(s, OP_GETUPVAR, dst, idx, lv);
}

/*
 * Generates an `OP_SETUPVAR` instruction to set an upvalue.
 *
 * The upvalue `id` is first located using `search_upvar` to determine its
 * lexical level (`lv`) and index (`idx`) within that outer scope.
 * Then, an `OP_SETUPVAR` instruction is generated to set this upvalue
 * using the value from register `dst`.
 *
 * Peephole Optimization:
 * - If the immediately preceding instruction was an `OP_MOVE` where register `dst`
 *   was the destination (`data.a == dst`), this function will rewind the program
 *   counter and use the source register of that `OP_MOVE` (`data.b`) as the source
 *   for `OP_SETUPVAR` instead. This effectively uses the original value before the move.
 *
 * @param s The current code generation scope.
 * @param dst The register index holding the value to set the upvalue to.
 * @param id The `mrb_sym` (symbol) of the upvalue to set.
 */
static void
gen_setupvar(codegen_scope *s, uint16_t dst, mrb_sym id)
{
  int idx;
  int lv = search_upvar(s, id, &idx);

  if (!no_peephole(s)) {
    struct mrb_insn_data data = mrb_last_insn(s);
    if (data.insn == OP_MOVE && data.a == dst) {
      dst = data.b;
      rewind_pc(s);
    }
  }
  genop_3(s, OP_SETUPVAR, dst, idx, lv);
}

/*
 * Generates a return instruction (e.g., `OP_RETURN`, `OP_RETURN_BLK`).
 *
 * This function emits the specified return opcode `op` with the source register `src`
 * containing the value to be returned.
 *
 * Peephole Optimization:
 * - If peephole optimization is enabled and the immediately preceding instruction
 *   was an `OP_MOVE` into the `src` register (`data.insn == OP_MOVE && src == data.a`),
 *   this function will rewind the program counter and generate the return instruction
 *   using the original source register of that `OP_MOVE` (`data.b`). This avoids
 *   a redundant move before returning.
 * - It also avoids emitting multiple consecutive `OP_RETURN` instructions.
 *
 * @param s The current code generation scope.
 * @param op The specific return opcode to generate (e.g., `OP_RETURN`, `OP_RETURN_BLK`).
 * @param src The register index holding the value to be returned.
 */
static void
gen_return(codegen_scope *s, uint8_t op, uint16_t src)
{
  if (no_peephole(s)) {
    genop_1(s, op, src);
  }
  else {
    struct mrb_insn_data data = mrb_last_insn(s);

    if (data.insn == OP_MOVE && src == data.a) {
      rewind_pc(s);
      genop_1(s, op, data.b);
    }
    else if (data.insn == OP_LOADSELF && src == data.a && op == OP_RETURN) {
      /* LOADSELF + RETURN -> RETSELF */
      rewind_pc(s);
      genop_0(s, OP_RETSELF);
    }
    else if (data.insn == OP_LOADNIL && src == data.a && op == OP_RETURN) {
      /* LOADNIL + RETURN -> RETNIL */
      rewind_pc(s);
      genop_0(s, OP_RETNIL);
    }
    else if (data.insn == OP_LOADTRUE && src == data.a && op == OP_RETURN) {
      /* LOADTRUE + RETURN -> RETTRUE */
      rewind_pc(s);
      genop_0(s, OP_RETTRUE);
    }
    else if (data.insn == OP_LOADFALSE && src == data.a && op == OP_RETURN) {
      /* LOADFALSE + RETURN -> RETFALSE */
      rewind_pc(s);
      genop_0(s, OP_RETFALSE);
    }
    else if (data.insn != OP_RETURN && data.insn != OP_RETSELF && data.insn != OP_RETNIL &&
             data.insn != OP_RETTRUE && data.insn != OP_RETFALSE) {
      genop_1(s, op, src);
    }
  }
}

/*
 * Attempts to extract a compile-time integer value from a given instruction.
 *
 * This function checks if the instruction described by `data` is one of
 * the integer loading opcodes (e.g., `OP_LOADI__1`, `OP_LOADINEG`, `OP_LOADI_0`
 * through `OP_LOADI_7`, `OP_LOADI8`, `OP_LOADI16`, `OP_LOADI32`) or `OP_LOADL`
 * where the literal pool entry is an integer.
 *
 * If successful, it stores the extracted integer value into the output
 * parameter `*n` and returns `TRUE`. Otherwise, it returns `FALSE`.
 *
 * @param s The current code generation scope (used to access the literal pool for `OP_LOADL`).
 * @param data Pointer to an `mrb_insn_data` structure describing the instruction.
 * @param n Output parameter: pointer to an `mrb_int` where the extracted integer
 *          value will be stored if successful.
 * @return `TRUE` if an integer value was successfully extracted, `FALSE` otherwise.
 */
static mrb_bool
get_int_operand(codegen_scope *s, struct mrb_insn_data *data, mrb_int *n)
{
  switch (data->insn) {
  case OP_LOADI__1:
    *n = -1;
    return TRUE;

  case OP_LOADINEG:
    *n = -data->b;
    return TRUE;

  case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
  case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
    *n = data->insn - OP_LOADI_0;
    return TRUE;

  case OP_LOADI8:
  case OP_LOADI16:
    *n = (int16_t)data->b;
    return TRUE;

  case OP_LOADI32:
    *n = (int32_t)((uint32_t)data->b<<16)+data->c;
    return TRUE;

  case OP_LOADL:
    {
      mrb_irep_pool *p = &s->pool[data->b];

      if (p->tt == IREP_TT_INT32) {
        *n = (mrb_int)p->u.i32;
      }
#ifdef MRB_INT64
      else if (p->tt == IREP_TT_INT64) {
        *n = (mrb_int)p->u.i64;
      }
#endif
      else {
        return FALSE;
      }
    }
    return TRUE;

  default:
    return FALSE;
  }
}

static int new_lit_str2(codegen_scope *s, const char *str1, mrb_int len1, const char *str2, mrb_int len2);
static int find_pool_str(codegen_scope *s, const char *str1, mrb_int len1, const char *str2, mrb_int len2);
static void gen_string(codegen_scope *s, node *list, int val);

/*
 * Reallocates or allocates memory for a string literal in the IREP's literal pool.
 *
 * This function is used when a string literal needs to be resized, typically
 * during string concatenation optimizations (`merge_op_string`).
 *
 * - If the original pool entry `p` pointed to a shared string (e.g., a string
 *   from read-only data, `IREP_TT_SSTR`), new memory is allocated for the resized string.
 * - If `p` was already a dynamically allocated string (`IREP_TT_STR`), its buffer
 *   is reallocated to the new `len`.
 *
 * After allocation/reallocation, the pool entry `p` is updated:
 * - Its type `tt` is set to `IREP_TT_STR` (or kept as `IREP_TT_STR`).
 * - The length in `tt` is updated to the new `len`.
 * - The string is null-terminated.
 * - `p->u.str` points to the new or reallocated buffer.
 *
 * @param s The current code generation scope.
 * @param p Pointer to the `mrb_irep_pool` entry for the string literal.
 * @param len The new length of the string (excluding the null terminator).
 */
static void
realloc_pool_str(codegen_scope *s, mrb_irep_pool *p, mrb_int len)
{
  char *str;
  mrb_int olen = p->tt >> 2;  /* original length */
  if ((p->tt & 3) == IREP_TT_SSTR) { /* Check if it's a shared/static string */
    const char *old = p->u.str;
    str = (char*)mrbc_malloc(len+1); /* Allocate new memory if it was shared */
    memcpy(str, old, olen);  /* Copy original content */
  }
  else { /* It's already a heap-allocated string */
    str = (char*)p->u.str;
    str = (char*)mrbc_realloc(str, len+1);
  }
  p->tt = (uint32_t)(len<<2 | IREP_TT_STR);
  str[len] = '\0';
  p->u.str = (const char*)str;
}

/*
 * Frees the memory associated with a string literal in the IREP's literal pool,
 * if it's not a shared (static) string.
 *
 * This function is typically called when a string literal pool entry is being
 * effectively removed or replaced due to optimizations like string merging.
 *
 * - It checks if the pool entry `p`'s type `tt` indicates it's a dynamically
 *   allocated string (not `IREP_TT_SSTR`).
 * - If so, it frees the memory pointed to by `p->u.str`.
 * - It then sets `p->u.str` to `NULL` and decrements the total count of literals
 *   in the pool (`s->irep->plen`). Note: This decrement might be problematic if
 *   pool entries are not compacted, as it could lead to an incorrect `plen`.
 *
 * @param s The current code generation scope.
 * @param p Pointer to the `mrb_irep_pool` entry of the string to be freed.
 */
static void
free_pool_str(codegen_scope *s, mrb_irep_pool *p)
{
  if ((p->tt & 3) != IREP_TT_SSTR) { /* Only free if not a shared/static string */
    mrbc_free((char*)p->u.str);
  }
  p->u.str = NULL;
  s->irep->plen--; /* Decrements the count of pool entries. */
}

/*
 * Performs a peephole optimization for string concatenation.
 *
 * This function is called when an `OP_ADD` (string concatenation) instruction
 * is encountered. It checks if the two operands to `OP_ADD` were themselves
 * loaded by `OP_STRING` instructions (i.e., string literals from the pool
 * at indices `b1` and `b2`).
 *
 * If this pattern is found, `merge_op_string` attempts to:
 * 1. Determine if the literal pool entries `b1` and `b2` are used by any other
 *    `OP_STRING` instructions prior to the instruction at `pc` (the start of the
 *    first `OP_STRING` in the sequence).
 * 2. Based on this usage (`used` flags), it decides on a strategy to merge
 *    the string content of `b1` and `b2`:
 *    - If neither `b1` nor `b2` is otherwise referenced, or only `b2` is, it reuses
 *      and resizes pool entry `b1` to hold the concatenated string. If `b2` was
 *      the last entry in the pool and not shared, `b2` is freed.
 *    - If only `b1` is referenced, it reuses and resizes pool entry `b2`.
 *    - If both `b1` and `b2` are referenced by other instructions, it creates a
 *      new literal pool entry for the concatenated string.
 * 3. If an existing pool entry already matches the concatenated string, that entry is used.
 * 4. Finally, it rewinds the program counter to `pc` (the location of the original
 *    first `OP_STRING`) and generates a single `OP_STRING` instruction to load the
 *    merged/reused literal into the destination register `dst`.
 *
 * @param s The current code generation scope.
 * @param dst The destination register for the result of the concatenation.
 * @param b1 The pool index of the first string literal.
 * @param b2 The pool index of the second string literal.
 * @param pc The program counter of the instruction that loaded the first string literal (`b1`).
 *           This is where the new merged `OP_STRING` will be generated.
 */
static void
merge_op_string(codegen_scope *s, uint16_t dst, uint16_t b1, uint16_t b2, const mrb_code *pc)
{
  int used = 0;
  const mrb_code *i = s->iseq;

  /* scan OP_STRING that refers b1 or b2 */
  mrb_assert(pc < s->iseq + s->icapa);
  while (i<pc) {
    struct mrb_insn_data data = mrb_decode_insn(i);
    if (data.insn == OP_STRING) {
      if (data.b == b1) used |= 1;
      if (data.b == b2) used |= 2;
    }
    switch (i[0]) {
    case OP_EXT1:
      i += mrb_insn_size1[i[1]] + 1;
      break;
    case OP_EXT2:
      i += mrb_insn_size2[i[1]] + 1;
      break;
    case OP_EXT3:
      i += mrb_insn_size3[i[1]] + 1;
      break;
    default:
      i += mrb_insn_size[i[0]];
      break;
    }
  }

  mrb_irep_pool *p1 = &s->pool[b1];
  mrb_irep_pool *p2 = &s->pool[b2];
  mrb_int len1 = p1->tt>>2;
  mrb_int len2 = p2->tt>>2;
  int off = find_pool_str(s, p1->u.str, len1, p2->u.str, len2);

  if (off < 0) {
    switch (used) {
    case 0:                       /* both pools are free */
    case 2:                       /* b2 is referenced */
      /* overwrite p1; free b2 if possible */
      off = b1;
      realloc_pool_str(s, p1, len1+len2);
      memcpy((void*)(p1->u.str+len1), (void*)p2->u.str, len2);
      if (b1 != b2 && used == 0 && b2+1 == s->irep->plen) {
        free_pool_str(s, p2);
      }
      break;
    case 1:                       /* b1 is referenced */
      /* overwrite p2 */
      off = b2;
      realloc_pool_str(s, p2, len1+len2);
      memmove((void*)(p2->u.str+len1), (void*)p2->u.str, len2);
      memcpy((void*)p2->u.str, p1->u.str, len1);
      break;
    case 3:                       /* both b1&b2 are referenced */
      /* create new pool */
      off = new_lit_str2(s, p1->u.str, len1, p2->u.str, len2);
      break;
    }
  }
  s->pc = addr_pc(s, pc);
  genop_2(s, OP_STRING, dst, off);
}

/*
 * Generates code for addition (`OP_ADD`) or subtraction (`OP_SUB`)
 * operations, storing the result in register `dst`.
 *
 * This function includes several peephole optimizations:
 * 1. String Concatenation: If `op` is `OP_ADD` and the two preceding instructions
 *    were `OP_STRING` (loading string literals), it calls `merge_op_string`
 *    to attempt compile-time concatenation of these literals.
 * 2. Immediate Operations: If the last instruction loaded an integer literal (`n`)
 *    and the instruction before that loaded another integer (`n0`), but `n0` is not
 *    suitable for further folding (e.g., it's at a label, or the instruction
 *    before it isn't an integer load), it attempts to convert the operation to
 *    `OP_ADDI` or `OP_SUBI` if `n` fits within an 8-bit signed integer.
 * 3. Constant Folding: If both the last two instructions loaded integer literals
 *    (`n0` and `n`), it performs the addition or subtraction at compile time.
 *    The program counter is rewound to the location of the first literal load,
 *    and code is generated to load the folded result directly using `gen_int`.
 *
 * If no optimizations are applicable, it generates the standard `OP_ADD` or `OP_SUB`
 * instruction.
 *
 * @param s The current code generation scope.
 * @param op The operation code, either `OP_ADD` or `OP_SUB`.
 * @param dst The destination register index for the result.
 */
static void
gen_addsub(codegen_scope *s, uint8_t op, uint16_t dst)
{
  if (no_peephole(s)) {
  normal:
    genop_1(s, op, dst);
    return;
  }
  else {
    struct mrb_insn_data data = mrb_last_insn(s);
    mrb_int n;

    if (!get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      if (op == OP_ADD && data.insn == OP_STRING) {
        struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
        if (data0.insn == OP_STRING) {
          merge_op_string(s, dst, data0.b, data.b, data0.addr);
          return;
        }
      }
      goto normal;
    }
    struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
    mrb_int n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data0, &n0)) {
      /* OP_ADDI/OP_SUBI takes upto 8bits */
      if (n > UINT8_MAX || n < -UINT8_MAX) goto normal;
      rewind_pc(s);
      if (n == 0) return;
      if (n > 0) {
        if (op == OP_ADD) genop_2(s, OP_ADDI, dst, (uint16_t)n);
        else genop_2(s, OP_SUBI, dst, (uint16_t)n);
      }
      else {                    /* n < 0 */
        n = -n;
        if (op == OP_ADD) genop_2(s, OP_SUBI, dst, (uint16_t)n);
        else genop_2(s, OP_ADDI, dst, (uint16_t)n);
      }
      return;
    }
    if (op == OP_ADD) {
      if (mrb_int_add_overflow(n0, n, &n)) goto normal;
    }
    else { /* OP_SUB */
      if (mrb_int_sub_overflow(n0, n, &n)) goto normal;
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
  }
}

/*
 * Generates code for multiplication (`OP_MUL`) or division (`OP_DIV`)
 * operations, storing the result in register `dst`.
 *
 * Peephole Optimization (Constant Folding):
 * - If peephole optimization is enabled and the two immediately preceding
 *   instructions loaded integer literals (into registers that are operands
 *   for this multiplication/division), this function performs the operation
 *   at compile time.
 * - The program counter is rewound to the location of the first literal load,
 *   and code is generated to load the folded result directly using `gen_int`.
 * - For division, if the divisor is zero or if it's `MRB_INT_MIN / -1` (which
 *   would overflow), the optimization is skipped.
 *
 * If no optimization is applicable, it generates the standard `OP_MUL` or `OP_DIV`
 * instruction.
 *
 * @param s The current code generation scope.
 * @param op The operation code, either `OP_MUL` or `OP_DIV`.
 * @param dst The destination register index for the result.
 */
static void
gen_muldiv(codegen_scope *s, uint8_t op, uint16_t dst)
{
  if (no_peephole(s)) {
  normal:
    genop_1(s, op, dst);
    return;
  }
  else {
    struct mrb_insn_data data = mrb_last_insn(s);
    mrb_int n, n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      goto normal;
    }
    struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
    if (!get_int_operand(s, &data0, &n0)) {
      goto normal;
    }
    if (op == OP_MUL) {
      if (mrb_int_mul_overflow(n0, n, &n)) goto normal;
    }
    else { /* OP_DIV */
      if (n == 0) goto normal;
      if (n0 == MRB_INT_MIN && n == -1) goto normal;
      n = mrb_div_int(n0, n);
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
  }
}

mrb_bool mrb_num_shift(mrb_state *mrb, mrb_int val, mrb_int width, mrb_int *num);

/*
 * Generates code for various binary operations, identified by `sym_op`,
 * storing the result in register `dst`.
 *
 * This function handles specific binary operations and includes peephole
 * optimizations for constant folding when operands are integer literals.
 *
 * Operations Handled & Optimizations:
 * - `aref` (`[]`): Generates `OP_GETIDX`.
 * - Bitwise shifts (`<<`, `>>`): If both operands are integer literals,
 *   performs the shift at compile time using `mrb_num_shift` and loads the result.
 * - Modulo (`%`): If both operands are integer literals, performs modulo
 *   at compile time and loads the result. Handles `MRB_INT_MIN % -1`.
 * - Bitwise AND (`&`), OR (`|`), XOR (`^`): If both operands are integer
 *   literals, performs the operation at compile time and loads the result.
 *
 * If an optimization is applied (e.g., constant folding), the program counter
 * is rewound, and `gen_int` is used to load the computed result.
 *
 * @param s The current code generation scope.
 * @param op The `mrb_sym` representing the binary operator (e.g., `MRB_OPSYM_LSHIFT`).
 * @param dst The destination register index for the result.
 * @return `TRUE` if a specific optimization was applied (like `OP_GETIDX` or constant folding),
 *         `FALSE` otherwise. A `FALSE` return typically indicates that a generic
 *         `OP_SEND` instruction should be generated for the operation.
 */
static mrb_bool
gen_binop(codegen_scope *s, mrb_sym op, uint16_t dst)
{
  if (no_peephole(s)) return FALSE;
  else if (op == MRB_OPSYM(aref)) {
    /* GETIDX0 fusion: MOVE dst arr; LOADI_0 dst+1 -> GETIDX0 dst arr */
    struct mrb_insn_data data = mrb_last_insn(s);
    if (data.insn == OP_LOADI_0 && data.a == (uint32_t)dst+1 && addr_pc(s, data.addr) != s->lastlabel) {
      struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
      if (data0.insn == OP_MOVE && data0.a == dst && data0.b != dst) {
        s->pc = addr_pc(s, data0.addr);
        genop_2(s, OP_GETIDX0, dst, data0.b);
        return TRUE;
      }
    }
    genop_1(s, OP_GETIDX, dst);
    return TRUE;
  }
  else {
    struct mrb_insn_data data = mrb_last_insn(s);
    mrb_int n, n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      return FALSE;
    }
    struct mrb_insn_data data0 = mrb_decode_insn(mrb_prev_pc(s, data.addr));
    if (!get_int_operand(s, &data0, &n0)) {
      return FALSE;
    }
    if (op == MRB_OPSYM(lshift)) {
      if (!mrb_num_shift(s->mrb, n0, n, &n)) return FALSE;
    }
    else if (op == MRB_OPSYM(rshift)) {
      if (n == MRB_INT_MIN) return FALSE;
      if (!mrb_num_shift(s->mrb, n0, -n, &n)) return FALSE;
    }
    else if (op == MRB_OPSYM(mod) && n != 0) {
      if (n0 == MRB_INT_MIN && n == -1) {
        n = 0;
      }
      else {
        mrb_int n1 = n0 % n;
        if ((n0 < 0) != (n < 0) && n1 != 0) {
          n1 += n;
        }
        n = n1;
      }
    }
    else if (op == MRB_OPSYM(and)) {
      n = n0 & n;
    }
    else if (op == MRB_OPSYM(or)) {
      n = n0 | n;
    }
    else if (op == MRB_OPSYM(xor)) {
      n = n0 ^ n;
    }
    else {
      return FALSE;
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
    return TRUE;
  }
}

/*
 * Resolves the target address of a previously generated jump instruction.
 *
 * Jump instructions are often generated with placeholder offsets (e.g., 0 or a
 * link to another jump) when their final target is not yet known. This function
 * patches such a jump.
 *
 * `pos0` is the program counter (address) of the 2-byte field within a jump
 * instruction that holds its offset (or a link in a jump chain).
 *
 * The function calculates the correct relative offset from the instruction
 * *after* the jump's offset field (`pos0 + 2`) to the current program
 * counter (`s->pc`), which is the actual target of the jump. This calculated
 * offset is then written back into the bytecode at `pos0`.
 *
 * If the original value at `pos0` was not 0 (i.e., it was part of a jump chain,
 * pointing to the next jump to patch), this original value (which is an offset
 * relative to `pos0 + 2`) is returned so that `dispatch_linked` can continue
 * patching the chain. If the original value was 0, it signifies the end of a chain,
 * and 0 is returned.
 *
 * @param s The current code generation scope.
 * @param pos0 The address of the 2-byte offset field within a jump instruction.
 * @return The next position in a jump chain to dispatch (calculated from the
 *         original offset stored at `pos0`), or 0 if it's the end of a chain.
 */
static uint32_t
dispatch(codegen_scope *s, uint32_t pos0)
{
  int32_t pos1;
  int32_t offset;
  int16_t newpos;

  if (pos0 == JMPLINK_START) return 0;

  pos1 = pos0 + 2;
  offset = s->pc - pos1;
  if (offset > INT16_MAX) {
    codegen_error(s, "too big jmp offset");
  }
  s->lastlabel = s->pc;
  newpos = (int16_t)PEEK_S(s->iseq+pos0);
  emit_S(s, pos0, (uint16_t)offset);
  if (newpos == 0) return 0;
  return pos1+newpos;
}

/*
 * Patches a chain of linked jump instructions to all point to the current
 * program counter (`s->pc`).
 *
 * Jump instructions whose targets are not yet known can be linked together.
 * Each jump's offset field initially stores the relative offset to the next
 * jump in the chain (or 0 if it's the last one). `pos` is the address of the
 * first jump's offset field in such a chain.
 *
 * This function iterates through the chain:
 * - It calls `dispatch(s, pos)` to patch the jump at `pos` to target the current `s->pc`.
 * - `dispatch` returns the address of the next jump in the chain (or 0 if the end).
 * - The process repeats until the end of the chain is reached.
 *
 * If `pos` is `JMPLINK_START`, it means there's no chain to dispatch, so it returns early.
 *
 * @param s The current code generation scope.
 * @param pos The address of the offset field of the first jump instruction in a linked chain.
 */
static void
dispatch_linked(codegen_scope *s, uint32_t pos)
{
  if (pos==JMPLINK_START) return;
  for (;;) {
    pos = dispatch(s, pos);
    if (pos==0) break;
  }
}

/* Updates the nregs (number of registers used) if the current stack pointer (sp) exceeds it. */
#define nregs_update do {if (s->sp > s->nregs) s->nregs = s->sp;} while (0)
static void
push_n_(codegen_scope *s, int n)
{
  if (s->sp+n >= 0xffff) {
    codegen_error(s, "too complex expression");
  }
  s->sp+=n;
  nregs_update;
}

static void
pop_n_(codegen_scope *s, int n)
{
  if ((int)s->sp-n < 0) {
    codegen_error(s, "stack pointer underflow");
  }
  s->sp-=n;
}

/* Increments the stack pointer (sp) by 1 and updates nregs. */
#define push() push_n_(s,1)
/* Increments the stack pointer (sp) by n and updates nregs. */
#define push_n(n) push_n_(s,n)
/* Decrements the stack pointer (sp) by 1. */
#define pop() pop_n_(s,1)
/* Decrements the stack pointer (sp) by n. */
#define pop_n(n) pop_n_(s,n)
/* Returns the current stack pointer (sp) value. */
#define cursp() (s->sp)

/*
 * Extends the literal pool (`s->pool`) of the current IREP (`s->irep`) if necessary.
 *
 * If the number of literals currently in the pool (`s->irep->plen`) has reached
 * the pool's capacity (`s->pcapa`), this function doubles the capacity by
 * reallocating the `s->pool` array.
 * After ensuring there's space, it increments `s->irep->plen` and returns a pointer
 * to the newly available slot in the literal pool.
 *
 * @param s The current code generation scope.
 * @return A pointer to the next available (or newly allocated) `mrb_irep_pool` entry.
 */
static mrb_irep_pool*
lit_pool_extend(codegen_scope *s)
{
  if (s->irep->plen == s->pcapa) {
    s->pcapa *= 2;
    s->pool = (mrb_irep_pool*)mrbc_realloc(s->pool, sizeof(mrb_irep_pool)*s->pcapa);
  }

  return &s->pool[s->irep->plen++];
}

/* Helper functions for simple load operations that follow the pattern:
 * if (!val) return; <prepare>; genop_X(...); push(); */
static void
gen_load_op1(codegen_scope *s, mrb_code op, int val)
{
  if (!val) return;
  genop_1(s, op, cursp());
  push();
}

static void
gen_load_op2(codegen_scope *s, mrb_code op, uint16_t arg, int val)
{
  if (!val) return;
  genop_2(s, op, cursp(), arg);
  push();
}

/* Helper function for conditional nil loading - loads nil only if val is needed */
static void
gen_load_nil(codegen_scope *s, int val)
{
  if (!val) return;
  genop_1(s, OP_LOADNIL, cursp());
  push();
}

/* Helper function for loading literal and pushing */
static void
gen_load_lit(codegen_scope *s, int off)
{
  genop_2(s, OP_LOADL, cursp(), off);
  push();
}

/*
 * Adds a big integer literal (BigInt) to the IREP's literal pool.
 * The BigInt is provided as a string `p` in the given `base`.
 *
 * - It first searches the existing literal pool to see if an identical BigInt
 *   (same string representation and base) already exists. If so, its index is returned.
 * - If not found, a new entry is created in the pool:
 *   - The pool is extended if necessary using `lit_pool_extend`.
 *   - The new pool entry's type `tt` is set to `IREP_TT_BIGINT`.
 *   - Memory is allocated to store the BigInt's string representation, its length (1 byte),
 *     and its base (1 byte). The string `p` is copied into this buffer.
 *   - `pv->u.str` points to this allocated buffer.
 * - If the length of the string `p` exceeds 255, a "integer too big" error is raised.
 *
 * @param s The current code generation scope.
 * @param p A string representing the big integer.
 * @param base The base of the string representation (e.g., 10 for decimal).
 * @return The index of the BigInt literal in the pool.
 */
static int
new_litbint(codegen_scope *s, const char *p, int base)
{
  int i;
  size_t plen;
  mrb_irep_pool *pv;

  plen = strlen(p);
  if (plen > 255) {
    codegen_error(s, "integer too big");
  }
  for (i=0; i<s->irep->plen; i++) {
    size_t len;
    pv = &s->pool[i];
    if (pv->tt != IREP_TT_BIGINT) continue;
    len = pv->u.str[0];
    if (len == plen && pv->u.str[1] == base && memcmp(pv->u.str+2, p, len) == 0)
      return i;
  }

  pv = lit_pool_extend(s);

  char *buf;
  pv->tt = IREP_TT_BIGINT;
  buf = (char*)mrbc_malloc(plen+3);
  buf[0] = (char)plen;
  buf[1] = base;
  memcpy(buf+2, p, plen);
  buf[plen+2] = '\0';
  pv->u.str = buf;

  return i;
}

/*
 * Searches the IREP's literal pool for an existing string that is identical
 * to the concatenation of `str1` (of length `len1`) and `str2` (of length `len2`).
 *
 * It iterates through the existing literal pool entries:
 * - Skips entries that are not strings or are marked with `IREP_TT_NFLAG`.
 * - Compares the total length (`len1 + len2`) with the length of the pool string.
 * - If lengths match, it performs a `memcmp` to check if the content is identical
 *   to the concatenation of `str1` and `str2`.
 *
 * @param s The current code generation scope.
 * @param str1 Pointer to the first part of the string to find.
 * @param len1 Length of `str1`.
 * @param str2 Pointer to the second part of the string to find (can be NULL if `len2` is 0).
 * @param len2 Length of `str2`.
 * @return The index of the matching string literal in the pool if found, otherwise -1.
 */
static int
find_pool_str(codegen_scope *s, const char *str1, mrb_int len1, const char *str2, mrb_int len2)
{
  mrb_irep_pool *pool;
  mrb_int len = len1 + len2;
  int i;

  for (i=0; i<s->irep->plen; i++) {
    pool = &s->pool[i];
    if (pool->tt & IREP_TT_NFLAG) continue;
    mrb_int plen = pool->tt>>2;
    if (len != plen) continue;
    if (memcmp(pool->u.str, str1, len1) == 0 &&
        (len2 == 0 || memcmp(pool->u.str + len1, str2, len2) == 0))
      return i;
  }
  return -1;
}

/*
 * Adds a string literal, potentially formed by concatenating `str1` and `str2`,
 * to the IREP's literal pool.
 *
 * - It first calls `find_pool_str` to check if an identical concatenated string
 *   already exists in the pool. If so, its index is returned.
 * - If not found:
 *   - A new slot in the literal pool is obtained using `lit_pool_extend`.
 *   - If `str1` points to read-only data (`mrb_ro_data_p(str1)`) and `str2` is NULL
 *     (meaning `str1` is the complete string and it's from a static source),
 *     the pool entry is marked as `IREP_TT_SSTR` (shared string) and `pool->u.str`
 *     points directly to `str1`.
 *   - Otherwise (if the string needs to be dynamically created or is not from
 *     read-only data), memory is allocated for the combined length of `str1` and
 *     `str2` plus a null terminator. `str1` and `str2` (if present) are copied
 *     into this new buffer. The pool entry is marked as `IREP_TT_STR`, and
 *     `pool->u.str` points to this newly allocated buffer.
 * - The index of the new or found literal is returned.
 *
 * @param s The current code generation scope.
 * @param str1 Pointer to the first part of the string.
 * @param len1 Length of `str1`.
 * @param str2 Pointer to the second part of the string (can be NULL if `len2` is 0).
 * @param len2 Length of `str2`.
 * @return The index of the string literal in the pool.
 */
static int
new_lit_str2(codegen_scope *s, const char *str1, mrb_int len1, const char *str2, mrb_int len2)
{
  int i = find_pool_str(s, str1, len1, str2, len2);

  if (i >= 0) return i;
  i = s->irep->plen;

  mrb_irep_pool *pool = lit_pool_extend(s);
  mrb_int len = len1 + len2;

  if (mrb_ro_data_p(str1) && !str2) {
    pool->tt = (uint32_t)(len<<2) | IREP_TT_SSTR;
    pool->u.str = str1;
  }
  else {
    char *p;
    pool->tt = (uint32_t)(len<<2) | IREP_TT_STR;
    p = (char*)mrbc_malloc(len+1);
    memcpy(p, str1, len1);
    if (str2) memcpy(p+len1, str2, len2);
    p[len] = '\0';
    pool->u.str = p;
  }

  return i;
}

/*
 * Adds a string literal (from `str` with length `len`) to the IREP's literal pool.
 * This is a wrapper around `new_lit_str2`, passing NULL for `str2` and 0 for `len2`.
 *
 * @param s The current code generation scope.
 * @param str Pointer to the string.
 * @param len Length of the string.
 * @return The index of the string literal in the pool.
 */
static int
new_lit_str(codegen_scope *s, const char *str, mrb_int len)
{
  return new_lit_str2(s, str, len, NULL, 0);
}

/*
 * Adds a C-string literal (null-terminated string `str`) to the IREP's literal pool.
 * This is a wrapper around `new_lit_str`, calculating the length of `str` using `strlen`.
 *
 * @param s The current code generation scope.
 * @param str Pointer to the null-terminated C-string.
 * @return The index of the string literal in the pool.
 */
static int
new_lit_cstr(codegen_scope *s, const char *str)
{
  return new_lit_str(s, str, (mrb_int)strlen(str));
}

/*
 * Adds an integer literal `num` to the IREP's literal pool.
 *
 * - It first searches the existing literal pool to see if an identical integer
 *   value already exists. If so, its index is returned.
 * - If not found, a new entry is created:
 *   - The pool is extended if necessary using `lit_pool_extend`.
 *   - The new pool entry's type `tt` is set to `IREP_TT_INT32` or `IREP_TT_INT64`
 *     depending on whether `MRB_INT64` is defined.
 *   - The integer `num` is stored in `pool->u.i32` or `pool->u.i64`.
 *
 * @param s The current code generation scope.
 * @param num The `mrb_int` value to add to the pool.
 * @return The index of the integer literal in the pool.
 */
static int
new_lit_int(codegen_scope *s, mrb_int num)
{
  int i;
  mrb_irep_pool *pool;

  for (i=0; i<s->irep->plen; i++) {
    pool = &s->pool[i];
    if (pool->tt == IREP_TT_INT32) {
      if (num == pool->u.i32) return i;
    }
#ifdef MRB_64BIT
    else if (pool->tt == IREP_TT_INT64) {
      if (num == pool->u.i64) return i;
    }
    continue;
#endif
  }

  pool = lit_pool_extend(s);

#ifdef MRB_INT64
  pool->tt = IREP_TT_INT64;
  pool->u.i64 = num;
#else
  pool->tt = IREP_TT_INT32;
  pool->u.i32 = num;
#endif

  return i;
}

#ifndef MRB_NO_FLOAT
/*
 * Adds a float literal `num` to the IREP's literal pool.
 * This function is only compiled if `MRB_NO_FLOAT` is not defined.
 *
 * - It first searches the existing literal pool to see if an identical float
 *   value (considering both value and sign bit) already exists. If so, its
 *   index is returned.
 * - If not found, a new entry is created:
 *   - The pool is extended if necessary using `lit_pool_extend`.
 *   - The new pool entry's type `tt` is set to `IREP_TT_FLOAT`.
 *   - The float `num` is stored in `pool->u.f`.
 *
 * @param s The current code generation scope.
 * @param num The `mrb_float` value to add to the pool.
 * @return The index of the float literal in the pool.
 */
static int
new_lit_float(codegen_scope *s, mrb_float num)
{
  int i;
  mrb_irep_pool *pool;

  for (i=0; i<s->irep->plen; i++) {
    mrb_float f;
    pool = &s->pool[i];
    if (pool->tt != IREP_TT_FLOAT) continue;
    f = pool->u.f;
    if (f == num && !signbit(f) == !signbit(num)) return i;
  }

  pool = lit_pool_extend(s);

  pool->tt = IREP_TT_FLOAT;
  pool->u.f = num;

  return i;
}
#endif

/*
 * Adds a symbol `sym` to the IREP's symbol list (`s->syms`).
 *
 * - It first iterates through the existing symbols in `s->syms` (up to `s->irep->slen`)
 *   to check if the symbol `sym` already exists. If found, its index is returned.
 * - If the symbol is not found:
 *   - It checks if the current symbol list capacity (`s->scapa`) is sufficient.
 *     If not, `s->scapa` is doubled, and `s->syms` is reallocated.
 *     If the new capacity would exceed 0xFFFF, a "too many symbols" error is raised.
 *   - The symbol `sym` is added to `s->syms` at the current end of the list (`s->irep->slen`).
 *   - `s->irep->slen` is incremented.
 * - The index of the (newly added or existing) symbol is returned.
 *
 * @param s The current code generation scope.
 * @param sym The `mrb_sym` to add to the symbol list.
 * @return The index of the symbol in the IREP's symbol list.
 */
static int
sym_idx(codegen_scope *s, mrb_sym sym)
{
  int i, len;

  mrb_assert(s->irep);

  len = s->irep->slen;
  for (i=0; i<len; i++) {
    if (s->syms[i] == sym) return i;
  }
  if (s->irep->slen >= s->scapa) {
    s->scapa *= 2;
    if (s->scapa > 0xffff) {
      codegen_error(s, "too many symbols");
    }
    s->syms = (mrb_sym*)mrbc_realloc(s->syms, sizeof(mrb_sym)*s->scapa);
  }
  s->syms[s->irep->slen] = sym;
  return s->irep->slen++;
}

/*
 * Generates an instruction to set a variable, where the variable is identified by a symbol.
 * This is a generic helper for opcodes like `OP_SETGV`, `OP_SETIV`, `OP_SETCV`, `OP_SETCONST`.
 *
 * - It first ensures the symbol `sym` is in the IREP's symbol list by calling `sym_idx`,
 *   obtaining its index `idx`.
 * - Peephole Optimization: If `val` is `NOVAL` (false) and peephole optimization is enabled,
 *   it checks if the immediately preceding instruction was an `OP_MOVE` into the `dst`
 *   register. If so, it means the value intended for the variable assignment was moved
 *   into `dst`. In this case, it rewinds the program counter and uses the original source
 *   register of that `OP_MOVE` as the source for the set operation, effectively using
 *   the value before it was moved to `dst`.
 * - Finally, it generates the specified opcode `op` with operands `dst` (the source
 *   register for the value, possibly modified by peephole optimization) and `idx`
 *   (the symbol index) using `genop_2`.
 *
 * @param s The current code generation scope.
 * @param op The specific set variable opcode (e.g., `OP_SETGV`, `OP_SETIV`).
 * @param dst The register index holding the value to be assigned to the variable.
 * @param sym The `mrb_sym` (symbol) identifying the variable.
 * @param val A flag indicating context (often whether the value in `dst` is from an
 *            expression that should be preserved if the set operation is part of a larger one).
 *            If `NOVAL`, it enables the peephole optimization.
 */
static void
gen_setxv(codegen_scope *s, uint8_t op, uint16_t dst, mrb_sym sym, int val)
{
  int idx = sym_idx(s, sym);
  if (!val && !no_peephole(s)) {
    struct mrb_insn_data data = mrb_last_insn(s);
    if (data.insn == OP_MOVE && data.a == dst) {
      dst = data.b;
      rewind_pc(s);
    }
  }
  genop_2(s, op, dst, idx);
}

/*
 * Generates the most compact instruction(s) to load an integer literal `i`
 * into the destination register `dst`.
 *
 * It employs a series of checks to use specialized, shorter opcodes for common integer values:
 * - `OP_LOADI__1` for -1.
 * - `OP_LOADINEG` for negative integers between -255 and -2 (operand is positive magnitude).
 * - `OP_LOADI16` for negative integers fitting in a signed 16-bit integer (INT16_MIN to -256).
 * - `OP_LOADI32` for negative integers fitting in a signed 32-bit integer (INT32_MIN to not fitting in 16-bit).
 * - `OP_LOADI_0` through `OP_LOADI_7` for integers 0 through 7.
 * - `OP_LOADI8` for positive integers between 8 and 255.
 * - `OP_LOADI16` for positive integers fitting in a signed 16-bit integer (256 to INT16_MAX).
 * - `OP_LOADI32` for positive integers fitting in a signed 32-bit integer (not fitting in 16-bit to INT32_MAX).
 *
 * If the integer `i` does not fit any of these specialized opcodes (i.e., it's too large
 * or too small for `OP_LOADI32`), it falls back to `OP_LOADL`. This involves adding
 * the integer to the IREP's literal pool using `new_lit_int` and then generating
 * `OP_LOADL` with the resulting pool index.
 *
 * @param s The current code generation scope.
 * @param dst The destination register index where the integer will be loaded.
 * @param i The `mrb_int` value to load.
 */
static void
gen_int(codegen_scope *s, uint16_t dst, mrb_int i)
{
  if (i < 0) {
    if (i == -1) genop_1(s, OP_LOADI__1, dst);
    else if (i >= -0xff) genop_2(s, OP_LOADINEG, dst, (uint16_t)-i);
    else if (i >= INT16_MIN) genop_2S(s, OP_LOADI16, dst, (uint16_t)i);
    else if (i >= INT32_MIN) genop_2SS(s, OP_LOADI32, dst, (uint32_t)i);
    else goto int_lit;
  }
  else if (i < 8) genop_1(s, OP_LOADI_0 + (uint8_t)i, dst);
  else if (i <= 0xff) genop_2(s, OP_LOADI8, dst, (uint16_t)i);
  else if (i <= INT16_MAX) genop_2S(s, OP_LOADI16, dst, (uint16_t)i);
  else if (i <= INT32_MAX) genop_2SS(s, OP_LOADI32, dst, (uint32_t)i);
  else {
  int_lit:
    genop_2(s, OP_LOADL, dst, new_lit_int(s, i));
  }
}

/*
 * Generates code for a unary operation specified by `sym`, operating on the
 * value in register `dst`, and storing the result back into `dst`.
 *
 * Supported unary operations:
 * - Unary plus (`+`): This is a no-op in terms of value change, but the function
 *   still processes it.
 * - Unary minus (`-`): Negates the integer value.
 * - Bitwise NOT (`~`): Performs a bitwise complement on the integer value.
 *
 * Peephole Optimization (Constant Folding):
 * - If peephole optimization is enabled and the immediately preceding instruction
 *   loaded an integer literal into register `dst` (which is also the operand
 *   register for this unary operation), this function performs the unary operation
 *   at compile time.
 * - The program counter is rewound to the location of the literal load, and code
 *   is generated to load the folded result directly using `gen_int`.
 * - For unary minus, if the original integer is `MRB_INT_MIN`, constant folding
 *   is skipped to avoid overflow.
 *
 * If the operation is not one of the recognized unary ops or if constant folding
 * is not applicable, the function returns `FALSE`.
 *
 * @param s The current code generation scope.
 * @param sym The `mrb_sym` representing the unary operator (e.g., `MRB_OPSYM_PLUS`, `MRB_OPSYM_MINUS`).
 * @param dst The register index which holds the operand and will store the result.
 * @return `TRUE` if a constant folding optimization was successfully applied,
 *         `FALSE` otherwise (e.g., if the operation is not supported for folding,
 *         or if the preceding instruction was not a suitable integer load).
 */
static mrb_bool
gen_uniop(codegen_scope *s, mrb_sym sym, uint16_t dst)
{
  if (no_peephole(s)) return FALSE;
  struct mrb_insn_data data = mrb_last_insn(s);
  mrb_int n;

  if (!get_int_operand(s, &data, &n)) return FALSE;
  if (sym == MRB_OPSYM(plus)) {
    /* unary plus does nothing */
  }
  else if (sym == MRB_OPSYM(minus)) {
    if (n == MRB_INT_MIN) return FALSE;
    n = -n;
  }
  else if (sym == MRB_OPSYM(neg)) {
    n = ~n;
  }
  else {
    return FALSE;
  }
  s->pc = addr_pc(s, data.addr);
  gen_int(s, dst, n);
  return TRUE;
}

/*
 * Calculates and returns the number of elements in a linked list of AST nodes.
 * The list is traversed via the `cdr` field of each `node`.
 *
 * @param tree Pointer to the head of the AST node list.
 * @return The number of nodes in the list.
 */
static int
node_len(node *tree)
{
  int n = 0;

  /* Validate pointer before using it */
  if (!tree || ((uintptr_t)tree < 0x1000)) {
    return 0;
  }

  while (tree) {
    n++;
    tree = tree->cdr;
  }
  return n;
}

/* Casts a void* (typically from an AST node part) to an int. */
#define node_to_sym(x) ((mrb_sym)(intptr_t)(x))
#define node_to_int(x) ((int)(intptr_t)(x))
/* Casts a void* (typically from an AST node part) to a char. */
#define node_to_char(x) ((char)(intptr_t)(x))
/* Casts a void* (typically from an AST node part) to an mrb_sym. */

/* Extracts the symbol (name) of a local variable from its AST node representation. */
#define lv_name(lv) node_to_sym((lv)->car)

/*
 * Searches for a local variable `id` within the current scope's local variable list (`s->lv`).
 * The local variable list `s->lv` is a linked list of AST nodes, where each node's
 * `car` holds the symbol of the local variable.
 *
 * @param s The current code generation scope.
 * @param id The `mrb_sym` (symbol) of the local variable to search for.
 * @return The 1-based index of the local variable in the current scope if found;
 *         otherwise, returns 0.
 */
static int
lv_idx(codegen_scope *s, mrb_sym id)
{
  node *lv = s->lv;
  int n = 1;

  while (lv) {
    if (lv_name(lv) == id) return n;
    n++;
    lv = lv->cdr;
  }
  return 0;
}

static int
search_upvar(codegen_scope *s, mrb_sym id, int *idx)
{
  const struct RProc *u;
  int lv = 0;
  codegen_scope *up = s->prev;

  while (up) {
    *idx = lv_idx(up, id);
    if (*idx > 0) {
      return lv;
    }
    lv++;
    up = up->prev;
  }

  if (lv < 1) lv = 1;
  u = s->parser->upper;
  while (u && !MRB_PROC_CFUNC_P(u)) {
    const struct mrb_irep *ir = u->body.irep;
    uint_fast16_t n = ir->nlocals;
    int i;

    const mrb_sym *v = ir->lv;
    if (v) {
      for (i=1; n > 1; n--, v++, i++) {
        if (*v == id) {
          *idx = i;
          return lv - 1;
        }
      }
    }
    if (MRB_PROC_SCOPE_P(u)) break;
    u = u->upper;
    lv++;
  }

  if (id == MRB_OPSYM(and)) {
    codegen_error(s, "No anonymous block parameter");
  }
  else if (id == MRB_OPSYM(mul)) {
    codegen_error(s, "No anonymous rest parameter");
  }
  else if (id == MRB_OPSYM(pow)) {
    codegen_error(s, "No anonymous keyword rest parameter");
  }
  else {
    codegen_error(s, "Can't find local variables");
  }
  return -1; /* not reached */
}

/*
 * Generates the bytecode for the body of a lambda or a block.
 * This function is responsible for creating a new scope, handling arguments
 * (including optional, rest, keyword, and block arguments), generating code
 * for the body's expressions, and finalizing the resulting `mrb_irep`.
 *
 * @param s The parent code generation scope.
 * @param tree The AST node representing the lambda or block.
 *             `tree->car` contains the argument list AST.
 *             `tree->cdr->car` is the body of the lambda/block.
 * @param blk A flag indicating if this is a block (`TRUE`) or a lambda (`FALSE`).
 *            This affects `s->mscope` and loop setup.
 * @return The index of the newly created `mrb_irep` in the parent scope's `reps` array.
 */
static int
lambda_body(codegen_scope *s, node *locals, struct mrb_ast_args *args, node *body, int blk)
{
  codegen_scope *parent = s;
  /* Create a new scope for the lambda/block body. */
  s = scope_new(s->mrb, s, locals);

  /* `mscope` is false for blocks, true for lambdas/methods. */
  s->mscope = !blk;

  /* If it's a block, push a LOOP_BLOCK structure for break/next/return handling. */
  if (blk) {
    struct loopinfo *lp = loop_push(s, LOOP_BLOCK);
    lp->pc0 = new_label(s); /* Mark entry point for potential retry/redo. */
  }

  /* Argument processing */
  if (args == NULL) { /* No arguments */
    genop_W(s, OP_ENTER, 0); /* Generate OP_ENTER with no argument specification. */
    s->ainfo = 0;
  }
  else { /* Has arguments */
    mrb_aspec a;
    int ma, oa, ra, pa, ka, kd, ba, i;
    uint32_t pos;
    node *opt;
    node *margs, *pargs;

    /* args is already struct mrb_ast_args * */

    /* mandatory arguments */
    ma = node_len(args->mandatory_args);
    margs = args->mandatory_args;

    /* optional arguments */
    oa = node_len(args->optional_args);
    /* rest argument? */
    ra = args->rest_arg ? 1 : 0;
    /* mandatory arguments after rest argument */
    pa = node_len(args->post_mandatory_args);
    pargs = args->post_mandatory_args;

    /* keyword arguments */
    ka = args->keyword_args ? node_len(args->keyword_args) : 0;
    kd = args->kwrest_arg ? 1 : 0;
    /* &nil: no block accepted (noblock flag in aspec) */
    mrb_bool noblock = args->block_arg == MRB_SYM(nil);
    ba = (args->block_arg && !noblock) ? 1 : 0;

    if (ma > 0x1f || oa > 0x1f || pa > 0x1f || ka > 0x1f) {
      codegen_error(s, "too many formal arguments");
    }
    /* (24bits = 1:5:5:1:5:5:1:1) */
    a = (noblock ? MRB_ARGS_NOBLOCK() : 0)
      | MRB_ARGS_REQ(ma)
      | MRB_ARGS_OPT(oa)
      | (ra ? MRB_ARGS_REST() : 0)
      | MRB_ARGS_POST(pa)
      | MRB_ARGS_KEY(ka, kd)
      | (ba ? MRB_ARGS_BLOCK() : 0);
    genop_W(s, OP_ENTER, a);
    /* (13bits = 6:1:5:1:1) - Store argument counts for block argument passing (OP_BLKPUSH) */
    s->ainfo = (((ma+oa) & 0x3f) << 7)
      | ((ra & 0x1) << 6)
      | ((pa & 0x1f) << 1)
      | (ka || kd)
      | ((ba & 0x1) << 13);

    /* Optional argument default value initialization */
    pos = new_label(s); /* Start of the optional argument jump table. */
    for (i=0; i<oa; i++) {
      new_label(s);
      genjmp_0(s, OP_JMP); /* Placeholder jump for each optional arg. */
    }
    if (oa > 0) {
      genjmp_0(s, OP_JMP); /* Jump to skip all default assignments if all optional args are provided. */
    }
    opt = args->optional_args; /* AST node for optional arguments. */
    i = 0;
    while (opt) { /* Iterate through optional arguments. */
      int idx;
      mrb_sym id = node_to_sym(opt->car->car); /* Symbol of the optional argument. */

      dispatch(s, pos+i*3+1); /* Patch the jump to this argument's default value code. */
      codegen(s, opt->car->cdr, VAL); /* Generate code for the default value expression. */
      pop();
      idx = lv_idx(s, id); /* Get local variable index. */
      if (idx > 0) {
        gen_move(s, idx, cursp(), 0); /* Move default value to the local variable. */
      }
      else { /* Should not happen for optional args, but handle as upvar if it does. */
        gen_getupvar(s, cursp(), id);
      }
      i++;
      opt = opt->cdr;
    }
    if (oa > 0) {
      dispatch(s, pos+i*3+1); /* Patch the final jump to after all default assignments. */
    }

    /* Keyword argument processing */
    if (ka > 0 || kd > 0) { /* Has keyword arguments or keyword rest */
      node *kwds;
      int kwrest = kd; /* Flag for keyword rest argument (e.g., **kwargs) */

      kwds = args->keyword_args;

      while (kwds) {
        int jmpif_key_p, jmp_def_set = -1;
        node *kwd = kwds->car;
        mrb_sym kwd_sym = node_to_sym(kwd->car);   /* Direct access to key */
        node *def_arg = kwd->cdr;                  /* Direct access to value */

        if (def_arg) {
          int idx;
          genop_2(s, OP_KEY_P, lv_idx(s, kwd_sym), sym_idx(s, kwd_sym));
          jmpif_key_p = genjmp2_0(s, OP_JMPIF, lv_idx(s, kwd_sym), NOVAL);
          codegen(s, def_arg, VAL);
          pop();
          idx = lv_idx(s, kwd_sym);
          if (idx > 0) {
            gen_move(s, idx, cursp(), 0);
          }
          else {
            gen_getupvar(s, cursp(), kwd_sym);
          }
          jmp_def_set = genjmp_0(s, OP_JMP);
          dispatch(s, jmpif_key_p);
        }
        genop_2(s, OP_KARG, lv_idx(s, kwd_sym), sym_idx(s, kwd_sym));
        if (jmp_def_set != -1) {
          dispatch(s, jmp_def_set);
        }
        i++;

        kwds = kwds->cdr;
      }
      /* Check if there are keyword args but no keyword rest */
      int has_keywords = args->keyword_args != NULL;

      if (has_keywords && !kwrest) { /* If there are keyword args but no keyword rest. */
        genop_0(s, OP_KEYEND); /* Signal end of keyword arguments. */

        /* Reconstruct keyword hash for super to use */
        /* After KEYEND, the hash at kw_pos is empty (all keys deleted by KARG) */
        /* Build a fresh hash from the extracted keyword local variables */
        int kw_dict_pos = ma + oa + ra + pa + 1;
        int sp_save = cursp();

        /* Load key-value pairs for each keyword argument starting at stack position */
        node *kw_list = args->keyword_args;
        int num_pairs = 0;
        while (kw_list) {
          node *kw = kw_list->car;
          mrb_sym kw_sym = node_to_sym(kw->car);

          /* Load symbol (key) */
          genop_2(s, OP_LOADSYM, cursp(), sym_idx(s, kw_sym));
          push();

          /* Load keyword local variable value */
          genop_2(s, OP_MOVE, cursp(), lv_idx(s, kw_sym));
          push();

          num_pairs++;
          kw_list = kw_list->cdr;
        }

        /* Create hash at current stack position, then move to keyword dict position */
        if (num_pairs > 0) {
          genop_2(s, OP_HASH, sp_save, num_pairs);
          genop_2(s, OP_MOVE, kw_dict_pos, sp_save);
        }
        else {
          /* No keyword args, create empty hash */
          genop_2(s, OP_HASH, sp_save, 0);
          genop_2(s, OP_MOVE, kw_dict_pos, sp_save);
        }

        /* Restore stack pointer */
        s->sp = sp_save;
      }
    }

    /* Block argument processing */
    if (ba) { /* If a block argument (e.g., &blk) is present. */
      mrb_sym bparam = args->block_arg;
      pos = ma+oa+ra+pa+(ka||kd); /* Calculate register offset for the block parameter. */
      if (bparam) { /* If it's a named block parameter. */
        int idx = lv_idx(s, bparam);
        genop_2(s, OP_MOVE, idx, pos+1); /* Move the block from its argument slot to the local variable. */
      }
    }

    /* Argument destructuring for mandatory and post-mandatory arguments */
    if (margs) { /* Mandatory arguments */
      node *n = margs;
      pos = 1; /* Start from register 1 (after self). */
      while (n) {
        if (node_type(n->car) == NODE_MARG) { /* If the argument is a mass assignment (e.g., |(a,b)| ). */
          struct mrb_ast_masgn_node *masgn_n = (struct mrb_ast_masgn_node*)n->car;
          /* Use dedicated parameter destructuring logic instead of general codegen_masgn */
          int nn = 0;
          /* Handle pre variables */
          if (masgn_n->pre) {
            node *pre = masgn_n->pre;
            while (pre) {
              int sp = cursp();
              genop_3(s, OP_AREF, sp, pos, nn);
              push();
              gen_assignment(s, pre->car, NULL, sp, NOVAL);
              pop();
              nn++;
              pre = pre->cdr;
            }
          }
          /* For now, only handle simple pre variables - rest/post would need more complex logic */
        }
        pos++;
        n = n->cdr;
      }
    }
    if (pargs) { /* Post-mandatory arguments */
      node *n = pargs;
      pos = ma+oa+ra+1; /* Calculate starting register for post-mandatory args. */
      while (n) {
        if (node_type(n->car) == NODE_MARG) { /* If argument is a mass assignment. */
          struct mrb_ast_masgn_node *masgn_n = (struct mrb_ast_masgn_node*)n->car;
          /* Use dedicated parameter destructuring logic instead of general codegen_masgn */
          int nn = 0;
          /* Handle pre variables */
          if (masgn_n->pre) {
            node *pre = masgn_n->pre;
            while (pre) {
              int sp = cursp();
              genop_3(s, OP_AREF, sp, pos, nn);
              push();
              gen_assignment(s, pre->car, NULL, sp, NOVAL);
              pop();
              nn++;
              pre = pre->cdr;
            }
          }
          /* For now, only handle simple pre variables - rest/post would need more complex logic */
        }
        pos++;
        n = n->cdr;
      }
    }
  }

  /* Generate code for the actual body of the lambda/block. */
  codegen(s, body, VAL);
  pop(); /* Pop the result of the body. */

  /* Implicit return of the last evaluated expression. */
  if (s->pc > 0) { /* Ensure there's some code before adding return. */
    gen_return(s, OP_RETURN, cursp());
  }

  if (blk) {
    loop_pop(s, NOVAL); /* Pop the LOOP_BLOCK structure. */
  }
  scope_finish(s); /* Finalize the IREP for this lambda/block. */
  return parent->irep->rlen - 1; /* Return the index of this IREP in the parent's REP list. */
}

/*
 * Generates code for a new lexical scope, typically for class/module definitions
 * or the top-level script.
 *
 * This function handles the creation of a new `codegen_scope`, recursively
 * generates code for the body of that scope, and then finalizes the scope
 * to produce an `mrb_irep`.
 *
 * @param s The parent code generation scope.
 * @param tree The AST node representing the scope.
 *             `tree->car` contains the list of local variables for the new scope.
 *             `tree->cdr` is the body (sequence of expressions) of the scope.
 * @param val Unused in this specific function's direct logic for return value,
 *            but passed to `codegen` for the body.
 * @return The index of the newly created `mrb_irep` in the parent scope's `reps` array.
 *         Returns 0 if `s->irep` is NULL (should not happen in normal operation).
 */
static int
scope_body(codegen_scope *s, node *locals, node *body, int val)
{
  /* Create a new scope, inheriting from `s`, with local variables from `locals`. */
  codegen_scope *scope = scope_new(s->mrb, s, locals);

  /* Generate code for the body of the scope. */
  codegen(scope, body, val);

  /* If this is the outermost scope (e.g., top-level script), add OP_STOP. */
  if (!s->iseq) { /* s->iseq would be NULL for the initial dummy scope. */
    if (val) {
      gen_return(scope, OP_RETURN, scope->sp-1);
    }
    /* skip RETURN when no_return_value; STOP will terminate VM */
    genop_0(scope, OP_STOP);
  }
  else {
    /* Ensure the scope returns the value of its last expression. */
    if (val) {
      gen_return(scope, OP_RETURN, scope->sp-1);
    }
    else {
      gen_return(scope, OP_RETURN, 0);  /* return nil */
    }
  }

  /* Finalize the IREP for this scope. */
  scope_finish(scope);

  if (!s->irep) {
    /* This case should ideally not be reached in normal compilation. */
    return 0;
  }
  /* Return the index of the newly created IREP in the parent's list of REPs. */
  return s->irep->rlen - 1;
}

static struct mrb_ast_var_header*
get_var_header(node *n)
{
  if (!n) return NULL;

  /* Try to interpret as variable-sized node */
  struct mrb_ast_var_header *header = (struct mrb_ast_var_header*)n;
  return header;
}

/* Helper to detect splat nodes in variable-sized format */
static mrb_bool
is_splat_node(node *n)
{
  return (node_type(n) == NODE_SPLAT);
}

static mrb_bool
nosplat(node *t)
{
  while (t) {
    if (is_splat_node(t->car)) return FALSE;
    t = t->cdr;
  }
  return TRUE;
}

/* Check if node is a simple literal that can be generated into any register */
static mrb_bool
is_simple_literal(node *n)
{
  switch (node_type(n)) {
  case NODE_INT:
  case NODE_NIL:
  case NODE_TRUE:
  case NODE_FALSE:
    return TRUE;
  default:
    return FALSE;
  }
}

/* Check if all lhs are local variables and get their registers */
static mrb_bool
all_lvar_pre(codegen_scope *s, node *pre, int *regs, int max)
{
  int i = 0;
  while (pre && i < max) {
    if (node_type(pre->car) != NODE_LVAR) return FALSE;
    int idx = lv_idx(s, var_node(pre->car)->symbol);
    if (idx <= 0) return FALSE;  /* not a local variable */
    regs[i++] = idx;
    pre = pre->cdr;
  }
  return pre == NULL;  /* all processed */
}

/* Generate a simple literal directly into a specific register */
static void
gen_literal_to_reg(codegen_scope *s, node *n, int reg)
{
  switch (node_type(n)) {
  case NODE_INT:
    gen_int(s, reg, int_node(n)->value);
    break;
  case NODE_NIL:
    genop_1(s, OP_LOADNIL, reg);
    break;
  case NODE_TRUE:
    genop_1(s, OP_LOADTRUE, reg);
    break;
  case NODE_FALSE:
    genop_1(s, OP_LOADFALSE, reg);
    break;
  default:
    break;
  }
}

static mrb_sym
attrsym(codegen_scope *s, mrb_sym a)
{
  mrb_int len;
  const char *name = mrb_sym_name_len(s->mrb, a, &len);
  char *name2 = (char*)codegen_palloc(s,
                                      (size_t)len
                                      + 1 /* '=' */
                                      + 1 /* '\0' */
                                     );
  mrb_assert_int_fit(mrb_int, len, size_t, SIZE_MAX);
  memcpy(name2, name, (size_t)len);
  name2[len] = '=';
  name2[len+1] = '\0';

  return mrb_intern(s->mrb, name2, len+1);
}

/* Maximum number of arguments for a call that can be encoded directly in some opcodes (e.g. OP_SEND). */
#define CALL_MAXARGS 15
/* Maximum number of elements in a literal array/hash handled by simpler opcodes before needing OP_ARYPUSH/OP_HASHADD. */
#define GEN_LIT_ARY_MAX 64
/* Stack pointer threshold during value sequence generation; if cursp() exceeds this, intermediate arrays might be formed. */
#define GEN_VAL_STACK_MAX 99

static int
gen_values(codegen_scope *s, node *t, int val, int limit)
{
  int n = 0;
  int first = 1;
  int slimit = GEN_VAL_STACK_MAX;

  if (limit == 0) limit = GEN_LIT_ARY_MAX;
  if (cursp() >= slimit) slimit = INT16_MAX;

  if (!val) {
    while (t) {
      codegen(s, t->car, NOVAL);
      n++;
      t = t->cdr;
    }
    return n;
  }

  while (t) {
    int is_splat = is_splat_node(t->car);

    /* Optimization: skip or inline literal splat arrays
     * - Empty splat (`*[]`/`*zarray`): contributes nothing; skip.
     * - Non-empty literal array with no inner splat (`*[a,b]`): inline
     *   as normal positional args to avoid building/concatenating arrays.
     */
    if (is_splat) {
      struct mrb_ast_splat_node *splat = splat_node(t->car);
      node *sv = splat->value;
      if (sv) {
        enum node_type nt = node_type(sv);
        if (nt == NODE_ARRAY) {
          struct mrb_ast_array_node *an = array_node(sv);
          if (an->elements == NULL) {
            /* empty splat; contributes nothing */
            t = t->cdr;
            continue;
          }
          else if (nosplat(an->elements)) {
            /* Inline non-empty literal array elements as regular args */
            node *e = an->elements;
            while (e) {
              /* Honor evaluation order */
              codegen(s, e->car, val);
              n++;
              e = e->cdr;
            }
            t = t->cdr;
            continue;
          }
        }
        else if (nt == NODE_ZARRAY) {
          /* explicit empty array literal */
          t = t->cdr;
          continue;
        }
      }
    }

    if (is_splat || cursp() >= slimit) { /* flush stack */
      pop_n(n);
      if (first) {
        if (n == 0) {
          genop_1(s, OP_LOADNIL, cursp());
        }
        else {
          genop_2(s, OP_ARRAY, cursp(), n);
        }
        push();
        first = 0;
        limit = GEN_LIT_ARY_MAX;
      }
      else if (n > 0) {
        pop();
        genop_2(s, OP_ARYPUSH, cursp(), n);
        push();
      }
      n = 0;
    }
    codegen(s, t->car, val);
    if (is_splat) {
      pop(); pop();
      genop_1(s, OP_ARYCAT, cursp());
      push();
    }
    else {
      n++;
    }
    t = t->cdr;
  }
  if (!first) {
    pop();
    if (n > 0) {
      pop_n(n);
      genop_2(s, OP_ARYPUSH, cursp(), n);
    }
    return -1;                  /* variable length */
  }
  else if (n > limit) {
    pop_n(n);
    genop_2(s, OP_ARRAY, cursp(), n);
    return -1;
  }
  return n;
}

static int
gen_hash(codegen_scope *s, node *tree, int val, int limit)
{
  int slimit = GEN_VAL_STACK_MAX;
  if (cursp() >= GEN_LIT_ARY_MAX) slimit = INT16_MAX;
  int len = 0;
  mrb_bool update = FALSE;
  mrb_bool first = TRUE;

  while (tree) {
    if (node_to_sym(tree->car->car) == MRB_OPSYM(pow)) {
      if (val && first) {
        genop_2(s, OP_HASH, cursp(), 0);
        push();
        update = TRUE;
      }
      else if (val && len > 0) {
        pop_n(len*2);
        if (!update) {
          genop_2(s, OP_HASH, cursp(), len);
        }
        else {
          pop();
          genop_2(s, OP_HASHADD, cursp(), len);
        }
        push();
      }
      codegen(s, tree->car->cdr, val);
      if (val && (len > 0 || update)) {
        pop(); pop();
        genop_1(s, OP_HASHCAT, cursp());
        push();
      }
      update = TRUE;
      len = 0;
    }
    else {
      codegen(s, tree->car->car, val);
      codegen(s, tree->car->cdr, val);
      len++;
    }
    tree = tree->cdr;
    if (val && cursp() >= slimit) {
      pop_n(len*2);
      if (!update) {
        genop_2(s, OP_HASH, cursp(), len);
      }
      else {
        pop();
        genop_2(s, OP_HASHADD, cursp(), len);
      }
      push();
      update = TRUE;
      len = 0;
    }
    first = FALSE;
  }
  if (val && len > limit) {
    pop_n(len*2);
    genop_2(s, OP_HASH, cursp(), len);
    push();
    return -1;
  }
  if (update) {
    if (val && len > 0) {
      pop_n(len*2+1);
      genop_2(s, OP_HASHADD, cursp(), len);
      push();
    }
    return -1;                  /* variable length */
  }
  return len;
}


static void
gen_colon_assign_common(codegen_scope *s, node *rhs, int sp, int val, int idx, int final_op)
{
  if (rhs) {
    codegen(s, rhs, VAL);
    pop();
    gen_move(s, sp, cursp(), 0);
  }
  pop(); pop();
  genop_2(s, final_op, cursp(), idx);
  if (val) push();
}

static void
gen_colon2_assign(codegen_scope *s, node *varnode, node *rhs, int sp, int val)
{
  struct mrb_ast_colon2_node *n = (struct mrb_ast_colon2_node*)varnode;
  int idx;

  if (sp) {
    gen_move(s, cursp(), sp, 0);
  }
  sp = cursp();
  push();
  codegen(s, n->base, VAL);
  idx = sym_idx(s, n->name);
  gen_colon_assign_common(s, rhs, sp, val, idx, OP_SETMCNST);
}

static void
gen_colon3_assign(codegen_scope *s, node *varnode, node *rhs, int sp, int val)
{
  struct mrb_ast_colon3_node *n = (struct mrb_ast_colon3_node*)varnode;
  int idx;

  if (sp) {
    gen_move(s, cursp(), sp, 0);
  }
  sp = cursp();
  push();
  genop_1(s, OP_OCLASS, cursp());
  push();
  idx = sym_idx(s, n->name);
  gen_colon_assign_common(s, rhs, sp, val, idx, OP_SETCONST);
}

static void
gen_xvar_assignment(codegen_scope *s, node *tree, node *rhs, int sp, int val, uint8_t op)
{
  struct mrb_ast_var_node *var = (struct mrb_ast_var_node*)tree;
  if (rhs) {
    codegen(s, rhs, VAL);
    pop();
    sp = cursp();
  }
  gen_setxv(s, op, sp, var->symbol, val);
}

static void
gen_xvar(codegen_scope *s, mrb_sym sym, int val, uint8_t op)
{
  if (!val) return;
  int i = sym_idx(s, sym);

  genop_2(s, op, cursp(), i);
  push();
}

static void
gen_assignment(codegen_scope *s, node *tree, node *rhs, int sp, int val)
{
  int idx;

  /* Check if this is a variable-sized node first */
  enum node_type var_type = node_type(tree);
  switch (var_type) {
  case NODE_NIL:
    if (rhs) {
      codegen(s, rhs, VAL);
      pop();
      sp = cursp();
    }
    /* NODE_NIL assignment is complete - just break (splat without assignment) */
    break;
  case NODE_COLON2:
    gen_colon2_assign(s, tree, rhs, sp, val);
    return;
  case NODE_COLON3:
    gen_colon3_assign(s, tree, rhs, sp, val);
    return;
  case NODE_GVAR:
    gen_xvar_assignment(s, tree, rhs, sp, val, OP_SETGV);
    break;
  case NODE_IVAR:
    gen_xvar_assignment(s, tree, rhs, sp, val, OP_SETIV);
    break;
  case NODE_CVAR:
    gen_xvar_assignment(s, tree, rhs, sp, val, OP_SETCV);
    break;
  case NODE_CONST:
    gen_xvar_assignment(s, tree, rhs, sp, val, OP_SETCONST);
    break;
  case NODE_MASGN:
  case NODE_MARG:
    /* Multiple assignment: expressions (MASGN) and parameter destructuring (MARG) */
    codegen_masgn(s, tree, rhs, sp, val);
    return;
  case NODE_LVAR:
    {
      mrb_sym sym = var_node(tree)->symbol;
      if (rhs) {
        codegen(s, rhs, VAL);
        pop();
        sp = cursp();
      }
      idx = lv_idx(s, sym);
      if (idx > 0) {
        if (idx != sp) {
          gen_move(s, idx, sp, val);
        }
        break;
      }
      else {
        gen_setupvar(s, sp, sym);
      }
    }
    break;
  case NODE_CALL:
    codegen_call_assign(s, tree, rhs, sp, val);
    return;
  default:
    codegen_error(s, "unsupported variable-sized lhs");
    break;
  }
  if (val) push();
  return;
}

static void
gen_massignment(codegen_scope *s, node *tree, int rhs, int val)
{
  int n = 0, post = 0;
  node *t, *p;

  if (tree->car) {              /* pre */
    t = tree->car;
    n = 0;
    while (t) {
      int sp = cursp();

      genop_3(s, OP_AREF, sp, rhs, n);
      push();
      gen_assignment(s, t->car, NULL, sp, NOVAL);
      pop();
      n++;
      t = t->cdr;
    }
  }
  t = tree->cdr;
  if (t) {
    if (t->cdr) {               /* post count */
      p = t->cdr->car;
      while (p) {
        post++;
        p = p->cdr;
      }
    }
    gen_move(s, cursp(), rhs, val);
    push_n(post+1);
    pop_n(post+1);
    genop_3(s, OP_APOST, cursp(), n, post);
    n = 1;
    if (t->car && t->car != (node*)-1) { /* rest */
      gen_assignment(s, t->car, NULL, cursp(), NOVAL);
    }
    if (t->cdr && t->cdr->car) {
      t = t->cdr->car;
      while (t) {
        gen_assignment(s, t->car, NULL, cursp()+n, NOVAL);
        t = t->cdr;
        n++;
      }
    }
    if (val) {
      gen_move(s, cursp(), rhs, 0);
    }
  }
}

static void
gen_intern(codegen_scope *s)
{
  pop();
  if (!no_peephole(s)) {
    struct mrb_insn_data data = mrb_last_insn(s);

    if (data.insn == OP_STRING && data.a == cursp()) {
      rewind_pc(s);
      genop_2(s, OP_SYMBOL, data.a, data.b);
      push();
      return;
    }
  }
  genop_1(s, OP_INTERN, cursp());
  push();
}

static void
gen_literal_array(codegen_scope *s, node *tree, mrb_bool sym, int val)
{
  if (val) {
    int array_size = 0;
    node *current = tree;

    /* Process each segment separated by NODE_LITERAL_DELIM */
    while (current) {
      /* Find the segment boundaries without allocating */
      node *segment_start = current;
      node *segment_prev = NULL;

      /* Find end of segment (delimiter or end of list) */
      while (current && !IS_LITERAL_DELIM(current)) {
        segment_prev = current;
        current = current->cdr;
      }

      /* Process the segment if it has content */
      if (segment_start != current) {
        /* Check if this is an empty string segment (for %w[] case) */
        mrb_bool is_empty_segment = TRUE;
        node *check = segment_start;
        while (check != current) {
          if (check->car) {
            mrb_int len = node_to_int(check->car->car);
            if (len > 0) {
              is_empty_segment = FALSE;
              break;
            }
            else if (len < 0) {
              /* Expression node - not empty */
              is_empty_segment = FALSE;
              break;
            }
            /* len == 0 means empty string, continue checking */
          }
          check = check->cdr;
        }

        /* Only process non-empty segments */
        if (!is_empty_segment) {
          /* Temporarily terminate the segment by saving and clearing the cdr */
          node *saved_cdr = NULL;
          if (segment_prev) {
            saved_cdr = segment_prev->cdr;
            segment_prev->cdr = NULL;
          }

          /* Use gen_string for this segment */
          gen_string(s, segment_start, VAL);

          /* Restore the original cdr */
          if (segment_prev) {
            segment_prev->cdr = saved_cdr;
          }

          /* Apply symbol conversion if needed */
          if (sym) {
            gen_intern(s);
          }

          array_size++;
        }
      }

      /* Skip the delimiter if present */
      if (current && IS_LITERAL_DELIM(current)) {
        current = current->cdr;
      }
    }

    /* Generate the array from pushed elements */
    if (array_size > 0) {
      pop_n(array_size);
      genop_2(s, OP_ARRAY, cursp(), array_size);
    }
    else {
      genop_2(s, OP_ARRAY, cursp(), 0);
    }
    push();
  }
  else {
    /* NOVAL case: only evaluate expressions for side effects */
    node *current = tree;

    while (current) {
      /* Process nodes until delimiter */
      while (current && !IS_LITERAL_DELIM(current)) {
        node *elem = current->car;
        if (elem) {
          mrb_int len = node_to_int(elem->car);
          if (len < 0) {
            /* Expression: (-1 . node) - evaluate for side effects */
            codegen(s, (node*)elem->cdr, NOVAL);
          }
          /* String literals: (len . str) - no side effects, skip */
        }
        current = current->cdr;
      }

      /* Skip delimiter */
      if (current && IS_LITERAL_DELIM(current)) {
        current = current->cdr;
      }
    }
  }
}

static void
raise_error(codegen_scope *s, const char *msg)
{
  int idx = new_lit_cstr(s, msg);

  genop_1(s, OP_ERR, idx);
}

static void
gen_retval(codegen_scope *s, node *tree)
{
  if (is_splat_node(tree)) {
    codegen(s, tree, VAL);
    pop();
    genop_1(s, OP_ARYSPLAT, cursp());
  }
  else {
    codegen(s, tree, VAL);
    pop();
  }
}

static mrb_bool
true_always(node *tree)
{
  /* Check if this is a variable-sized node first */
  enum node_type var_type = node_type(tree);
  switch (var_type) {
  case NODE_INT:
  case NODE_BIGINT:
  case NODE_FLOAT:
  case NODE_TRUE:
    return TRUE;
  default:
    return FALSE;
  }
}

static mrb_bool
false_always(node *tree)
{
  /* Check variable-sized nodes that are always false */
  switch (node_type(tree)) {
  case NODE_FALSE:
  case NODE_NIL:
    return TRUE;
  default:
    return FALSE;
  }
}

static void
gen_blkmove(codegen_scope *s, uint16_t ainfo, int lv)
{
  int m1 = (ainfo>>7)&0x3f;
  int r  = (ainfo>>6)&0x1;
  int m2 = (ainfo>>1)&0x1f;
  int kd = (ainfo)&0x1;
  int off = m1+r+m2+kd+1;
  if (lv == 0) {
    gen_move(s, cursp(), off, 0);
  }
  else {
    genop_3(s, OP_GETUPVAR, cursp(), off, lv);
  }
  push();
}

static void
gen_lvar(codegen_scope *s, mrb_sym sym, int val)
{
  if (!val) return;
  int idx = lv_idx(s, sym);

  if (idx > 0) {
    gen_move(s, cursp(), idx, val);
  }
  else {
    gen_getupvar(s, cursp(), sym);
  }
  push();
}

static void
codegen_hash(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_hash_node *hash = hash_node(varnode);
  node *pairs = hash->pairs;
  int regular_pairs = 0;
  mrb_bool update = FALSE;
  mrb_bool first = TRUE;

  if (!val) return;

  if (!pairs) {
    genop_2(s, OP_HASH, cursp(), 0);
    push();
    return;
  }

  /* Process each key-value pair using cons-list iteration, handling double-splat (**) cases */
  node *current = pairs;
  while (current) {
    /* Each current->car is a cons (key . value) */
    node *pair = current->car;
    struct mrb_ast_node *key = pair->car;
    struct mrb_ast_node *value = pair->cdr;

    /* Check if this is a double-splat (**kwargs) */
    if (node_to_sym(key) == MRB_OPSYM(pow)) {
      /* Flush any accumulated regular pairs first */
      if (val && first && regular_pairs == 0) {
        /* First element is splat - create empty hash */
        genop_2(s, OP_HASH, cursp(), 0);
        push();
        update = TRUE;
      }
      else if (val && regular_pairs > 0) {
        /* Create/add hash from accumulated pairs */
        pop_n(regular_pairs * 2);
        if (!update) {
          genop_2(s, OP_HASH, cursp(), regular_pairs);
        }
        else {
          pop();
          genop_2(s, OP_HASHADD, cursp(), regular_pairs);
        }
        push();
      }

      /* Generate the splat hash */
      codegen(s, value, val);

      /* Merge the splat hash */
      if (val && (regular_pairs > 0 || update)) {
        pop(); pop();
        genop_1(s, OP_HASHCAT, cursp());
        push();
      }

      update = TRUE;
      regular_pairs = 0;
    }
    else {
      /* Regular key-value pair */
      codegen(s, key, val);
      codegen(s, value, val);
      regular_pairs++;
    }
    first = FALSE;

    current = current->cdr;
  }

  /* Handle any remaining regular pairs */
  if (val) {
    if (!update && regular_pairs > 0) {
      /* Simple case: no splats, just create hash */
      pop_n(regular_pairs * 2);
      genop_2(s, OP_HASH, cursp(), regular_pairs);
      push();
    }
    else if (update && regular_pairs > 0) {
      /* Add remaining pairs to existing hash */
      pop_n(regular_pairs * 2 + 1);
      genop_2(s, OP_HASHADD, cursp(), regular_pairs);
      push();
    }
  }
}



/* Common function to generate bytecode for cons list string representation
 * Handles list of elements where each element is either:
 * - (len . str) for string literals
 * - (-1 . node) for expressions that need evaluation
 */
/* Common function to generate bytecode for cons list string representation
 * Handles list of elements where each element is either:
 * - (len . str) for string literals
 * - (-1 . node) for expressions that need evaluation
 */
/* Common function to generate bytecode for cons list string representation
 * Handles list of elements where each element is either:
 * - (len . str) for string literals
 * - (-1 . node) for expressions that need evaluation
 */
/* Common function to generate bytecode for cons list string representation
 * Handles list of elements where each element is either:
 * - (len . str) for string literals
 * - (-1 . node) for expressions that need evaluation
 */
static void
gen_string(codegen_scope *s, node *list, int val)
{
  if (val) {
    /* Handle as cons list of string parts with safety checks */
    node *n = list;
    mrb_bool first = TRUE;

    while (n) {
      node *elem = n->car;
      if (!elem) break;

      mrb_int len = node_to_int(elem->car);

      if (len >= 0) {
        /* String literal: (len . str) */
        const char *str = (char*)elem->cdr;
        if (!str) {str = ""; len = 0;}
        int off = new_lit_str(s, str, len);
        genop_2(s, OP_STRING, cursp(), off);
        push();
      }
      else {
        /* Expression: (-1 . node) */
        codegen(s, (node*)elem->cdr, VAL);
      }

      /* Concatenate with previous parts (except for first element) */
      if (!first) {
        pop(); pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
      }
      else {
        first = FALSE;
      }

      n = n->cdr;
    }

    /* Handle empty list case */
    if (first) {
      gen_load_nil(s, 1);
    }
  }
  else {
    /* NOVAL case: only evaluate expressions for side effects */
    node *n = list;
    while (n) {
      node *elem = n->car;
      if (!elem) break;
      if (node_to_int(elem->car) < 0) {
        /* Expression: (-1 . node) - evaluate for side effects */
        codegen(s, (node*)elem->cdr, NOVAL);
      }
      /* String literals: (len . str) - no side effects, skip */
      n = n->cdr;
    }
  }
}


/* Handle variable-sized node types */
static void
codegen_call(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_call_node *call = call_node(varnode);
  mrb_sym sym = call->method_name;
  int skip = 0, n = 0, nk = 0, noop = no_optimize(s), noself = 0, blk = 0, sp_save = cursp();
  enum mrb_insn opt_op = OP_NOP;
  int safe = call->safe_call;
  node *args = call->args;

  if (!noop) {
    if (sym == MRB_OPSYM(add)) opt_op = OP_ADD;
    else if (sym == MRB_OPSYM(sub)) opt_op = OP_SUB;
    else if (sym == MRB_OPSYM(mul)) opt_op = OP_MUL;
    else if (sym == MRB_OPSYM(div)) opt_op = OP_DIV;
    else if (sym == MRB_OPSYM(lt)) opt_op = OP_LT;
    else if (sym == MRB_OPSYM(le)) opt_op = OP_LE;
    else if (sym == MRB_OPSYM(gt)) opt_op = OP_GT;
    else if (sym == MRB_OPSYM(ge)) opt_op = OP_GE;
    else if (sym == MRB_OPSYM(eq)) opt_op = OP_EQ;
    else if (sym == MRB_OPSYM(aref)) opt_op = OP_GETIDX;
    else if (sym == MRB_OPSYM(aset)) opt_op = OP_SETIDX;
  }

  if (!call->receiver || (opt_op == OP_NOP && node_type(call->receiver) == NODE_SELF)) {
    noself = 1;
    push();
  }
  else {
    codegen(s, call->receiver, VAL); /* receiver */
  }

  if (safe) {
    int recv = cursp()-1;
    gen_move(s, cursp(), recv, 1);
    skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
  }

  /* Generate arguments - use gen_values to properly handle splat */
  if (args) {
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)args;
    if (callargs->regular_args) {
      n = gen_values(s, callargs->regular_args, VAL, 14);
      if (n < 0) {              /* variable length (contains splat) */
        n = 15;
        push();
        noop = 1;
      }
    }

    /* Handle keyword arguments if present */
    if (callargs->keyword_args) {
      nk = gen_hash(s, callargs->keyword_args, VAL, 14);
      if (nk < 0) {
        nk = 15;
      }
      noop = 1;
    }

    /* Handle block if present */
    if (callargs->block_arg) {
      codegen(s, callargs->block_arg, VAL);
      pop();
      blk = 1;
      noop = 1;
    }
  }

  push();
  s->sp = sp_save;

  /* Apply optimizations */
  if (opt_op == OP_ADD && n == 1) {
    gen_addsub(s, OP_ADD, cursp());
  }
  else if (opt_op == OP_SUB && n == 1) {
    gen_addsub(s, OP_SUB, cursp());
  }
  else if (opt_op == OP_MUL && n == 1) {
    gen_muldiv(s, OP_MUL, cursp());
  }
  else if (opt_op == OP_DIV && n == 1) {
    gen_muldiv(s, OP_DIV, cursp());
  }
  else if (opt_op == OP_LT && n == 1) {
    genop_1(s, OP_LT, cursp());
  }
  else if (opt_op == OP_LE && n == 1) {
    genop_1(s, OP_LE, cursp());
  }
  else if (opt_op == OP_GT && n == 1) {
    genop_1(s, OP_GT, cursp());
  }
  else if (opt_op == OP_GE && n == 1) {
    genop_1(s, OP_GE, cursp());
  }
  else if (opt_op == OP_EQ && n == 1) {
    genop_1(s, OP_EQ, cursp());
  }
  else if (opt_op == OP_SETIDX && n == 2) {
    genop_1(s, OP_SETIDX, cursp());
  }
  else if (!noop && n == 0 && gen_uniop(s, sym, cursp())) {
    /* constant folding succeeded */
  }
  else if (!noop && n == 1 && gen_binop(s, sym, cursp())) {
    /* constant folding succeeded */
  }
  else if (noself) {
    if (!blk && n == 0 && nk == 0) {
      genop_2(s, OP_SSEND0, cursp(), sym_idx(s, sym));
    }
    else {
      genop_3(s, blk ? OP_SSENDB : OP_SSEND, cursp(), sym_idx(s, sym), n|(nk<<4));
    }
  }
  else if (!blk && n == 0 && nk == 0) {
    genop_2(s, OP_SEND0, cursp(), sym_idx(s, sym));
  }
  else {
    genop_3(s, blk ? OP_SENDB : OP_SEND, cursp(), sym_idx(s, sym), n|(nk<<4));
  }

  if (safe) {
    dispatch(s, skip);
  }
  if (!val) return;
  push();
}

static void
codegen_call_assign(codegen_scope *s, node *varnode, node *rhs, int sp, int val)
{
  enum node_type var_type = NODE_TYPE(varnode);
  int noself = 0, safe = 0, skip = 0, top, callsp, n = 0, nk = 0;
  mrb_sym mid = 0;
  node *args = NULL;
  node *receiver = NULL;
  enum mrb_insn opt_op = OP_NOP;
  int noop = no_optimize(s);

  /* Extract information based on node type */
  if (var_type == NODE_CALL) {
    struct mrb_ast_call_node *call = call_node(varnode);
    mid = call->method_name;
    args = call->args;
    receiver = call->receiver;
    safe = call->safe_call;
  }
  else {
    codegen_error(s, "unsupported call type in assignment");
    return;
  }

  /* Convert method name to assignment form (e.g., [] -> []=) */
  mrb_sym assign_mid = attrsym(s, mid);

  /* Check for optimizable operations */
  if (!noop) {
    if (mid == MRB_OPSYM(aref)) opt_op = OP_SETIDX;
  }

  top = cursp();
  if (val || sp == cursp()) {
    push();                   /* room for retval */
  }
  callsp = cursp();

  /* Generate receiver */
  if (!receiver) {
    noself = 1;
    push();
  }
  else {
    codegen(s, receiver, VAL); /* receiver */
  }

  /* Handle safe navigation */
  if (safe) {
    int recv = cursp()-1;
    gen_move(s, cursp(), recv, 1);
    skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
  }

  /* Generate arguments from original call */
  if (args) {
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)args;
    if (callargs->regular_args) {
      node *regular_args = callargs->regular_args;
      node *arg_iter = regular_args;
      while (arg_iter) {
        codegen(s, arg_iter->car, VAL);
        n++;
        arg_iter = arg_iter->cdr;
      }
      if (n > 13) {  /* leave room for rhs */
        pop_n(n);
        genop_2(s, OP_ARRAY, cursp(), n);
        push();
        n = 15;
        noop = 1;
      }
    }

    /* Handle keyword arguments if present */
    if (callargs->keyword_args) {
      node *kwargs = callargs->keyword_args;
      if (n == 13 || n == 14) {
        pop_n(n);
        genop_2(s, OP_ARRAY, cursp(), n);
        push();
        n = 15;
      }
      gen_hash(s, kwargs->cdr, VAL, 0);
      if (n < 14) {
        n++;
      }
      else {
        pop_n(2);
        genop_2(s, OP_ARYPUSH, cursp(), 1);
      }
      push();
      noop = 1;
    }
  }

  /* Generate rhs (the assigned value) */
  if (rhs) {
    codegen(s, rhs, VAL);
    pop();
  }
  else {
    /* For compound assignments, move the computed value from sp to cursp() */
    gen_move(s, cursp(), sp, 0);
  }
  if (val) {
    gen_move(s, top, cursp(), 1);
  }
  /* Account for the value being assigned (either from rhs or already on stack) */
  if (n < 14) {
    n++;
  }
  else {
    if (rhs) {
      pop_n(2);
      genop_2(s, OP_ARYPUSH, cursp(), 1);
      push();
    }
  }

  /* Generate the optimized instruction or method call */
  push(); push();
  s->sp = callsp;

  if (opt_op == OP_SETIDX && n == 2) {
    /* Always preserve return value for SETIDX - assignments return the assigned value */
    genop_1(s, OP_SETIDX, cursp());
  }
  else if (noself) {
    genop_3(s, OP_SSEND, cursp(), sym_idx(s, assign_mid), n|(nk<<4));
  }
  else {
    genop_3(s, OP_SEND, cursp(), sym_idx(s, assign_mid), n|(nk<<4));
  }

  if (safe) {
    dispatch(s, skip);
  }

  /* Restore stack pointer like legacy code */
  s->sp = top;

  if (val) {
    push();
  }
}

static void
codegen_array(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_array_node *array = array_node(varnode);
  node *elements = array->elements;
  int regular_elements = 0;
  int first = 1;
  int slimit = GEN_VAL_STACK_MAX;

  if (!val) return;

  if (!elements) {
    genop_2(s, OP_ARRAY, cursp(), 0);
    push();
    return;
  }

  if (cursp() >= GEN_LIT_ARY_MAX) slimit = INT16_MAX;

  /* Process each element using cons-list iteration, handling splats */
  node *current = elements;
  while (current) {
    struct mrb_ast_node *element = current->car;
    int is_splat = is_splat_node(element);

    /* Skip splat of an empty literal array: [*[]] => [] without ARYCAT noise */
    if (is_splat) {
      struct mrb_ast_splat_node *splat = splat_node(element);
      node *sv = splat->value;
      if (sv) {
        enum node_type nt = node_type(sv);
        if (nt == NODE_ARRAY) {
          struct mrb_ast_array_node *an = array_node(sv);
          if (an->elements == NULL) {
            current = current->cdr;
            continue;
          }
        }
        else if (nt == NODE_ZARRAY) {
          current = current->cdr;
          continue;
        }
      }
    }

    if (is_splat || cursp() >= slimit) { /* flush accumulated elements */
      if (regular_elements > 0) {
        pop_n(regular_elements);
        if (first) {
          genop_2(s, OP_ARRAY, cursp(), regular_elements);
          push();
          first = 0;
        }
        else {
          pop();
          genop_2(s, OP_ARYPUSH, cursp(), regular_elements);
          push();
        }
        regular_elements = 0;
      }
      else if (first && is_splat) {
        /* First element is splat - create empty array */
        genop_1(s, OP_LOADNIL, cursp());
        genop_2(s, OP_ARRAY, cursp(), 0);
        push();
        first = 0;
      }
    }

    codegen(s, element, val);

    if (is_splat) {
      /* Concatenate splat array */
      pop(); pop();
      genop_1(s, OP_ARYCAT, cursp());
      push();
    }
    else {
      regular_elements++;
    }

    current = current->cdr;
  }

  /* Handle any remaining regular elements */
  if (!first) {
    /* Variable length - we have an array from splats */
    if (regular_elements > 0) {
      pop_n(regular_elements + 1);
      genop_2(s, OP_ARYPUSH, cursp(), regular_elements);
      push();
    }
  }
  else {
    /* Simple case: no splats, just create array */
    pop_n(regular_elements);
    genop_2(s, OP_ARRAY, cursp(), regular_elements);
    push();
  }
}

/* Control flow and definition node codegen functions */
static mrb_bool
callargs_empty(node *n)
{
  if (!n) return TRUE;
  return (callargs_node(n)->regular_args == 0 && callargs_node(n)->keyword_args == 0 && callargs_node(n)->block_arg == 0);
}

static void
codegen_if(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_if_node *if_n = if_node(varnode);
  node *condition = if_n->condition;
  node *then_body = if_n->then_body;
  node *else_body = if_n->else_body;
  uint32_t pos1, pos2;
  mrb_bool nil_p = FALSE;

  if (!condition) {
    codegen(s, else_body, val);
    return;
  }
  if (true_always(condition)) {
    codegen(s, then_body, val);
    return;
  }
  if (false_always(condition)) {
    codegen(s, else_body, val);
    return;
  }

  /* Check for nil? optimization */
  if (node_type(condition) == NODE_CALL) {
    /* Variable-sized NODE_CALL */
    struct mrb_ast_call_node *call_n = (struct mrb_ast_call_node*)condition;
    mrb_sym sym_nil_p = MRB_SYM_Q(nil);
    if (call_n->method_name == sym_nil_p && callargs_empty(call_n->args)) {
      nil_p = TRUE;
      codegen(s, call_n->receiver, VAL);
    }
  }

  if (!nil_p) {
    /* Generate condition code */
    codegen(s, condition, VAL);
  }
  pop();

  if (val || then_body) {
    if (nil_p) {
      pos2 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
      pos1 = genjmp_0(s, OP_JMP);
      dispatch(s, pos2);
    }
    else {
      pos1 = genjmp2_0(s, OP_JMPNOT, cursp(), val);
    }
    codegen(s, then_body, val);
    if (val) pop();
    if (else_body || val) {
      pos2 = genjmp_0(s, OP_JMP);
      dispatch(s, pos1);
      codegen(s, else_body, val);
      dispatch(s, pos2);
    }
    else {
      dispatch(s, pos1);
    }
  }
  else {  /* empty then-part */
    if (else_body) {
      if (nil_p) {
        pos1 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
      }
      else {
        pos1 = genjmp2_0(s, OP_JMPIF, cursp(), val);
      }
      codegen(s, else_body, val);
      dispatch(s, pos1);
    }
    else if (val && !nil_p) {
      gen_load_nil(s, 1);
    }
  }
}

static void
codegen_while(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_while_node *while_n = while_node(varnode);
  node *condition = while_n->condition;
  node *body = while_n->body;

  /* Check for constant conditions first */
  if (true_always(condition)) {
    /* while true - infinite loop, don't generate condition check */
    struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
    if (!val) lp->reg = -1;
    lp->pc0 = new_label(s);
    lp->pc1 = new_label(s);
    genop_0(s, OP_NOP); /* for redo */
    codegen(s, body, NOVAL);
    genjmp(s, OP_JMP, lp->pc0);
    loop_pop(s, val);
    return;
  }
  if (false_always(condition)) {
    /* while false - never execute, just return nil */
    if (val) {
      gen_load_nil(s, 1);
    }
    return;
  }

  struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
  uint32_t pos;

  if (!val) lp->reg = -1;
  lp->pc0 = new_label(s);
  codegen(s, condition, VAL);
  pop();
  pos = genjmp2_0(s, OP_JMPNOT, cursp(), NOVAL);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */
  codegen(s, body, NOVAL);
  genjmp(s, OP_JMP, lp->pc0);
  dispatch(s, pos);
  loop_pop(s, val);
}

static void
codegen_until(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_until_node *until_n = until_node(varnode);
  node *condition = until_n->condition;
  node *body = until_n->body;

  /* Check for constant conditions first */
  if (true_always(condition)) {
    /* until true - never execute, just return nil */
    if (val) {
      gen_load_nil(s, 1);
    }
    return;
  }
  if (false_always(condition)) {
    /* until false - infinite loop, don't generate condition check */
    struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
    if (!val) lp->reg = -1;
    lp->pc0 = new_label(s);
    lp->pc1 = new_label(s);
    genop_0(s, OP_NOP); /* for redo */
    codegen(s, body, NOVAL);
    genjmp(s, OP_JMP, lp->pc0);
    loop_pop(s, val);
    return;
  }

  struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
  uint32_t pos;

  if (!val) lp->reg = -1;
  lp->pc0 = new_label(s);
  codegen(s, condition, VAL);
  pop();
  pos = genjmp2_0(s, OP_JMPIF, cursp(), NOVAL);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */
  codegen(s, body, NOVAL);
  genjmp(s, OP_JMP, lp->pc0);
  dispatch(s, pos);
  loop_pop(s, val);
}

static void
codegen_while_mod(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_while_node *while_n = while_node(varnode);
  node *condition = while_n->condition;
  node *body = while_n->body;

  /* Handle special constant cases for post-tested loops */
  if (false_always(condition)) {
    /* begin...end while false - execute once then exit */
    codegen(s, body, val);
    if (val) push();
    return;
  }
  if (true_always(condition)) {
    /* begin...end while true - infinite loop after first execution */
    struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
    if (!val) lp->reg = -1;

    uint32_t pos0 = genjmp_0(s, OP_JMP);
    lp->pc0 = new_label(s);
    lp->pc1 = new_label(s);
    genop_0(s, OP_NOP); /* for redo */
    dispatch(s, pos0);
    codegen(s, body, NOVAL);
    genjmp(s, OP_JMP, lp->pc0);
    loop_pop(s, val);
    return;
  }

  /* Normal post-tested while loop */
  struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
  if (!val) lp->reg = -1;

  uint32_t pos0 = genjmp_0(s, OP_JMP);
  lp->pc0 = new_label(s);
  codegen(s, condition, VAL);
  pop();
  uint32_t pos = genjmp2_0(s, OP_JMPNOT, cursp(), NOVAL);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */
  dispatch(s, pos0);
  codegen(s, body, NOVAL);
  genjmp(s, OP_JMP, lp->pc0);
  dispatch(s, pos);
  loop_pop(s, val);
}

static void
codegen_until_mod(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_until_node *until_n = until_node(varnode);
  node *condition = until_n->condition;
  node *body = until_n->body;

  /* Handle special constant cases for post-tested loops */
  if (true_always(condition)) {
    /* begin...end until true - execute once then exit */
    codegen(s, body, val);
    if (val) push();
    return;
  }
  if (false_always(condition)) {
    /* begin...end until false - infinite loop after first execution */
    struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
    if (!val) lp->reg = -1;

    uint32_t pos0 = genjmp_0(s, OP_JMP);
    lp->pc0 = new_label(s);
    lp->pc1 = new_label(s);
    genop_0(s, OP_NOP); /* for redo */
    dispatch(s, pos0);
    codegen(s, body, NOVAL);
    genjmp(s, OP_JMP, lp->pc0);
    loop_pop(s, val);
    return;
  }

  /* Normal post-tested until loop */
  struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
  if (!val) lp->reg = -1;

  uint32_t pos0 = genjmp_0(s, OP_JMP);
  lp->pc0 = new_label(s);
  codegen(s, condition, VAL);
  pop();
  uint32_t pos = genjmp2_0(s, OP_JMPIF, cursp(), NOVAL);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */
  dispatch(s, pos0);
  codegen(s, body, NOVAL);
  genjmp(s, OP_JMP, lp->pc0);
  dispatch(s, pos);
  loop_pop(s, val);
}

static void
codegen_for(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_for_node *for_n = for_node(varnode);
  node *var = for_n->var;
  node *iterable = for_n->iterable;
  node *body = for_n->body;

  codegen_scope *prev = s;
  int idx;
  struct loopinfo *lp;

  /* generate receiver */
  codegen(s, iterable, VAL);
  /* generate loop-block */
  s = scope_new(s->mrb, s, NULL);

  push();                       /* push for a block parameter */

  /* generate loop variable */
  genop_W(s, OP_ENTER, 0x40000);
  if (var->car && !var->car->cdr && !var->cdr) {
    gen_assignment(s, var->car->car, NULL, 1, NOVAL);
  }
  else {
    gen_massignment(s, var, 1, VAL);
  }
  /* construct loop */
  lp = loop_push(s, LOOP_FOR);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */

  /* loop body */
  codegen(s, body, VAL);
  pop();
  gen_return(s, OP_RETURN, cursp());
  loop_pop(s, NOVAL);
  scope_finish(s);
  s = prev;
  genop_2(s, OP_BLOCK, cursp(), s->irep->rlen-1);
  push();pop(); /* space for a block */
  pop();
  idx = sym_idx(s, MRB_SYM(each));
  genop_3(s, OP_SENDB, cursp(), idx, 0);
  if (val) push();
}

static void
codegen_case(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_case_node *case_n = case_node(varnode);
  node *value = case_n->value;
  node *body = case_n->body;

  int head = 0;
  uint32_t case_end_jumps, tmp;
  uint32_t next_when_pos = JMPLINK_START;
  node *n;

  case_end_jumps = JMPLINK_START;

  /* Handle case value exactly like original */
  if (value) {
    head = cursp();
    codegen(s, value, VAL);
  }

  /* Iterate through when clauses list with JMPNOT optimization */
  node *current_when = body;
  while (current_when) {
    node *when_clause = current_when->car;

    /* Dispatch previous when's "next" jump to this location */
    if (next_when_pos != JMPLINK_START) {
      dispatch_linked(s, next_when_pos);
      next_when_pos = JMPLINK_START;
    }

    /* when_clause is (condition . body) cons node */
    node *args = when_clause->car;  /* when conditions */
    node *when_body = when_clause->cdr;  /* when body */

    /* Process when conditions with JMPNOT optimization */
    n = args;
    uint32_t condition_success_pos = JMPLINK_START;

    while (n) {
      codegen(s, n->car, VAL);
      if (head) {
        gen_move(s, cursp(), head, 0);
        push(); push(); pop(); pop(); pop();
        if (is_splat_node(n->car)) {
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_SYM(__case_eqq)), 1);
        }
        else {
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(eqq)), 1);
        }
      }
      else {
        pop();
      }

      if (n->cdr) {
        /* More conditions in this when - use JMPIF to success handler */
        tmp = genjmp2(s, OP_JMPIF, cursp(), condition_success_pos, !head);
        condition_success_pos = tmp;
      }
      else {
        /* Last condition - use JMPNOT to next when clause */
        tmp = genjmp2(s, OP_JMPNOT, cursp(), next_when_pos, !head);
        next_when_pos = tmp;
      }
      n = n->cdr;
    }

    /* Dispatch multiple condition success jumps to body */
    if (condition_success_pos != JMPLINK_START) {
      dispatch_linked(s, condition_success_pos);
    }

    /* Generate when body */
    codegen(s, when_body, val);
    if (val) pop();

    /* Check if this is the last when clause before else, or if there's no else clause */
    node *next_node = current_when->cdr;

    tmp = genjmp(s, OP_JMP, case_end_jumps);
    case_end_jumps = tmp;

    current_when = next_node;
  }

  /* Handle case where no else clause was found */
  if (next_when_pos != JMPLINK_START) {
    dispatch_linked(s, next_when_pos);
    /* No else clause, generate LOADNIL for VAL case */
    if (val) {
      genop_1(s, OP_LOADNIL, cursp());
    }
  }

  /* Apply stack management strategy for cases without else clause */
  if (val) {
    /* Dispatch remaining case_end_jumps */
    if (case_end_jumps != JMPLINK_START) {
      dispatch_linked(s, case_end_jumps);
    }
    if (head) {
      /* Move result to original case value position */
      gen_move(s, head, cursp(), 0);
      pop();
    }
    /* Always push to maintain stack alignment */
    push();
  }
  else {
    /* NOVAL case */
    if (case_end_jumps != JMPLINK_START) {
      dispatch_linked(s, case_end_jumps);
    }
    if (head) {
      pop();
    }
  }
}

/* Forward declaration for pattern matching code generation
 * known_array_len: -1 if unknown, >= 0 if target is known to be an array of that length
 */
static void codegen_pattern(codegen_scope *s, node *pattern, int target, uint32_t *fail_pos, int known_array_len);

/* Pattern matching case/in expression */
static void
codegen_case_match(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_case_match_node *case_match_n = case_match_node(varnode);
  node *value = case_match_n->value;
  node *in_clauses = case_match_n->in_clauses;

  int head = cursp();
  uint32_t case_end_jumps = JMPLINK_START;
  uint32_t tmp;

  /* Check if value is an array literal - allows optimizations in pattern matching */
  int known_array_len = -1;
  if (node_type(value) == NODE_ARRAY) {
    struct mrb_ast_array_node *arr = array_node(value);
    node *elem;
    known_array_len = 0;
    for (elem = arr->elements; elem; elem = elem->cdr) known_array_len++;
  }

  /* Generate code for the case value */
  codegen(s, value, VAL);

  /* Iterate through in clauses */
  node *current_in = in_clauses;
  while (current_in) {
    struct mrb_ast_in_node *in_n = in_node(current_in->car);
    node *pattern = in_n->pattern;
    node *guard = in_n->guard;
    mrb_bool guard_is_unless = in_n->guard_is_unless;
    node *body = in_n->body;

    uint32_t fail_pos = JMPLINK_START;

    if (pattern) {
      /* Generate pattern matching code */
      codegen_pattern(s, pattern, head, &fail_pos, known_array_len);
    }

    /* Generate guard clause if present */
    if (guard) {
      codegen(s, guard, VAL);
      pop();  /* pop before jump - cursp() now points to guard result */
      if (guard_is_unless) {
        /* unless guard: fail if guard is true */
        tmp = genjmp2(s, OP_JMPIF, cursp(), fail_pos, 0);
      }
      else {
        /* if guard: fail if guard is false */
        tmp = genjmp2(s, OP_JMPNOT, cursp(), fail_pos, 0);
      }
      fail_pos = tmp;
    }

    /* Generate in-clause body */
    codegen(s, body, val);
    if (val) pop();

    /* Jump to end of case/in */
    tmp = genjmp(s, OP_JMP, case_end_jumps);
    case_end_jumps = tmp;

    /* Dispatch fail jumps to next in-clause */
    if (fail_pos != JMPLINK_START) {
      dispatch_linked(s, fail_pos);
    }

    current_in = current_in->cdr;
  }

  /* No pattern matched - generate nil or error */
  if (val) {
    genop_1(s, OP_LOADNIL, cursp());
  }

  /* Dispatch all end jumps */
  if (case_end_jumps != JMPLINK_START) {
    dispatch_linked(s, case_end_jumps);
  }

  if (val) {
    /* Move result to original case value position */
    gen_move(s, head, cursp(), 0);
    pop();
    push();
  }
  else {
    pop();  /* pop the case value */
  }
}

/* Generate pattern matching code for a single pattern.
 * target: stack position of the value being matched
 * fail_pos: linked list of jump positions for pattern match failure
 * known_array_len: -1 if unknown, >= 0 if target is known to be an array of that length
 */
/* generate code to load a hash pattern key onto the stack */
static void
gen_pat_key(codegen_scope *s, node *key)
{
  if (node_type(key) == NODE_SYM) {
    genop_2(s, OP_LOADSYM, cursp(), sym_idx(s, sym_node(key)->symbol));
  }
  else {
    codegen(s, key, VAL);
  }
}

/* generate OP_ARRAY of hash pattern keys on the stack */
static void
gen_pat_keys_ary(codegen_scope *s, node *pairs, int num_keys)
{
  int i = 0;
  node *pair;
  for (pair = pairs; pair; pair = pair->cdr, i++) {
    gen_pat_key(s, pair->car->car);
    push();
  }
  genop_2(s, OP_ARRAY, cursp() - num_keys, num_keys);
  for (i = 1; i < num_keys; i++) pop();
}

static void
codegen_pattern(codegen_scope *s, node *pattern, int target, uint32_t *fail_pos, int known_array_len)
{
  uint32_t tmp;

  switch (node_type(pattern)) {
  case NODE_PAT_VALUE:
    {
      struct mrb_ast_pat_value_node *pat_val = pat_value_node(pattern);
      /* Generate: pattern_value === target */
      codegen(s, pat_val->value, VAL);
      gen_move(s, cursp(), target, 0);
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(eqq)), 1);
      /* Jump to fail if not matched */
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;
    }
    break;

  case NODE_PAT_VAR:
    {
      struct mrb_ast_pat_var_node *pat_var = pat_var_node(pattern);
      if (pat_var->name) {
        /* Bind the matched value to the variable */
        int idx = lv_idx(s, pat_var->name);
        if (idx > 0) {
          gen_move(s, idx, target, 1);  /* nopeep=1 to prevent optimization */
        }
      }
      /* Variable pattern always matches (wildcard if name is 0) */
    }
    break;

  case NODE_PAT_ALT:
    {
      struct mrb_ast_pat_alt_node *pat_alt = pat_alt_node(pattern);
      uint32_t left_fail = JMPLINK_START;
      uint32_t success_pos = JMPLINK_START;

      /* Try left pattern */
      codegen_pattern(s, pat_alt->left, target, &left_fail, known_array_len);

      /* Optimize JMPNOT+JMP to JMPIF when:
       * 1. Left pattern is not another NODE_PAT_ALT (avoid recursion issues)
       * 2. Left pattern generated at least one JMPNOT
       * 3. The last JMPNOT is immediately before current position
       * 4. The instruction is actually OP_JMPNOT (not OP_JMP which has
       *    different format S vs BS - converting OP_JMP would corrupt bytecode)
       * In this case, convert JMPNOT to JMPIF and skip generating JMP */
      if (node_type(pat_alt->left) != NODE_PAT_ALT &&
          left_fail != JMPLINK_START && left_fail + 2 == s->pc &&
          s->iseq[left_fail - 2] == OP_JMPNOT) {
        /* Extract the previous link from the JMPNOT chain.
         * The chain uses relative offsets where the end is marked by
         * an offset that points to address 0 (i.e., (pos+2)+offset == 0) */
        int16_t prev_offset = (int16_t)PEEK_S(s->iseq + left_fail);
        int32_t next_addr = (int32_t)(left_fail + 2) + prev_offset;
        uint32_t prev_link = (next_addr == 0) ? JMPLINK_START : (uint32_t)next_addr;
        /* Convert JMPNOT to JMPIF */
        s->iseq[left_fail - 2] = OP_JMPIF;
        /* Clear offset to mark end of success chain */
        emit_S(s, left_fail, 0);
        success_pos = left_fail;
        /* Continue with remaining fail chain */
        left_fail = prev_link;
      }
      else {
        /* Left succeeded - jump to success */
        tmp = genjmp(s, OP_JMP, success_pos);
        success_pos = tmp;
      }

      /* Left failed - try right pattern */
      if (left_fail != JMPLINK_START) {
        dispatch_linked(s, left_fail);
      }
      codegen_pattern(s, pat_alt->right, target, fail_pos, known_array_len);

      /* Dispatch success jumps */
      if (success_pos != JMPLINK_START) {
        dispatch_linked(s, success_pos);
      }
    }
    break;

  case NODE_PAT_AS:
    {
      struct mrb_ast_pat_as_node *pat_as = pat_as_node(pattern);
      /* First match the pattern */
      codegen_pattern(s, pat_as->pattern, target, fail_pos, known_array_len);
      /* Then bind the value to the variable */
      int idx = lv_idx(s, pat_as->name);
      if (idx > 0) {
        gen_move(s, idx, target, 0);
      }
    }
    break;

  case NODE_PAT_PIN:
    {
      struct mrb_ast_pat_pin_node *pat_pin = pat_pin_node(pattern);
      /* Get the current value of the pinned variable */
      int idx = lv_idx(s, pat_pin->name);
      if (idx > 0) {
        /* Compare: pinned_value === target */
        gen_move(s, cursp(), idx, 0);  /* Load pinned variable */
        push();
        gen_move(s, cursp(), target, 0);  /* Load target */
        push(); push(); pop(); pop(); pop();
        genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(eqq)), 1);
        /* Jump to fail if not matched */
        tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
        *fail_pos = tmp;
      }
      else {
        /* Variable not found - raise compile error like CRuby */
        codegen_error(s, "no such local variable for pin operator");
      }
    }
    break;

  case NODE_PAT_ARRAY:
    {
      struct mrb_ast_pat_array_node *pat_arr = pat_array_node(pattern);
      int pre_len = 0, post_len = 0;
      int arr_reg;
      node *elem;
      int i;

      /* Count pre and post elements */
      for (elem = pat_arr->pre; elem; elem = elem->cdr) pre_len++;
      for (elem = pat_arr->post; elem; elem = elem->cdr) post_len++;

      /* Optimization: if we know the target is an array, skip deconstruct */
      if (known_array_len >= 0) {
        /* Use target directly as array register */
        arr_reg = target;
        /* Compile-time size check */
        if (pat_arr->rest == 0) {
          /* No rest: exact length match required */
          if (known_array_len != pre_len) {
            /* Size mismatch - always fail */
            tmp = genjmp(s, OP_JMP, *fail_pos);
            *fail_pos = tmp;
            break;
          }
          /* Size matches, no runtime check needed */
        }
        else {
          /* Has rest: minimum length check */
          int min_len = pre_len + post_len;
          if (known_array_len < min_len) {
            /* Size too small - always fail */
            tmp = genjmp(s, OP_JMP, *fail_pos);
            *fail_pos = tmp;
            break;
          }
          /* Size sufficient, no runtime check needed */
        }

        /* Match pre-rest elements using GETIDX (faster than SEND :[]) */
        i = 0;
        for (elem = pat_arr->pre; elem; elem = elem->cdr, i++) {
          /* Get arr[i] using GETIDX */
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), i);
          genop_1(s, OP_GETIDX, cursp() - 1);  /* R[cursp-1] = R[cursp-1][R[cursp]] */
          /* Element is now at cursp-1 */
          /* Match element pattern (elements are not known arrays) */
          codegen_pattern(s, elem->car, cursp() - 1, fail_pos, -1);
          pop();  /* Clean up element slot */
        }

        /* Bind rest elements if rest is a variable */
        if (pat_arr->rest && pat_arr->rest != (node*)-1) {
          struct mrb_ast_pat_var_node *rest_var = pat_var_node(pat_arr->rest);
          if (rest_var->name) {
            int var_idx = lv_idx(s, rest_var->name);
            /* Generate: arr[pre_len..-(post_len+1)] or arr[pre_len..-1] if no post */
            gen_move(s, cursp(), arr_reg, 0);  /* arr at cursp */
            push();
            gen_int(s, cursp(), pre_len);      /* start at cursp */
            push();
            if (post_len > 0) {
              gen_int(s, cursp(), -(post_len + 1));  /* end at cursp */
            }
            else {
              gen_int(s, cursp(), -1);         /* end at cursp */
            }
            /* start at cursp-1, end at cursp; create inclusive range at cursp-1 */
            genop_1(s, OP_RANGE_INC, cursp() - 1);
            /* arr at cursp-2, range at cursp-1 */
            pop();  /* cursp now at range position */
            pop();  /* cursp now at arr position */
            genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
            if (var_idx > 0) {
              gen_move(s, var_idx, cursp(), 1);
            }
          }
        }

        /* Match post-rest elements using GETIDX */
        i = -post_len;
        for (elem = pat_arr->post; elem; elem = elem->cdr, i++) {
          /* Get arr[i] using GETIDX (negative index from end) */
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), i);
          genop_1(s, OP_GETIDX, cursp() - 1);
          /* Match element pattern */
          codegen_pattern(s, elem->car, cursp() - 1, fail_pos, -1);
          pop();  /* Clean up element slot */
        }
        /* No arr_reg to pop since we used target directly */
      }
      else {
        /* General case: need to call deconstruct and check size at runtime */
        arr_reg = cursp();

        /* Call deconstruct on target */
        gen_move(s, cursp(), target, 0);
        push();
        genop_3(s, OP_SEND, arr_reg, sym_idx(s, MRB_SYM(deconstruct)), 0);

        /* Check length constraints */
        if (pat_arr->rest == 0) {
          /* No rest: exact length match */
          /* Generate: arr.size == pre_len using EQ opcode */
          gen_move(s, cursp(), arr_reg, 0);
          push();
          genop_3(s, OP_SEND, cursp() - 1, sym_idx(s, MRB_SYM(size)), 0);
          gen_int(s, cursp(), pre_len);
          /* EQ: R[a] = R[a] == R[a+1]; size at cursp-1, pre_len at cursp */
          genop_1(s, OP_EQ, cursp() - 1);
          tmp = genjmp2(s, OP_JMPNOT, cursp() - 1, *fail_pos, 1);
          *fail_pos = tmp;
          pop();
        }
        else {
          /* Has rest: minimum length check */
          int min_len = pre_len + post_len;
          if (min_len > 0) {
            /* Generate: arr.size >= min_len using GE opcode */
            gen_move(s, cursp(), arr_reg, 0);
            push();
            genop_3(s, OP_SEND, cursp() - 1, sym_idx(s, MRB_SYM(size)), 0);
            gen_int(s, cursp(), min_len);
            /* GE: R[a] = R[a] >= R[a+1]; size at cursp-1, min_len at cursp */
            genop_1(s, OP_GE, cursp() - 1);
            tmp = genjmp2(s, OP_JMPNOT, cursp() - 1, *fail_pos, 1);
            *fail_pos = tmp;
            pop();
          }
        }

        /* Match pre-rest elements */
        i = 0;
        for (elem = pat_arr->pre; elem; elem = elem->cdr, i++) {
          /* Get arr[i] */
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), i);
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
          push();  /* Preserve element result for codegen_pattern */
          /* Match element pattern */
          codegen_pattern(s, elem->car, cursp() - 1, fail_pos, -1);
          pop();  /* Clean up element slot */
        }

        /* Bind rest elements if rest is a variable */
        if (pat_arr->rest && pat_arr->rest != (node*)-1) {
          struct mrb_ast_pat_var_node *rest_var = pat_var_node(pat_arr->rest);
          if (rest_var->name) {
            int var_idx = lv_idx(s, rest_var->name);
            /* Generate: arr[pre_len..-(post_len+1)] or arr[pre_len..-1] if no post */
            gen_move(s, cursp(), arr_reg, 0);  /* arr at cursp */
            push();
            gen_int(s, cursp(), pre_len);      /* start at cursp */
            push();
            if (post_len > 0) {
              gen_int(s, cursp(), -(post_len + 1));  /* end at cursp */
            }
            else {
              gen_int(s, cursp(), -1);         /* end at cursp */
            }
            /* start at cursp-1, end at cursp; create inclusive range at cursp-1 */
            genop_1(s, OP_RANGE_INC, cursp() - 1);
            /* arr at cursp-2, range at cursp-1 */
            pop();  /* cursp now at range position */
            pop();  /* cursp now at arr position */
            genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
            if (var_idx > 0) {
              gen_move(s, var_idx, cursp(), 1);
            }
          }
        }

        /* Match post-rest elements */
        i = -post_len;
        for (elem = pat_arr->post; elem; elem = elem->cdr, i++) {
          /* Get arr[i] (negative index from end) */
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), i);
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
          push();  /* Preserve element result for codegen_pattern */
          /* Match element pattern */
          codegen_pattern(s, elem->car, cursp() - 1, fail_pos, -1);
          pop();  /* Clean up element slot */
        }

        pop();  /* Pop arr_reg */
      }
    }
    break;

  case NODE_PAT_FIND:
    {
      /* Find pattern: [*pre, elem1, elem2, ..., *post]
       * Searches for elems anywhere in the array.
       *
       * Stack layout:
       *   arr_reg: deconstructed array (stable)
       *   idx_reg: current search index (stable)
       *
       * Loop bound is recomputed each iteration since OP_SEND clobbers registers.
       */
      struct mrb_ast_pat_find_node *pat_find = pat_find_node(pattern);
      int elems_len = 0;
      node *elem;
      int arr_reg = cursp();
      int idx_reg;
      uint32_t loop_start, match_fail, loop_end;

      /* Count middle elements */
      for (elem = pat_find->elems; elem; elem = elem->cdr) elems_len++;

      /* Call deconstruct on target */
      gen_move(s, cursp(), target, 0);
      push();
      genop_3(s, OP_SEND, arr_reg, sym_idx(s, MRB_SYM(deconstruct)), 0);

      /* Check minimum length: arr.size >= elems_len */
      gen_move(s, cursp(), arr_reg, 0);
      push();
      genop_3(s, OP_SEND, cursp() - 1, sym_idx(s, MRB_SYM(size)), 0);
      gen_int(s, cursp(), elems_len);
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(ge)), 1);
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;

      /* Initialize index to 0 */
      idx_reg = cursp();
      gen_int(s, idx_reg, 0);
      push();

      /* Loop: try matching at each position */
      loop_start = s->pc;
      match_fail = JMPLINK_START;

      /* Check if idx <= arr.size - elems_len (i.e., idx < arr.size - elems_len + 1) */
      /* Compute: arr.size - elems_len */
      gen_move(s, cursp(), arr_reg, 0);
      push();
      genop_3(s, OP_SEND, cursp() - 1, sym_idx(s, MRB_SYM(size)), 0);
      gen_int(s, cursp(), elems_len);
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(sub)), 1);
      /* Now cursp() has (size - elems_len), compare: idx <= (size - elems_len) */
      gen_move(s, cursp() + 1, idx_reg, 0);
      push();
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(ge)), 1);
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;

      /* Try to match each middle element at idx+offset */
      int offset = 0;
      for (elem = pat_find->elems; elem; elem = elem->cdr, offset++) {
        /* Get arr[idx + offset] */
        gen_move(s, cursp(), arr_reg, 0);
        push();
        if (offset == 0) {
          gen_move(s, cursp(), idx_reg, 0);
        }
        else {
          gen_move(s, cursp(), idx_reg, 0);
          push();
          gen_int(s, cursp(), offset);
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(add)), 1);
        }
        push(); push(); pop(); pop(); pop();
        genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
        push();  /* Preserve element result for codegen_pattern */
        /* Match element pattern - on fail, try next index */
        codegen_pattern(s, elem->car, cursp() - 1, &match_fail, -1);
        pop();  /* Clean up element slot */
      }

      /* All elements matched - bind pre and post if named */
      if (pat_find->pre && pat_find->pre != (node*)-1) {
        struct mrb_ast_pat_var_node *pre_var = pat_var_node(pat_find->pre);
        if (pre_var->name) {
          int var_idx = lv_idx(s, pre_var->name);
          /* pre = arr[0...idx] (exclusive range) */
          /* Following the NODE_PAT_ARRAY pattern exactly */
          gen_move(s, cursp(), arr_reg, 0);  /* arr at cursp */
          push();
          gen_int(s, cursp(), 0);            /* start=0 at cursp */
          push();
          gen_move(s, cursp(), idx_reg, 0);  /* end=idx at cursp */
          /* start at cursp-1, end at cursp; create exclusive range at cursp-1 */
          genop_1(s, OP_RANGE_EXC, cursp() - 1);
          /* arr at cursp-2, range at cursp-1 */
          pop();  /* cursp now at range position */
          pop();  /* cursp now at arr position */
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
          if (var_idx > 0) {
            gen_move(s, var_idx, cursp(), 1);
          }
        }
      }

      if (pat_find->post && pat_find->post != (node*)-1) {
        struct mrb_ast_pat_var_node *post_var = pat_var_node(pat_find->post);
        if (post_var->name) {
          int var_idx = lv_idx(s, post_var->name);
          /* post = arr[(idx+elems_len)..-1] (inclusive range) */
          /* Following the NODE_PAT_ARRAY pattern exactly */
          gen_move(s, cursp(), arr_reg, 0);  /* arr at cursp */
          push();
          /* Compute idx + elems_len for start index */
          gen_move(s, cursp(), idx_reg, 0);  /* idx at cursp */
          push();
          gen_int(s, cursp(), elems_len);    /* elems_len at cursp */
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(add)), 1);
          /* start index (idx+elems_len) now at cursp */
          push();
          gen_int(s, cursp(), -1);           /* end=-1 at cursp */
          /* start at cursp-1, end at cursp; create inclusive range at cursp-1 */
          genop_1(s, OP_RANGE_INC, cursp() - 1);
          /* arr at cursp-2, range at cursp-1 */
          pop();  /* cursp now at range position */
          pop();  /* cursp now at arr position */
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
          if (var_idx > 0) {
            gen_move(s, var_idx, cursp(), 1);
          }
        }
      }

      /* Jump to success (end of find pattern) */
      loop_end = genjmp(s, OP_JMP, JMPLINK_START);

      /* Match failed - increment index and try again */
      dispatch_linked(s, match_fail);
      gen_move(s, cursp(), idx_reg, 0);
      push();
      gen_int(s, cursp(), 1);
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(add)), 1);
      gen_move(s, idx_reg, cursp(), 0);
      genjmp(s, OP_JMP, loop_start);

      /* Success exit point */
      dispatch(s, loop_end);

      pop();  /* idx_reg */
      pop();  /* arr_reg */
    }
    break;

  case NODE_PAT_HASH:
    {
      struct mrb_ast_pat_hash_node *pat_hash = pat_hash_node(pattern);
      int hash_reg = cursp();
      node *pair;
      int num_keys = 0;

      /* Count keys */
      for (pair = pat_hash->pairs; pair; pair = pair->cdr) num_keys++;

      /* Call deconstruct_keys.
       * Pass nil when all keys are needed (rest or exact match).
       * Pass keys array only for partial match (optimization for custom classes). */
      gen_move(s, cursp(), target, 0);
      push();
      if (pat_hash->rest == NULL && num_keys > 0) {
        /* Partial match: pass keys array */
        gen_pat_keys_ary(s, pat_hash->pairs, num_keys);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      genop_3(s, OP_SEND, hash_reg, sym_idx(s, MRB_SYM(deconstruct_keys)), 1);
      pop();

      /* Check all keys exist and get values via __pat_values */
      if (num_keys > 0) {
        int vals_reg = cursp();
        gen_move(s, vals_reg, hash_reg, 0);
        push();
        gen_pat_keys_ary(s, pat_hash->pairs, num_keys);
        genop_3(s, OP_SEND, vals_reg, sym_idx(s, MRB_SYM(__pat_values)), 1);
        pop();  /* keys_ary */
        /* vals_reg = values array or false; fail if false */
        tmp = genjmp2(s, OP_JMPNOT, vals_reg, *fail_pos, 1);
        *fail_pos = tmp;

        /* Match each value against its pattern */
        int i = 0;
        for (pair = pat_hash->pairs; pair; pair = pair->cdr, i++) {
          node *pat = pair->car->cdr;

          gen_move(s, cursp(), vals_reg, 0);
          push();
          gen_int(s, cursp(), i);
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_OPSYM(aref)), 1);
          push();

          codegen_pattern(s, pat, cursp() - 1, fail_pos, -1);
          pop();
        }
        pop();  /* vals_reg */
      }

      /* Handle rest pattern */
      if (pat_hash->rest == (node*)-1 || (num_keys == 0 && pat_hash->rest == NULL)) {
        /* **nil or empty {}: exact match - verify hash.size == num_keys */
        gen_move(s, cursp(), hash_reg, 0);
        push();
        genop_3(s, OP_SEND, cursp() - 1, sym_idx(s, MRB_SYM(size)), 0);
        gen_int(s, cursp(), num_keys);
        genop_1(s, OP_EQ, cursp() - 1);
        tmp = genjmp2(s, OP_JMPNOT, cursp() - 1, *fail_pos, 1);
        *fail_pos = tmp;
        pop();
      }
      else if (pat_hash->rest && pat_hash->rest != (node*)-2) {
        /* **var: capture remaining keys via hash.__except(keys_array) */
        struct mrb_ast_pat_var_node *rest_var = pat_var_node(pat_hash->rest);
        if (rest_var->name) {
          int var_idx = lv_idx(s, rest_var->name);
          int recv = cursp();
          gen_move(s, recv, hash_reg, 0);
          push();
          if (num_keys > 0) {
            gen_pat_keys_ary(s, pat_hash->pairs, num_keys);
            genop_3(s, OP_SEND, recv, sym_idx(s, MRB_SYM(__except)), 1);
            pop();
          }
          else {
            genop_3(s, OP_SEND, recv, sym_idx(s, MRB_SYM(dup)), 0);
          }
          if (var_idx > 0) {
            gen_move(s, var_idx, recv, 1);
          }
          pop();
        }
      }

      pop();  /* hash_reg */
    }
    break;

  default:
    raise_error(s, "unsupported pattern type");
    break;
  }
}

/* Definition node codegen functions */

static void
codegen_def(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_def_node *def_n = def_node(varnode);
  int sym = sym_idx(s, def_n->name);

  /* Call lambda_body directly with individual parameters */
  /* For NODE_DEF, args should contain the full locals structure from defn_setup */
  int idx = lambda_body(s, def_n->locals, def_n->args, def_n->body, 0);

  if (idx <= 0xff) {
    /* TDEF fusion: TCLASS + METHOD + DEF -> TDEF */
    genop_3(s, OP_TDEF, cursp(), sym, idx);
  }
  else {
    genop_1(s, OP_TCLASS, cursp());
    push();
    genop_2(s, OP_METHOD, cursp(), idx);
    push(); pop();
    pop();
    genop_2(s, OP_DEF, cursp(), sym);
  }
  if (val) push();
}

/* Helper function for generating class/module/singleton class body */
/* Forward declaration */
static mrb_bool is_empty_stmts(node *stmt_node);

static void
gen_class_body(codegen_scope *s, node *body, int val)
{
  int idx;

  if (body && body->cdr) {
    /* Extract locals and body from the cons structure: (locals . body) */
    node *locals = body->car;
    node *body_stmts = body->cdr;

    /* Check for empty body case */
    if (is_empty_stmts(body_stmts)) {
      genop_1(s, OP_LOADNIL, cursp());
    }
    else {
      /* Generate proper scope with locals and body */
      idx = scope_body(s, locals, body_stmts, val);
      genop_2(s, OP_EXEC, cursp(), idx);
    }
  }
  else {
    /* No body - load nil */
    genop_1(s, OP_LOADNIL, cursp());
  }
}

/* Helper function for generating namespace/parent for class/module */
static void
gen_namespace(codegen_scope *s, node *name)
{
  if (name->car == (node*)0) {
    genop_1(s, OP_LOADNIL, cursp());
    push();
  }
  else if (name->car == (node*)1) {
    genop_1(s, OP_OCLASS, cursp());
    push();
  }
  else {
    codegen(s, name->car, VAL);
  }
}

static void
codegen_class(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_class_node *class_n = class_node(varnode);
  node *name = class_n->name;
  node *superclass = class_n->superclass;
  node *body = class_n->body;
  int idx;

  /* Handle class namespace */
  gen_namespace(s, name);

  /* Handle superclass */
  if (superclass) {
    codegen(s, superclass, VAL);
  }
  else {
    genop_1(s, OP_LOADNIL, cursp());
    push();
  }

  pop(); pop();

  /* Create class with name symbol */
  idx = sym_idx(s, node_to_sym(name->cdr));
  genop_2(s, OP_CLASS, cursp(), idx);

  /* Generate class body */
  gen_class_body(s, body, val);

  if (val) {
    push();
  }
}

static void
codegen_module(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_module_node *module_n = module_node(varnode);
  node *name = module_n->name;
  node *body = module_n->body;
  int idx;

  /* Handle module namespace */
  gen_namespace(s, name);
  pop();

  /* Create module with name symbol */
  idx = sym_idx(s, node_to_sym(name->cdr));
  genop_2(s, OP_MODULE, cursp(), idx);

  /* Generate module body */
  gen_class_body(s, body, val);

  if (val) {
    push();
  }
}

static void
codegen_sclass(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_sclass_node *sclass_n = sclass_node(varnode);
  node *obj = sclass_n->obj;
  node *body = sclass_n->body;

  /* Generate code for the singleton object */
  codegen(s, obj, VAL);
  pop();

  /* Enter singleton class scope */
  genop_1(s, OP_SCLASS, cursp());

  /* Generate singleton class body */
  gen_class_body(s, body, val);

  if (val) {
    push();
  }
}

/* Variable-sized assignment codegen functions */
static void
codegen_asgn(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_asgn_node *asgn_n = asgn_node(varnode);
  node *lhs = asgn_n->lhs;
  node *rhs = asgn_n->rhs;

  gen_assignment(s, lhs, rhs, 0, val);
}

static void
codegen_masgn(codegen_scope *s, node *varnode, node *rhs, int sp, int val)
{
  struct mrb_ast_masgn_node *masgn_n = (struct mrb_ast_masgn_node*)varnode;

  /* If called from codegen_variable_node context, use the embedded rhs */
  if (!rhs && sp == 0) {
    rhs = masgn_n->rhs;
    sp = 0;  /* Use register 0 as base for standalone assignment */
  }

  int len = 0, n = 0, post = 0;
  node *t = rhs ? rhs : masgn_n->rhs, *p;
  int rhs_reg = sp;

  if (!val && t && node_type(t) == NODE_ARRAY) {
    struct mrb_ast_array_node *an = array_node(t);
    if (an->elements && nosplat(an->elements)) {
      /* fixed rhs */
      t = an->elements;

      /* Optimization: direct generation for simple cases */
      /* When all lhs are local vars and all rhs are simple literals, */
      /* generate directly into target registers (no temporaries) */
      if (masgn_n->pre && !masgn_n->rest && !masgn_n->post) {
        int regs[16];  /* support up to 16 variables */
        node *lhs = masgn_n->pre;
        node *rhs_elem = t;
        int rhs_count = 0, lhs_count = 0;
        mrb_bool all_simple = TRUE;

        /* Count lhs variables */
        while (lhs && lhs_count < 16) {
          lhs_count++;
          lhs = lhs->cdr;
        }

        /* Count and check rhs are all simple literals */
        while (rhs_elem && rhs_count < 16) {
          if (!is_simple_literal(rhs_elem->car)) {
            all_simple = FALSE;
            break;
          }
          rhs_count++;
          rhs_elem = rhs_elem->cdr;
        }
        /* Only apply when lhs and rhs counts match exactly */
        lhs = masgn_n->pre;
        if (all_simple && lhs_count > 0 && lhs_count == rhs_count &&
            all_lvar_pre(s, lhs, regs, lhs_count)) {
          /* Direct generation: generate literals into target registers */
          rhs_elem = t;
          for (int i = 0; i < lhs_count; i++) {
            gen_literal_to_reg(s, rhs_elem->car, regs[i]);
            rhs_elem = rhs_elem->cdr;
          }
          return;
        }
      }

      rhs_reg = cursp();  /* Save register where values will be pushed */
      while (t) {
        codegen(s, t->car, VAL);
        len++;
        t = t->cdr;
      }
      if (masgn_n->pre) {                /* pre */
        t = masgn_n->pre;
        n = 0;
        while (t) {
          if (n < len) {
            gen_assignment(s, t->car, NULL, rhs_reg+n, NOVAL);
            n++;
          }
          else {
            genop_1(s, OP_LOADNIL, rhs_reg+n);
            gen_assignment(s, t->car, NULL, rhs_reg+n, NOVAL);
          }
          t = t->cdr;
        }
      }
      /* Count post variables */
      if (masgn_n->post) {
        p = masgn_n->post;
        while (p) {
          post++;
          p = p->cdr;
        }
      }
      /* Handle rest variable */
      if (masgn_n->rest && (intptr_t)masgn_n->rest != -1) {
            int rn;

            if (len < post + n) {
              rn = 0;
            }
            else {
              rn = len - post - n;
            }
            if (cursp() == rhs_reg+n) {
              genop_2(s, OP_ARRAY, cursp(), rn);
            }
            else {
              genop_3(s, OP_ARRAY2, cursp(), rhs_reg+n, rn);
            }
            gen_assignment(s, masgn_n->rest, NULL, cursp(), NOVAL);
            n += rn;
      }
      /* Handle post variables */
      if (masgn_n->post) {
        t = masgn_n->post;
        while (t) {
          if (n<len) {
            gen_assignment(s, t->car, NULL, rhs_reg+n, NOVAL);
          }
          else {
            genop_1(s, OP_LOADNIL, cursp());
            gen_assignment(s, t->car, NULL, cursp(), NOVAL);
          }
          t = t->cdr;
          n++;
        }
      }
      pop_n(len);
      return;
    }
  }

  {
    /* variable rhs - implement gen_massignment logic directly for variable-sized nodes */

    /* Check if this is parameter destructuring (called from lambda_body) */
    if (!rhs && sp > 0) {
      /* Parameter destructuring: value is already in register sp */
      rhs_reg = sp;
    }
    else if (t) {
      codegen(s, t, VAL);
      rhs_reg = cursp() - 1;  /* rhs is now at cursp()-1 */
    }
    else {
      /* No rhs and no sp value - should not happen in normal cases */
      return;
    }

    /* Handle the lhs structure directly */
    n = 0;
    post = 0;

    if (masgn_n->pre) {              /* pre */
      node *pre = masgn_n->pre;
      n = 0;
      while (pre) {
        int sp = cursp();
        genop_3(s, OP_AREF, sp, rhs_reg, n);
        push();
        gen_assignment(s, pre->car, NULL, sp, NOVAL);
        pop();
        n++;
        pre = pre->cdr;
      }
    }

    /* Count post variables */
    if (masgn_n->post) {
      node *p = masgn_n->post;
      while (p) {
        post++;
        p = p->cdr;
      }
    }

    /* Only generate APOST if there's rest or post variables */
    if ((masgn_n->rest && (intptr_t)masgn_n->rest != -1) || masgn_n->post) {
      gen_move(s, cursp(), rhs_reg, val);
      push_n(post+1);
      pop_n(post+1);
      genop_3(s, OP_APOST, cursp(), n, post);
      int nn = 1;
      if (masgn_n->rest && (intptr_t)masgn_n->rest != -1) { /* rest */
        gen_assignment(s, masgn_n->rest, NULL, cursp(), NOVAL);
      }
      if (masgn_n->post) {
        node *post_part = masgn_n->post;
        while (post_part) {
          gen_assignment(s, post_part->car, NULL, cursp()+nn, NOVAL);
          post_part = post_part->cdr;
          nn++;
        }
      }
    }

    if (!val && t) {
      pop();  /* pop the rhs value */
    }
  }
}

static void
codegen_op_asgn(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_op_asgn_node *op_asgn_n = op_asgn_node(varnode);
  node *lhs = op_asgn_n->lhs;
  node *rhs = op_asgn_n->rhs;
  mrb_sym sym = op_asgn_n->op;
  mrb_int len;
  const char *name = mrb_sym_name_len(s->mrb, sym, &len);
  int vsp = -1;

  /* Handle ||= and &&= operators */
  if (len == 2 &&
      ((name[0] == '|' && name[1] == '|') ||
       (name[0] == '&' && name[1] == '&'))) {
    uint32_t pos;
    enum node_type lhs_type = node_type(lhs);

    /* For ||= on class variables and constants, wrap read in exception handling */
    if (name[0] == '|' && (lhs_type == NODE_CVAR || lhs_type == NODE_CONST)) {
      int catch_entry, begin, end;
      int noexc, exc;
      struct loopinfo *lp;

      lp = loop_push(s, LOOP_BEGIN);
      lp->pc0 = new_label(s);
      catch_entry = catch_handler_new(s);
      begin = s->pc;
      exc = cursp();
      codegen(s, lhs, VAL);
      end = s->pc;
      noexc = genjmp_0(s, OP_JMP);
      lp->type = LOOP_RESCUE;
      catch_handler_set(s, catch_entry, MRB_CATCH_RESCUE, begin, end, s->pc);
      genop_1(s, OP_EXCEPT, exc);
      genop_1(s, OP_LOADFALSE, exc);
      dispatch(s, noexc);
      loop_pop(s, NOVAL);
    }
    else {
      /* Generate code to get current value of LHS */
      codegen(s, lhs, VAL);
    }

    pop();
    if (val) {
      if (vsp >= 0) {
        gen_move(s, vsp, cursp(), 1);
      }
      pos = genjmp2_0(s, name[0]=='|'?OP_JMPIF:OP_JMPNOT, cursp(), val);
    }
    else {
      pos = genjmp2_0(s, name[0]=='|'?OP_JMPIF:OP_JMPNOT, cursp(), val);
    }
    codegen(s, rhs, VAL);
    pop();
    if (val && vsp >= 0) {
      gen_move(s, vsp, cursp(), 1);
    }
    gen_assignment(s, lhs, NULL, cursp(), val);
    dispatch(s, pos);
    return;
  }

  /* For other operators, generate: lhs = lhs op rhs */
  codegen(s, lhs, VAL);
  codegen(s, rhs, VAL);
  push(); pop();
  pop(); pop();

  /* Apply the operator */
  if (len == 1 && name[0] == '+')  {
    gen_addsub(s, OP_ADD, cursp());
  }
  else if (len == 1 && name[0] == '-')  {
    gen_addsub(s, OP_SUB, cursp());
  }
  else if (len == 1 && name[0] == '*')  {
    genop_1(s, OP_MUL, cursp());
  }
  else if (len == 1 && name[0] == '/')  {
    genop_1(s, OP_DIV, cursp());
  }
  else if (len == 1 && name[0] == '<')  {
    genop_1(s, OP_LT, cursp());
  }
  else if (len == 2 && name[0] == '<' && name[1] == '=')  {
    genop_1(s, OP_LE, cursp());
  }
  else if (len == 1 && name[0] == '>')  {
    genop_1(s, OP_GT, cursp());
  }
  else if (len == 2 && name[0] == '>' && name[1] == '=')  {
    genop_1(s, OP_GE, cursp());
  }
  else {
    int idx = sym_idx(s, sym);
    genop_3(s, OP_SEND, cursp(), idx, 1);
  }

  /* Assign the result back to LHS */
  gen_assignment(s, lhs, NULL, cursp(), val);
}

/* Variable-sized expression codegen functions */
static void
codegen_and(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_and_node *and_n = (struct mrb_ast_and_node*)varnode;
  node *left = and_n->left;
  node *right = and_n->right;
  uint32_t pos;

  if (true_always(left)) {
    codegen(s, right, val);
    return;
  }
  if (false_always(left)) {
    codegen(s, left, val);
    return;
  }
  codegen(s, left, VAL);
  pop();
  pos = genjmp2_0(s, OP_JMPNOT, cursp(), val);
  codegen(s, right, val);
  dispatch(s, pos);
}

static void
codegen_or(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_or_node *or_n = (struct mrb_ast_or_node*)varnode;
  node *left = or_n->left;
  node *right = or_n->right;
  uint32_t pos;

  if (true_always(left)) {
    codegen(s, left, val);
    return;
  }
  if (false_always(left)) {
    codegen(s, right, val);
    return;
  }
  codegen(s, left, VAL);
  pop();
  pos = genjmp2_0(s, OP_JMPIF, cursp(), val);
  codegen(s, right, val);
  dispatch(s, pos);
}

static void
codegen_return(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_return_node *return_n = return_node(varnode);
  node *args = return_n->args;

  if (args) {
    gen_retval(s, args);
  }
  else {
    genop_1(s, OP_LOADNIL, cursp());
  }
  if (s->loop) {
    gen_return(s, OP_RETURN_BLK, cursp());
  }
  else {
    gen_return(s, OP_RETURN, cursp());
  }
  if (!val) return;
  push();
}

static void
codegen_yield(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_yield_node *yield_n = yield_node(varnode);
  node *args = yield_n->args;
  codegen_scope *s2 = s;
  int lv = 0, ainfo = -1;
  int n = 0, nk = 0, sendv = 0;

  while (!s2->mscope) {
    lv++;
    s2 = s2->prev;
    if (!s2) break;
  }
  if (s2) {
    ainfo = (int)s2->ainfo;
  }
  if (ainfo < 0) codegen_error(s, "invalid yield (SyntaxError)");
  if (lv > 0xf) codegen_error(s, "too deep nesting");
  push();
  if (args) {
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)args;
    if (callargs->regular_args) {
      n = gen_values(s, callargs->regular_args, VAL, 14);
      if (n < 0) {
        n = sendv = 1;
        push();
      }
    }
    if (callargs->keyword_args) {
      nk = gen_hash(s, callargs->keyword_args, VAL, 14);
      if (nk < 0) {
        nk = 15;
      }
    }
  }
  push();pop(); /* space for a block */
  pop_n(n + (nk == 15 ? 1 : nk * 2) + 1);
  genop_2S(s, OP_BLKPUSH, cursp(), (ainfo<<4)|(lv & 0xf));
  if (sendv) n = CALL_MAXARGS;
  if (nk == 0 && n < 15) {
    /* fast path: direct block call without method dispatch */
    genop_2(s, OP_BLKCALL, cursp(), n);
  }
  else {
    /* fallback: use SEND for keyword args or splat */
    genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_SYM(call)), n|(nk<<4));
  }
  if (val) push();
}

static void
codegen_super(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_super_node *super_n = super_node(varnode);
  node *tree = super_n->args;

  codegen_scope *s2 = s;
  int lv = 0;
  int n = 0, nk = 0, st = 0;

  push();
  while (!s2->mscope) {
    lv++;
    s2 = s2->prev;
    if (!s2) break;
  }
  if (tree) {
    /* Handle callargs structure - direct casting like new_args() */
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)tree;

      /* Regular arguments */
      if (callargs->regular_args) {
        st = n = gen_values(s, callargs->regular_args, VAL, 14);
        if (n < 0) {
          st = 1; n = 15;
          push();
        }
      }

      /* Keyword arguments */
      if (callargs->keyword_args) {
        nk = gen_hash(s, callargs->keyword_args, VAL, 14);
        if (nk < 0) {st++; nk = 15;}
        else st += nk*2;
        n |= nk<<4;
      }

      /* Block arguments */
      if (callargs->block_arg) {
        codegen(s, callargs->block_arg, VAL);
      }
      else if (s2) gen_blkmove(s, s2->ainfo, lv);
      else {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
    }
  else {
    if (s2) gen_blkmove(s, s2->ainfo, lv);
    else {
      genop_1(s, OP_LOADNIL, cursp());
      push();
    }
  }
  st++;
  pop_n(st+1);
  genop_2(s, OP_SUPER, cursp(), n);
  if (val) push();
}

/* Variable-sized literal node generation functions */
static void
codegen_str(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_str_node *str_n = str_node(varnode);
  node *list = str_n->list;

  /* Use common cons list string codegen */
  gen_string(s, list, val);
}

static void
codegen_dot2(codegen_scope *s, node *varnode, int val)
{
  node *left = dot2_node(varnode)->left;
  node *right = dot2_node(varnode)->right;

  codegen(s, left, val);
  codegen(s, right, val);
  if (!val) return;
  pop(); pop();
  genop_1(s, OP_RANGE_INC, cursp());
  push();
}

static void
codegen_dot3(codegen_scope *s, node *varnode, int val)
{
  node *left = dot3_node(varnode)->left;
  node *right = dot3_node(varnode)->right;

  codegen(s, left, val);
  codegen(s, right, val);
  if (!val) return;
  pop(); pop();
  genop_1(s, OP_RANGE_EXC, cursp());
  push();
}

static void
codegen_float(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_float_node *float_n = (struct mrb_ast_float_node*)varnode;
  const char *value = float_n->value;
  double f;

  mrb_read_float(value, NULL, &f);
  int off = new_lit_float(s, (mrb_float)f);

  gen_load_op2(s, OP_LOADL, off, val);
}

/* Variable-sized simple node generation functions */
static void
codegen_self(codegen_scope *s, node *varnode, int val)
{
  /* Use traditional self codegen logic */
  gen_load_op1(s, OP_LOADSELF, val);
}

static void
codegen_nil(codegen_scope *s, node *varnode, int val)
{
  /* Use traditional nil codegen logic */
  gen_load_op1(s, OP_LOADNIL, val);
}

static void
codegen_true(codegen_scope *s, node *varnode, int val)
{
  /* Generate OP_LOADTRUE instruction for true literal */
  gen_load_op1(s, OP_LOADTRUE, val);
}

static void
codegen_false(codegen_scope *s, node *varnode, int val)
{
  /* Generate OP_LOADFALSE instruction for false literal */
  gen_load_op1(s, OP_LOADFALSE, val);
}

static void
codegen_const(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_const_node *const_n = const_node(varnode);
  mrb_sym symbol = const_n->symbol;

  int i = sym_idx(s, symbol);
  genop_2(s, OP_GETCONST, cursp(), i);
  if (val) push();
}

static void
codegen_rescue(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_rescue_node *rescue = rescue_node(varnode);
  node *body = rescue->body;
  node *rescue_clauses = rescue->rescue_clauses;
  node *else_clause = rescue->else_clause;

  int noexc;
  uint32_t exend, pos1, pos2, tmp;
  struct loopinfo *lp;
  int catch_entry, begin, end;

  if (body == NULL) return;
  lp = loop_push(s, LOOP_BEGIN);
  lp->pc0 = new_label(s);
  catch_entry = catch_handler_new(s);
  begin = s->pc;
  codegen(s, body, VAL);
  pop();
  lp->type = LOOP_RESCUE;
  end = s->pc;
  noexc = genjmp_0(s, OP_JMP);
  catch_handler_set(s, catch_entry, MRB_CATCH_RESCUE, begin, end, s->pc);
  exend = JMPLINK_START;
  pos1 = JMPLINK_START;
  if (rescue_clauses) {
    node *n2 = rescue_clauses;
    int exc = cursp();

    genop_1(s, OP_EXCEPT, exc);
    push();
    while (n2) {
      node *n3 = n2->car;
      node *n4 = n3->car;

      dispatch(s, pos1);
      pos2 = JMPLINK_START;
      do {
        if (n4 && n4->car && is_splat_node(n4->car)) {
          codegen(s, n4->car, VAL);
          gen_move(s, cursp(), exc, 0);
          push_n(2); pop_n(2); /* space for one arg and a block */
          pop();
          genop_3(s, OP_SEND, cursp(), sym_idx(s, MRB_SYM(__case_eqq)), 1);
        }
        else {
          if (n4) {
            codegen(s, n4->car, VAL);
          }
          else {
            genop_2(s, OP_GETCONST, cursp(), sym_idx(s, MRB_SYM(StandardError)));
            push();
          }
          pop();
          genop_2(s, OP_RESCUE, exc, cursp());
        }
        tmp = genjmp2(s, OP_JMPIF, cursp(), pos2, val);
        pos2 = tmp;
        if (n4) {
          n4 = n4->cdr;
        }
      } while (n4);
      pos1 = genjmp_0(s, OP_JMP);
      dispatch_linked(s, pos2);

      pop();
      if (n3->cdr->car) {
        gen_assignment(s, n3->cdr->car, NULL, exc, NOVAL);
      }
      if (n3->cdr->cdr->car) {
        codegen(s, n3->cdr->cdr->car, val);
        if (val) pop();
      }
      tmp = genjmp(s, OP_JMP, exend);
      exend = tmp;
      n2 = n2->cdr;
      push();
    }
    if (pos1 != JMPLINK_START) {
      dispatch(s, pos1);
      genop_1(s, OP_RAISEIF, exc);
    }
  }
  pop();
  dispatch(s, noexc);
  if (else_clause) {
    codegen(s, else_clause, val);
  }
  else if (val) {
    push();
  }
  dispatch_linked(s, exend);
  loop_pop(s, NOVAL);
}

static void
codegen_block(codegen_scope *s, node *varnode, int val)
{
  if (!val) return;

  struct mrb_ast_block_node *n = block_node(varnode);

  /* Call lambda_body directly with individual parameters */
  int idx = lambda_body(s, n->locals, n->args, n->body, 1);
  genop_2(s, OP_BLOCK, cursp(), idx);
  push();
}

static void
codegen_break(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_break_node *n = (struct mrb_ast_break_node*)varnode;
  loop_break(s, n->value);
  if (!val) return;
  push();
}

static void
codegen_next(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_next_node *n = (struct mrb_ast_next_node*)varnode;
  if (!s->loop) {
    raise_error(s, "unexpected next");
  }
  else if (s->loop->type == LOOP_NORMAL) {
    codegen(s, n->value, NOVAL);
    genjmp(s, OP_JMPUW, s->loop->pc0);
  }
  else {
    if (n->value) {
      codegen(s, n->value, VAL);
      pop();
    }
    else {
      genop_1(s, OP_LOADNIL, cursp());
    }
    gen_return(s, OP_RETURN, cursp());
  }
  if (!val) return;
  push();
}

static void
codegen_redo(codegen_scope *s, node *varnode, int val)
{
  for (const struct loopinfo *lp = s->loop; ; lp = lp->prev) {
    if (!lp) {
      raise_error(s, "unexpected redo");
      break;
    }
    if (lp->type != LOOP_BEGIN && lp->type != LOOP_RESCUE) {
      genjmp(s, OP_JMPUW, lp->pc1);
      break;
    }
  }
  if (!val) return;
  push();
}

static void
codegen_retry(codegen_scope *s, node *varnode, int val)
{
  const struct loopinfo *lp = s->loop;

  while (lp && lp->type != LOOP_RESCUE) {
    lp = lp->prev;
  }
  if (!lp) {
    raise_error(s, "unexpected retry");
  }
  else {
    genjmp(s, OP_JMPUW, lp->pc0);
  }
  if (!val) return;
  push();
}

static void
codegen_xstr(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_xstr_node *n = xstr_node(varnode);
  node *list = n->list;
  int sym;

  /* Always execute backtick command for side effects, even in NOVAL mode */
  push();
  /* Generate string using common function */
  gen_string(s, list, VAL);

  push();                   /* for block */
  pop_n(3);
  sym = sym_idx(s, MRB_OPSYM(tick)); /* ` */
  genop_3(s, OP_SSEND, cursp(), sym, 1);

  if (val) {
    push(); /* Keep result on stack if needed */
  }
  /* If val=0, the result is discarded but the method was still called */
}

static void
codegen_regx(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_regx_node *n = regx_node(varnode);

  if (val) {
    int sym = sym_idx(s, mrb_intern_lit(s->mrb, REGEXP_CLASS));
    int argc = 1;
    int off;

    genop_1(s, OP_OCLASS, cursp());
    genop_2(s, OP_GETMCNST, cursp(), sym);
    push();

    /* Generate regex pattern using common cons list function */
    gen_string(s, n->list, VAL);

    /* Add flags and/or encoding if present */
    if ((n->flags && *n->flags) || (n->encoding && *n->encoding)) {
      /* Add flags (or nil if not present but encoding is) */
      if (n->flags && *n->flags) {
        off = new_lit_cstr(s, n->flags);
        genop_2(s, OP_STRING, cursp(), off);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
      }
      push();
      argc++;

      /* Add encoding if present */
      if (n->encoding && *n->encoding) {
        off = new_lit_cstr(s, n->encoding);
        genop_2(s, OP_STRING, cursp(), off);
        push();
        argc++;
      }
    }

    push(); /* space for a block */
    pop_n(argc+2);
    sym = sym_idx(s, MRB_SYM(compile));
    genop_3(s, OP_SEND, cursp(), sym, argc);
    push();
  }
  else {
    /* NOVAL case: still need to evaluate expressions for side effects */
    gen_string(s, n->list, NOVAL);
  }
}

static void
codegen_heredoc(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_heredoc_node *n = heredoc_node(varnode);
  // Process heredoc doc field as cons list string
  gen_string(s, n->info.doc, val);
}

static void
codegen_dsym(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_str_node *n = dsym_node(varnode);
  // Generate the list content, then intern to symbol
  gen_string(s, n->list, val);
  if (val) {
    gen_intern(s);
  }
}

static void
codegen_nth_ref(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_nth_ref_node *n = (struct mrb_ast_nth_ref_node*)varnode;
  mrb_state *mrb = s->mrb;
  mrb_value str;
  int sym;

  str = mrb_format(mrb, "$%d", n->nth);
  sym = sym_idx(s, mrb_intern_str(mrb, str));
  gen_load_op2(s, OP_GETGV, sym, val);
}

static void
codegen_back_ref(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_back_ref_node *n = (struct mrb_ast_back_ref_node*)varnode;
  char buf[] = {'$', (char)n->type};
  int sym = sym_idx(s, mrb_intern(s->mrb, buf, sizeof(buf)));
  gen_load_op2(s, OP_GETGV, sym, val);
}

static void
codegen_nvar(codegen_scope *s, node *varnode, int val)
{
  if (!val) return;
  struct mrb_ast_nvar_node *n = (struct mrb_ast_nvar_node*)varnode;

  gen_move(s, cursp(), n->num, val);
  push();
}

static void
codegen_dvar(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_dvar_node *n = (struct mrb_ast_dvar_node*)varnode;
  // DVAR nodes are not currently used in mruby, but provide basic implementation
  if (val) {
    gen_lvar(s, n->name, val);
  }
}

/* Unary operator codegen functions */
static void
codegen_not(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_not_node *n = (struct mrb_ast_not_node*)varnode;
  // NOT nodes are rarely used - generate method call to !
  if (val) {
    codegen(s, n->operand, TRUE);
    pop();
    mrb_sym sym = sym_idx(s, mrb_intern_lit(s->mrb, "!"));
    genop_3(s, OP_SEND, cursp(), sym, 0);
    push();
  }
}

static void
codegen_negate(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_negate_node *n = (struct mrb_ast_negate_node*)varnode;
  node *tree = n->operand;

  /* Check if the operand is a variable-sized node */
  enum node_type vnt = node_type(tree);
  switch (vnt) {
#ifndef MRB_NO_FLOAT
  case NODE_FLOAT:
    if (val) {
      struct mrb_ast_float_node *float_n = (struct mrb_ast_float_node*)tree;
      const char *value = float_n->value;
      double f;

      mrb_read_float(value, NULL, &f);
      int off = new_lit_float(s, (mrb_float)-f);

      gen_load_lit(s, off);
    }
    break;
#endif

  case NODE_INT:
    if (val) {
      int32_t value = int_node(tree)->value;
      if (value == INT32_MIN) {
        /* -INT32_MIN overflows, use bigint */
        int off = new_litbint(s, "2147483648", -10);
        genop_2(s, OP_LOADL, cursp(), off);
      }
      else {
        gen_int(s, cursp(), -value);
      }
      push();
    }
    break;

  case NODE_BIGINT:
    if (val) {
      char *str = bigint_node(tree)->string;
      int base = bigint_node(tree)->base;
      /* Negate base to indicate negative number */
      int off = new_litbint(s, str, -base);
      genop_2(s, OP_LOADL, cursp(), off);
      push();
    }
    break;

  default:
    codegen(s, tree, VAL);
    pop();
    push_n(2);pop_n(2); /* space for receiver&block */
    mrb_sym minus = MRB_OPSYM(minus);
    if (!gen_uniop(s, minus, cursp())) {
      genop_3(s, OP_SEND, cursp(), sym_idx(s, minus), 0);
    }
    if (val) push();
    break;
  }
}

static void
codegen_colon2(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_colon2_node *n = (struct mrb_ast_colon2_node*)varnode;
  // Generate COLON2 (::) access manually
  int sym = sym_idx(s, n->name);
  codegen(s, n->base, VAL);
  pop();
  genop_2(s, OP_GETMCNST, cursp(), sym);
  if (val) push();
}

static void
codegen_colon3(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_colon3_node *n = (struct mrb_ast_colon3_node*)varnode;
  int sym = sym_idx(s, n->name);
  genop_1(s, OP_OCLASS, cursp());
  genop_2(s, OP_GETMCNST, cursp(), sym);
  if (val) push();
}

static void
codegen_defined(codegen_scope *s, node *varnode, int val)
{
  // DEFINED nodes are rarely used - generate basic implementation
  (void)varnode; // suppress unused warning
  if (val) {
    // For now, just return nil (defined? is complex to implement correctly)
    genop_1(s, OP_LOADNIL, cursp());
    push();
  }
}

static void
codegen_zsuper(codegen_scope *s, node *varnode, int val)
{
  /* NODE_ZSUPER now uses mrb_ast_super_node, which may have args */
  struct mrb_ast_super_node *zsuper_n = super_node(varnode);
  node *tree = zsuper_n->args;  /* May be NULL or args added by call_with_block */

  codegen_scope *s2 = s;
  int lv = 0;
  uint16_t ainfo = 0;
  int n = CALL_MAXARGS;
  int sp = cursp();
  mrb_bool has_block_arg = FALSE;

  push();        /* room for receiver */
  int argary_pos = cursp();
  while (!s2->mscope) {
    lv++;
    s2 = s2->prev;
    if (!s2) break;
  }
  if (s2 && s2->ainfo > 0) {
    ainfo = s2->ainfo;
    has_block_arg = (ainfo >> 13) & 0x1;
  }
  if (lv > 0xf) codegen_error(s, "too deep nesting");
  if (ainfo > 0) {
    genop_2S(s, OP_ARGARY, argary_pos, (ainfo<<4)|(lv & 0xf));
    push(); push(); push();   /* ARGARY pushes 3 values at most */
    pop(); pop(); pop();
    /* keyword arguments */
    if (ainfo & 0x1) {
      n |= CALL_MAXARGS<<4;
      push();
      /* If parent has keywords but no block parameter, ARGARY reads garbage for block */
      if (!has_block_arg) {
        genop_1(s, OP_LOADNIL, argary_pos+2);
      }
    }
    /* block argument - tree here is args, so check for block */
    if (tree) {
      struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)tree;
      if (callargs->block_arg) {
        push();
        codegen(s, callargs->block_arg, VAL);
      }
    }
  }
  else {
    /* block argument */
    if (tree) {
      struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)tree;
      if (callargs->block_arg) {
        codegen(s, callargs->block_arg, VAL);
      }
    }
    else if (s2) {
      gen_blkmove(s, 0, lv);
    }
    else {
      genop_1(s, OP_LOADNIL, cursp());
    }
    n = 0;
  }
  s->sp = sp;
  genop_2(s, OP_SUPER, cursp(), n);
  if (val) push();
}

static void
codegen_lambda(codegen_scope *s, node *varnode, int val)
{
  if (!val) return;

  struct mrb_ast_lambda_node *n = lambda_node(varnode);

  /* Call lambda_body directly with individual parameters */
  int idx = lambda_body(s, n->locals, n->args, n->body, 1);
  genop_2(s, OP_LAMBDA, cursp(), idx);
  push();
}

static void
codegen_words(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_words_node *n = words_node(varnode);
  gen_literal_array(s, n->args, FALSE, val);
}

static void
codegen_symbols(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_symbols_node *n = symbols_node(varnode);
  gen_literal_array(s, n->args, TRUE, val);
}

static void
codegen_splat(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_splat_node *n = splat_node(varnode);
  // Generate code for the splat value directly
  codegen(s, n->value, val);
}

static void
codegen_block_arg(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_block_arg_node *n = block_arg_node(varnode);

  if (!n->value) {
    int idx = lv_idx(s, MRB_OPSYM(and));

    if (idx == 0) {
      gen_getupvar(s, cursp(), MRB_OPSYM(and));
    }
    else {
      gen_move(s, cursp(), idx, val);
    }
    if (val) push();
  }
  else {
    codegen(s, n->value, val);
  }
}

static void
codegen_scope_node(codegen_scope *s, const node *varnode, int val)
{
  struct mrb_ast_scope_node *scope = scope_node(varnode);

  /* Pass locals and body directly to scope_body() */
  scope_body(s, scope->locals, scope->body, val);
}

static void
codegen_begin(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_begin_node *begin = begin_node(varnode);
  node *body = begin->body;

  codegen(s, body, val);
}

static void
codegen_ensure(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_ensure_node *ensure = ensure_node(varnode);
  node *body = ensure->body;
  node *ensure_clause = ensure->ensure_clause;

  if (!ensure_clause || !is_empty_stmts(ensure_clause)) {
    int catch_entry, begin, end, target;
    int idx;

    catch_entry = catch_handler_new(s);
    begin = s->pc;
    codegen(s, body, val);
    end = target = s->pc;
    push();
    idx = cursp();
    genop_1(s, OP_EXCEPT, idx);
    push();
    codegen(s, ensure_clause, NOVAL);
    pop();
    genop_1(s, OP_RAISEIF, idx);
    pop();
    catch_handler_set(s, catch_entry, MRB_CATCH_ENSURE, begin, end, target);
  }
  else {                      /* empty ensure ignored */
    codegen(s, body, val);
  }
}

static void
codegen_stmts(codegen_scope *s, node *varnode, int val)
{
  struct mrb_ast_stmts_node *stmts = stmts_node(varnode);
  node *tree = stmts_node(stmts)->stmts;

  if (val && !tree) {
    gen_load_nil(s, 1);
  }
  while (tree) {
    codegen(s, tree->car, tree->cdr ? NOVAL : val);
    tree = tree->cdr;
  }
}

static mrb_bool
is_empty_stmts(node *stmt_node)
{
  if (!stmt_node) return TRUE;

  if (node_type(stmt_node) == NODE_STMTS) {
    /* Variable-sized NODE_STMTS with internal cons-list */
    struct mrb_ast_stmts_node *stmts = (struct mrb_ast_stmts_node*)stmt_node;
    return stmts->stmts == NULL;
  }

  return FALSE;
}

/* Declaration codegen functions */

static void
codegen_alias(codegen_scope *s, const node *varnode, int val)
{
  struct mrb_ast_alias_node *alias = alias_node(varnode);

  int a = sym_idx(s, alias->new_name);
  int b = sym_idx(s, alias->old_name);

  genop_2(s, OP_ALIAS, a, b);
  gen_load_nil(s, val);
}

static void
codegen_undef(codegen_scope *s, const node *varnode, int val)
{
  struct mrb_ast_undef_node *undef = undef_node(varnode);
  node *t = undef->syms;

  while (t) {
    int symbol = sym_idx(s, node_to_sym(t->car));
    genop_1(s, OP_UNDEF, symbol);
    t = t->cdr;
  }
  gen_load_nil(s, val);
}

static void
codegen_sdef(codegen_scope *s, const node *varnode, int val)
{
  struct mrb_ast_sdef_node *sdef = sdef_node(varnode);
  node *recv = sdef->obj;
  int sym = sym_idx(s, sdef->name);

  /* Call lambda_body directly with individual parameters */
  /* For NODE_SDEF, args should contain the full locals structure from defs_setup */
  int idx = lambda_body(s, sdef->locals, sdef->args, sdef->body, 0);

  codegen(s, recv, VAL);
  pop();
  if (idx <= 0xff) {
    /* SDEF fusion: SCLASS + METHOD + DEF -> SDEF */
    genop_3(s, OP_SDEF, cursp(), sym, idx);
  }
  else {
    genop_1(s, OP_SCLASS, cursp());
    push();
    genop_2(s, OP_METHOD, cursp(), idx);
    push(); pop();
    pop();
    genop_2(s, OP_DEF, cursp(), sym);
  }
  if (val) push();
}


static void
codegen(codegen_scope *s, node *tree, int val)
{
  int rlev = s->rlev;

  if (!tree) {
    if (val) {
      genop_1(s, OP_LOADNIL, cursp());
      push();
    }
    return;
  }

  s->rlev++;
  if (s->rlev > MRB_CODEGEN_LEVEL_MAX) {
    codegen_error(s, "too complex expression");
  }

  /* Check if this is a variable-sized node */
  /* For variable-sized nodes, get filename/lineno from the variable node header */
  struct mrb_ast_var_header *var_head = get_var_header(tree);

  if (s->irep && s->filename_index != var_head->filename_index) {
    mrb_sym fname = mrb_parser_get_filename(s->parser, s->filename_index);
    const char *filename = mrb_sym_name_len(s->mrb, fname, NULL);

    if (filename) {
      mrb_debug_info_append_file(s->mrb, s->irep->debug_info,
                                 filename, s->lines, s->debug_start_pos, s->pc);
    }
    s->debug_start_pos = s->pc;
    s->filename_index = var_head->filename_index;
    s->filename_sym = mrb_parser_get_filename(s->parser, var_head->filename_index);
  }
  s->lineno = var_head->lineno;

  /* Process variable-sized node directly */
  enum node_type var_type = (enum node_type)var_head->node_type;

  switch (var_type) {
  case NODE_INT:
    if (val) {
      gen_int(s, cursp(), int_node(tree)->value);
      push();
    }
    break;

  case NODE_BIGINT:
    if (val) {
      char *str = bigint_node(tree)->string;
      int base = bigint_node(tree)->base;
      int off = new_litbint(s, str, base);
      genop_2(s, OP_LOADL, cursp(), off);
      push();
    }
    break;

  case NODE_SYM:
    {
      int i = sym_idx(s, sym_node(tree)->symbol);
      gen_load_op2(s, OP_LOADSYM, i, val);
    }
    break;

  case NODE_LVAR:
    gen_lvar(s, var_node(tree)->symbol, val);
    break;

  case NODE_GVAR:
    gen_xvar(s, var_node(tree)->symbol, val, OP_GETGV);
    break;

  case NODE_IVAR:
    gen_xvar(s, var_node(tree)->symbol, val, OP_GETIV);
    break;

  case NODE_CVAR:
    gen_xvar(s, var_node(tree)->symbol, val, OP_GETCV);
    break;

  case NODE_CALL:
    codegen_call(s, tree, val);
    break;

  case NODE_ARRAY:
    codegen_array(s, tree, val);
    break;

  case NODE_HASH:
    codegen_hash(s, tree, val);
    break;

  case NODE_IF:
    codegen_if(s, tree, val);
    break;

  case NODE_WHILE:
    codegen_while(s, tree, val);
    break;

  case NODE_UNTIL:
    codegen_until(s, tree, val);
    break;

  case NODE_FOR:
    codegen_for(s, tree, val);
    break;

  case NODE_CASE:
    codegen_case(s, tree, val);
    break;

  case NODE_CASE_MATCH:
    codegen_case_match(s, tree, val);
    break;

  case NODE_MATCH_PAT:
    {
      /* One-line pattern matching: expr in pattern / expr => pattern */
      struct mrb_ast_match_pat_node *mp = match_pat_node(tree);
      int head;
      int known_array_len = -1;
      uint32_t fail_pos = JMPLINK_START;

      /* Optimize: for simple variable pattern, generate value directly into variable */
      if (node_type(mp->pattern) == NODE_PAT_VAR) {
        struct mrb_ast_pat_var_node *pat_var = pat_var_node(mp->pattern);
        if (pat_var->name) {
          int idx = lv_idx(s, pat_var->name);
          if (idx > 0) {
            codegen(s, mp->value, VAL);
            pop();
            gen_move(s, idx, cursp(), 0);  /* peephole optimizes LOADI+MOVE */
            goto match_pat_push_result;
          }
        }
        /* Wildcard pattern - just evaluate value for side effects */
        codegen(s, mp->value, NOVAL);
      match_pat_push_result:
        if (val) {
          /* 'in' pattern returns true, '=>' pattern returns nil */
          if (mp->raise_on_fail) {
            gen_load_nil(s, 1);
          }
          else {
            genop_1(s, OP_LOADTRUE, cursp());
            push();
          }
        }
        break;
      }

      /* Optimize: array literal => array pattern with matching sizes */
      if (node_type(mp->value) == NODE_ARRAY &&
          node_type(mp->pattern) == NODE_PAT_ARRAY) {
        struct mrb_ast_array_node *arr = array_node(mp->value);
        struct mrb_ast_pat_array_node *pat = pat_array_node(mp->pattern);
        /* Only optimize for exact match (no rest, no post) */
        if (pat->rest == 0 && pat->post == NULL) {
          /* Count array elements and pattern pre elements */
          int arr_len = 0, pat_len = 0;
          node *e;
          for (e = arr->elements; e; e = e->cdr) arr_len++;
          for (e = pat->pre; e; e = e->cdr) pat_len++;
          if (arr_len == pat_len) {
            /* Sizes match - skip deconstruct and size check */
            int arr_reg = cursp();
            int i = 0;
            codegen(s, mp->value, VAL);  /* Generate array */
            /* Extract elements directly with GETIDX */
            for (e = pat->pre; e; e = e->cdr, i++) {
              gen_move(s, cursp(), arr_reg, 0);
              push();
              gen_int(s, cursp(), i);
              push();
              genop_1(s, OP_GETIDX, cursp() - 2);  /* R[a] = R[a][R[a+1]] */
              pop();
              /* Match element pattern (element is now at cursp()-1) */
              codegen_pattern(s, e->car, cursp() - 1, &fail_pos, -1);
              pop();  /* clean up array copy slot */
            }
            pop();  /* pop array */
            if (fail_pos != JMPLINK_START) {
              goto pattern_fail_handling;
            }
            /* Pattern always matches - push result if needed */
            if (val) {
              if (mp->raise_on_fail) {
                gen_load_nil(s, 1);  /* '=>' pattern returns nil */
              }
              else {
                genop_1(s, OP_LOADTRUE, cursp());  /* 'in' pattern returns true */
                push();
              }
            }
            break;
          }
        }
      }

      head = cursp();

      /* Check if value is array literal for optimization */
      if (node_type(mp->value) == NODE_ARRAY) {
        struct mrb_ast_array_node *arr = array_node(mp->value);
        node *elem;
        known_array_len = 0;
        for (elem = arr->elements; elem; elem = elem->cdr) known_array_len++;
      }

      /* Evaluate the value */
      codegen(s, mp->value, VAL);

      /* Generate pattern matching code */
      codegen_pattern(s, mp->pattern, head, &fail_pos, known_array_len);

    pattern_fail_handling:
      if (fail_pos != JMPLINK_START) {
        /* Pattern can fail - generate failure handling code */
        uint32_t match_pos;
        int saved_sp = cursp();  /* save stack pointer before branching */

        /* Success path: pattern matched */
        pop();  /* pop the value */
        if (val) {
          /* 'in' pattern returns true, '=>' pattern returns nil */
          if (mp->raise_on_fail) {
            gen_load_nil(s, 1);
          }
          else {
            genop_1(s, OP_LOADTRUE, cursp());
            push();
          }
        }

        /* Optimize: single JMPNOT can be replaced with MATCHERR for raise_on_fail */
        /* Conditions: (1) single entry in fail_pos chain,
         * (2) JMPNOT is immediately before current position (no code between), and
         * (3) the instruction is actually JMPNOT (not JMP from undefined pinned var) */
        if ((int32_t)(fail_pos + 2) + (int16_t)PEEK_S(s->iseq+fail_pos) == 0 &&
            fail_pos + 2 == s->pc &&
            s->iseq[fail_pos - 2] == OP_JMPNOT) {
          if (mp->raise_on_fail) {
            /* Replace JMPNOT(BS,4bytes) with MATCHERR(B,2bytes)+NOP+NOP;
             * keep the same size so that any jump targeting s->pc stays valid */
            s->iseq[fail_pos - 2] = OP_MATCHERR;
            /* fail_pos-1 already holds the register operand */
            s->iseq[fail_pos] = OP_NOP;
            s->iseq[fail_pos + 1] = OP_NOP;
            s->sp = saved_sp - 1;
            if (val) push();
            break;  /* Pattern matching complete */
          }
          /* Single failure point with 'in' pattern - invert JMPNOT to JMPIF */
          s->iseq[fail_pos - 2] = OP_JMPIF;
          match_pos = fail_pos;
        }
        else {
          /* Multiple failure points - need JMP to skip error handling */
          match_pos = genjmp(s, OP_JMP, JMPLINK_START);
          dispatch_linked(s, fail_pos);
        }

        /* Failure path: restore stack pointer (value still on stack at runtime) */
        s->sp = saved_sp;
        pop();  /* pop the value */
        if (mp->raise_on_fail) {
          /* expr => pattern: raise NoMatchingPatternError */
          genop_1(s, OP_LOADFALSE, cursp());  /* Load false for MATCHERR */
          genop_1(s, OP_MATCHERR, cursp());
        }
        else {
          /* expr in pattern: return false */
          if (val) {
            genop_1(s, OP_LOADFALSE, cursp());
            push();
          }
        }

        /* End of pattern matching */
        dispatch(s, match_pos);
        /* Restore sp to match success path value */
        s->sp = saved_sp - 1;
        if (val) push();
      }
      else {
        /* Pattern always matches - pop value and push result if needed */
        pop();  /* pop the value */
        if (val) {
          if (mp->raise_on_fail) {
            gen_load_nil(s, 1);  /* '=>' pattern returns nil */
          }
          else {
            genop_1(s, OP_LOADTRUE, cursp());  /* 'in' pattern returns true */
            push();
          }
        }
      }
    }
    break;

  case NODE_DEF:
    codegen_def(s, tree, val);
    break;

  case NODE_CLASS:
    codegen_class(s, tree, val);
    break;

  case NODE_MODULE:
    codegen_module(s, tree, val);
    break;

  case NODE_SCLASS:
    codegen_sclass(s, tree, val);
    break;

  case NODE_ASGN:
    codegen_asgn(s, tree, val);
    break;

  case NODE_MASGN:
    codegen_masgn(s, tree, NULL, 0, val);
    break;

  case NODE_MARG:
    /* Parameter destructuring should be handled inline by lambda_body */
    /* This case should not be reached in normal execution */
    break;

  case NODE_OP_ASGN:
    codegen_op_asgn(s, tree, val);
    break;

  case NODE_AND:
    codegen_and(s, tree, val);
    break;

  case NODE_OR:
    codegen_or(s, tree, val);
    break;

  case NODE_RETURN:
    codegen_return(s, tree, val);
    break;

  case NODE_YIELD:
    codegen_yield(s, tree, val);
    break;

  case NODE_SUPER:
    codegen_super(s, tree, val);
    break;

  case NODE_STR:
    codegen_str(s, tree, val);
    break;

  case NODE_DOT2:
    codegen_dot2(s, tree, val);
    break;

  case NODE_DOT3:
    codegen_dot3(s, tree, val);
    break;

  case NODE_FLOAT:
    codegen_float(s, tree, val);
    break;

  case NODE_SELF:
    codegen_self(s, tree, val);
    break;

  case NODE_NIL:
    codegen_nil(s, tree, val);
    break;

  case NODE_TRUE:
    codegen_true(s, tree, val);
    break;

  case NODE_FALSE:
    codegen_false(s, tree, val);
    break;

  case NODE_CONST:
    codegen_const(s, tree, val);
    break;

  case NODE_RESCUE:
    codegen_rescue(s, tree, val);
    break;

  case NODE_BLOCK:
    codegen_block(s, tree, val);
    break;

  case NODE_BREAK:
    codegen_break(s, tree, val);
    break;

  case NODE_NEXT:
    codegen_next(s, tree, val);
    break;

  case NODE_REDO:
    codegen_redo(s, tree, val);
    break;

  case NODE_RETRY:
    codegen_retry(s, tree, val);
    break;

  case NODE_WHILE_MOD:
    codegen_while_mod(s, tree, val);
    break;

  case NODE_UNTIL_MOD:
    codegen_until_mod(s, tree, val);
    break;

  case NODE_XSTR:
    codegen_xstr(s, tree, val);
    break;

  case NODE_REGX:
    codegen_regx(s, tree, val);
    break;

  case NODE_HEREDOC:
    codegen_heredoc(s, tree, val);
    break;

  case NODE_DSYM:
    codegen_dsym(s, tree, val);
    break;

  case NODE_NTH_REF:
    codegen_nth_ref(s, tree, val);
    break;

  case NODE_BACK_REF:
    codegen_back_ref(s, tree, val);
    break;

  case NODE_NVAR:
    codegen_nvar(s, tree, val);
    break;

  case NODE_DVAR:
    codegen_dvar(s, tree, val);
    break;

  case NODE_NOT:
    codegen_not(s, tree, val);
    break;

  case NODE_NEGATE:
    codegen_negate(s, tree, val);
    break;

  case NODE_COLON2:
    codegen_colon2(s, tree, val);
    break;

  case NODE_COLON3:
    codegen_colon3(s, tree, val);
    break;

  case NODE_DEFINED:
    codegen_defined(s, tree, val);
    break;

  case NODE_ZSUPER:
    codegen_zsuper(s, tree, val);
    break;

  case NODE_LAMBDA:
    codegen_lambda(s, tree, val);
    break;

  case NODE_WORDS:
    codegen_words(s, tree, val);
    break;

  case NODE_SYMBOLS:
    codegen_symbols(s, tree, val);
    break;

  case NODE_SPLAT:
    codegen_splat(s, tree, val);
    break;

  case NODE_BLOCK_ARG:
    codegen_block_arg(s, tree, val);
    break;

  case NODE_SCOPE:
    codegen_scope_node(s, tree, val);
    break;

  case NODE_BEGIN:
    codegen_begin(s, tree, val);
    break;

  case NODE_ENSURE:
    codegen_ensure(s, tree, val);
    break;

  case NODE_STMTS:
    codegen_stmts(s, tree, val);
    break;

  case NODE_ALIAS:
    codegen_alias(s, tree, val);
    break;

  case NODE_UNDEF:
    codegen_undef(s, tree, val);
    break;

  case NODE_POSTEXE:
    {
      struct mrb_ast_postexe_node *postexe = postexe_node(tree);
      codegen(s, postexe->body, NOVAL);
    }
    break;

  case NODE_SDEF:
    codegen_sdef(s, tree, val);
    break;

  default:
    /* Unhandled variable-sized node type - should not occur with current AST */
    break;
  }
  s->rlev = rlev;
}

static void
scope_add_irep(codegen_scope *s)
{
  mrb_irep *irep;
  codegen_scope *prev = s->prev;

  if (prev->irep == NULL) {
    irep = mrb_add_irep(s->mrb);
    prev->irep = s->irep = irep;
    return;
  }
  else {
    if (prev->irep->rlen == UINT16_MAX) {
      codegen_error(s, "too many nested blocks/methods");
    }
    s->irep = irep = mrb_add_irep(s->mrb);
    if (prev->irep->rlen == prev->rcapa) {
      prev->rcapa *= 2;
      prev->reps = (mrb_irep**)mrbc_realloc(prev->reps, sizeof(mrb_irep*)*prev->rcapa);
    }
    prev->reps[prev->irep->rlen] = irep;
    prev->irep->rlen++;
  }
}

static codegen_scope*
scope_new(mrb_state *mrb, codegen_scope *prev, node *nlv)
{
  static const codegen_scope codegen_scope_zero = { 0 };
  mempool *pool = mempool_open();
  codegen_scope *s = (codegen_scope*)mempool_alloc(pool, sizeof(codegen_scope));

  if (!s) {
    if (prev)
      codegen_error(prev, "unexpected scope");
    return NULL;
  }
  *s = codegen_scope_zero;
  s->mrb = mrb;
  s->mpool = pool;
  if (!prev) return s;
  s->prev = prev;
  s->ainfo = 0;
  s->mscope = 0;

  scope_add_irep(s);

  s->rcapa = 8;
  s->reps = (mrb_irep**)mrbc_malloc(sizeof(mrb_irep*)*s->rcapa);

  s->icapa = 1024;
  s->iseq = (mrb_code*)mrbc_malloc(sizeof(mrb_code)*s->icapa);

  s->pcapa = 32;
  s->pool = (mrb_irep_pool*)mrbc_malloc(sizeof(mrb_irep_pool)*s->pcapa);

  s->scapa = 256;
  s->syms = (mrb_sym*)mrbc_malloc(sizeof(mrb_sym)*s->scapa);

  s->lv = nlv;
  s->sp += (nlv ? node_len(nlv) : 0) + 1;        /* add self */
  s->nlocals = s->nregs = s->sp;
  if (nlv) {
    mrb_sym *lv;
    node *n = nlv;
    size_t i = 0;

    s->irep->lv = lv = (mrb_sym*)mrbc_malloc(sizeof(mrb_sym)*(s->nlocals-1));
    for (i=0, n=nlv; n; i++,n=n->cdr) {
      lv[i] = lv_name(n);
    }
    mrb_assert(i + 1 == s->nlocals);
  }
  s->ai = mrb_gc_arena_save(mrb);

  s->filename_sym = prev->filename_sym;
  if (s->filename_sym) {
    s->lines = (uint16_t*)mrbc_malloc(sizeof(short)*s->icapa);
  }
  s->lineno = prev->lineno;

  /* debug setting */
  s->debug_start_pos = 0;
  if (s->filename_sym) {
    mrb_debug_info_alloc(mrb, s->irep);
  }
  else {
    s->irep->debug_info = NULL;
  }
  s->parser = prev->parser;
  s->filename_index = prev->filename_index;

  s->rlev = prev->rlev+1;

  return s;
}

static void
scope_finish(codegen_scope *s)
{
  mrb_state *mrb = s->mrb;
  mrb_irep *irep = s->irep;

  if (s->nlocals > 0xff) {
    codegen_error(s, "too many local variables");
  }
  irep->flags = 0;
  if (s->iseq) {
    size_t catchsize = sizeof(struct mrb_irep_catch_handler) * irep->clen;
    irep->iseq = (const mrb_code*)mrbc_realloc(s->iseq, sizeof(mrb_code)*s->pc + catchsize);
    irep->ilen = s->pc;
    if (irep->clen > 0) {
      memcpy((void*)(irep->iseq + irep->ilen), s->catch_table, catchsize);
    }
  }
  else {
    irep->clen = 0;
  }
  mrbc_free(s->catch_table);
  s->catch_table = NULL;
  irep->pool = (const mrb_irep_pool*)mrbc_realloc(s->pool, sizeof(mrb_irep_pool)*irep->plen);
  irep->syms = (const mrb_sym*)mrbc_realloc(s->syms, sizeof(mrb_sym)*irep->slen);
  irep->reps = (const mrb_irep**)mrbc_realloc(s->reps, sizeof(mrb_irep*)*irep->rlen);
  if (s->filename_sym) {
    mrb_sym fname = mrb_parser_get_filename(s->parser, s->filename_index);
    const char *filename = mrb_sym_name_len(s->mrb, fname, NULL);

    mrb_debug_info_append_file(s->mrb, s->irep->debug_info,
                               filename, s->lines, s->debug_start_pos, s->pc);
  }
  mrbc_free(s->lines);

  irep->nlocals = s->nlocals;
  irep->nregs = s->nregs;

  mrb_gc_arena_restore(mrb, s->ai);
  mempool_close(s->mpool);
}

static struct loopinfo*
loop_push(codegen_scope *s, enum looptype t)
{
  struct loopinfo *p = (struct loopinfo*)codegen_palloc(s, sizeof(struct loopinfo));

  p->type = t;
  p->pc0 = p->pc1 = p->pc2 = JMPLINK_START;
  p->prev = s->loop;
  p->reg = cursp();
  s->loop = p;

  return p;
}

static void
loop_break(codegen_scope *s, node *tree)
{
  if (!s->loop) {
    codegen(s, tree, NOVAL);
    raise_error(s, "unexpected break");
  }
  else {
    struct loopinfo *loop;

    loop = s->loop;
    if (tree) {
      if (loop->reg < 0) {
        codegen(s, tree, NOVAL);
      }
      else {
        gen_retval(s, tree);
      }
    }
    while (loop) {
      if (loop->type == LOOP_BEGIN) {
        loop = loop->prev;
      }
      else if (loop->type == LOOP_RESCUE) {
        loop = loop->prev;
      }
      else{
        break;
      }
    }
    if (!loop) {
      raise_error(s, "unexpected break");
      return;
    }

    if (loop->type == LOOP_NORMAL) {
      int tmp;

      if (loop->reg >= 0) {
        if (tree) {
          gen_move(s, loop->reg, cursp(), 0);
        }
        else {
          genop_1(s, OP_LOADNIL, loop->reg);
        }
      }
      tmp = genjmp(s, OP_JMPUW, loop->pc2);
      loop->pc2 = tmp;
    }
    else {
      if (!tree) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      gen_return(s, OP_BREAK, cursp());
    }
  }
}

static void
loop_pop(codegen_scope *s, int val)
{
  if (val) {
    genop_1(s, OP_LOADNIL, cursp());
  }
  dispatch_linked(s, s->loop->pc2);
  s->loop = s->loop->prev;
  if (val) push();
}

static int
catch_handler_new(codegen_scope *s)
{
  size_t newsize = sizeof(struct mrb_irep_catch_handler) * (s->irep->clen + 1);
  s->catch_table = (struct mrb_irep_catch_handler*)mrbc_realloc((void*)s->catch_table, newsize);
  return s->irep->clen++;
}

static void
catch_handler_set(codegen_scope *s, int ent, enum mrb_catch_type type, uint32_t begin, uint32_t end, uint32_t target)
{
  struct mrb_irep_catch_handler *e;

  mrb_assert(ent >= 0 && ent < s->irep->clen);

  e = &s->catch_table[ent];
  uint8_to_bin(type, &e->type);
  mrb_irep_catch_handler_pack(begin, e->begin);
  mrb_irep_catch_handler_pack(end, e->end);
  mrb_irep_catch_handler_pack(target, e->target);
}

static struct RProc*
generate_code(mrb_state *mrb, parser_state *p, int val)
{
  codegen_scope *scope = scope_new(mrb, 0, 0);
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;
  struct mrb_jmpbuf jmpbuf;
  struct RProc *proc;

  mrb->jmp = &jmpbuf;

  scope->mrb = mrb;
  scope->parser = p;
  scope->filename_sym = p->filename_sym;
  scope->filename_index = p->current_filename_index;

  MRB_TRY(mrb->jmp) {
    /* prepare irep */
    codegen(scope, p->tree, val);
    proc = mrb_proc_new(mrb, scope->irep);
    mrb_irep_decref(mrb, scope->irep);
    mempool_close(scope->mpool);
    proc->c = NULL;
    if (mrb->c->cibase && mrb->c->cibase->proc == proc->upper) {
      proc->upper = NULL;
    }
    mrb->jmp = prev_jmp;
    return proc;
  }
  MRB_CATCH(mrb->jmp) {
    mrb_irep_decref(mrb, scope->irep);
    mempool_close(scope->mpool);
    mrb->jmp = prev_jmp;
    return NULL;
  }
  MRB_END_EXC(mrb->jmp);
}


MRB_API struct RProc*
mrb_generate_code(mrb_state *mrb, parser_state *p)
{
  return generate_code(mrb, p, p->no_return_value ? NOVAL : VAL);
}
