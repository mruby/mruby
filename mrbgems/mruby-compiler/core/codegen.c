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

static void gen_assignment(codegen_scope *s, node *tree, node *rhs, int sp, int val);
static void gen_massignment(codegen_scope *s, node *tree, int sp, int val);

static void codegen(codegen_scope *s, node *tree, int val);
static void raise_error(codegen_scope *s, const char *msg);

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
    case OP_LOADF:
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
    case OP_LOADT: case OP_LOADI8: case OP_LOADINEG: case OP_LOADI__1:
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
    case OP_LOADNIL: case OP_LOADSELF: case OP_LOADT: case OP_LOADF:
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
          data0 = mrb_decode_insn(mrb_prev_pc(s, data0.addr));
          mrb_int n;
          if (data0.a == dst && get_int_operand(s, &data0, &n)) {
            if ((data.insn == OP_ADDI && !mrb_int_add_overflow(n, data.b, &n)) ||
                (data.insn == OP_SUBI && !mrb_int_sub_overflow(n, data.b, &n))) {
              s->pc = addr_pc(s, data0.addr);
              gen_int(s, dst, n);
              return;
            }
          }
        }
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
    else if (data.insn != OP_RETURN) {
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
  if ((p->tt & 3) == IREP_TT_SSTR) { /* Check if it's a shared/static string */
    str = (char*)mrbc_malloc(len+1); /* Allocate new memory if it was shared */
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
  else if (op == MRB_OPSYM_2(s->mrb, aref)) {
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
    if (op == MRB_OPSYM_2(s->mrb, lshift)) {
      if (!mrb_num_shift(s->mrb, n0, n, &n)) return FALSE;
    }
    else if (op == MRB_OPSYM_2(s->mrb, rshift)) {
      if (n == MRB_INT_MIN) return FALSE;
      if (!mrb_num_shift(s->mrb, n0, -n, &n)) return FALSE;
    }
    else if (op == MRB_OPSYM_2(s->mrb, mod) && n != 0) {
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
    else if (op == MRB_OPSYM_2(s->mrb, and)) {
      n = n0 & n;
    }
    else if (op == MRB_OPSYM_2(s->mrb, or)) {
      n = n0 | n;
    }
    else if (op == MRB_OPSYM_2(s->mrb, xor)) {
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
new_sym(codegen_scope *s, mrb_sym sym)
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
 * - It first ensures the symbol `sym` is in the IREP's symbol list by calling `new_sym`,
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
  int idx = new_sym(s, sym);
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
  if (sym == MRB_OPSYM_2(s->mrb, plus)) {
    /* unary plus does nothing */
  }
  else if (sym == MRB_OPSYM_2(s->mrb, minus)) {
    if (n == MRB_INT_MIN) return FALSE;
    n = -n;
  }
  else if (sym == MRB_OPSYM_2(s->mrb, neg)) {
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

  while (tree) {
    n++;
    tree = tree->cdr;
  }
  return n;
}

/* Casts a void* (typically from an AST node part) to an int. */
#define nint(x) ((int)(intptr_t)(x))
/* Casts a void* (typically from an AST node part) to a char. */
#define nchar(x) ((char)(intptr_t)(x))
/* Casts a void* (typically from an AST node part) to an mrb_sym. */
#define nsym(x) ((mrb_sym)(intptr_t)(x))

/* Extracts the symbol (name) of a local variable from its AST node representation. */
#define lv_name(lv) nsym((lv)->car)

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

  if (id == MRB_OPSYM_2(s->mrb, and)) {
    codegen_error(s, "No anonymous block parameter");
  }
  else if (id == MRB_OPSYM_2(s->mrb, mul)) {
    codegen_error(s, "No anonymous rest parameter");
  }
  else if (id == MRB_OPSYM_2(s->mrb, pow)) {
    codegen_error(s, "No anonymous keyword rest parameter");
  }
  else {
    codegen_error(s, "Can't find local variables");
  }
  return -1; /* not reached */
}

/*
 * Generates the bytecode for a `for` loop.
 *
 * A `for` loop in mruby, like `for x in collection`, is typically syntactic sugar for
 * `collection.each { |x| ... }`. This function implements that transformation.
 *
 * The process involves:
 * 1. Generating code for the `collection` (the receiver of the `each` call).
 * 2. Creating a new scope for the block that will be passed to `each`.
 * 3. Inside this new block scope:
 *    a. Emitting `OP_ENTER` to set up the block's argument handling.
 *       The argument specification `0x40000` likely indicates a block that
 *       takes one mandatory argument.
 *    b. Generating code to assign the iterated item (passed as a block argument)
 *       to the loop variable(s) specified in `tree->car`. This can be a simple
 *       assignment or a multiple assignment (destructuring).
 *    c. Setting up a `LOOP_FOR` context for handling `break`/`next`/`redo` within the loop.
 *    d. Generating code for the actual body of the `for` loop (`tree->cdr->cdr->car`).
 *    e. Emitting `OP_RETURN` for the block's implicit return.
 * 4. Finalizing the block scope and obtaining its `mrb_irep`.
 * 5. Back in the original scope, generating `OP_BLOCK` to create a closure from the
 *    block's `mrb_irep`.
 * 6. Generating `OP_SENDB` to call the `each` method (by symbol) on the collection,
 *    passing the newly created block.
 *
 * @param s The current code generation scope.
 * @param tree The AST node representing the `for` loop.
 *             `tree->car` contains the loop variable(s).
 *             `tree->cdr->car` is the collection being iterated over.
 *             `tree->cdr->cdr->car` is the body of the loop.
 */
static void
for_body(codegen_scope *s, node *tree)
{
  codegen_scope *prev = s;
  int idx;
  struct loopinfo *lp;
  node *n2;

  /* generate receiver */
  codegen(s, tree->cdr->car, VAL);
  /* generate loop-block */
  s = scope_new(s->mrb, s, NULL);

  push();                       /* push for a block parameter */

  /* generate loop variable */
  n2 = tree->car;
  genop_W(s, OP_ENTER, 0x40000);
  if (n2->car && !n2->car->cdr && !n2->cdr) {
    gen_assignment(s, n2->car->car, NULL, 1, NOVAL);
  }
  else {
    gen_massignment(s, n2, 1, VAL);
  }
  /* construct loop */
  lp = loop_push(s, LOOP_FOR);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */

  /* loop body */
  codegen(s, tree->cdr->cdr->car, VAL);
  pop();
  gen_return(s, OP_RETURN, cursp());
  loop_pop(s, NOVAL);
  scope_finish(s);
  s = prev;
  genop_2(s, OP_BLOCK, cursp(), s->irep->rlen-1);
  push();pop(); /* space for a block */
  pop();
  idx = new_sym(s, MRB_SYM_2(s->mrb, each));
  genop_3(s, OP_SENDB, cursp(), idx, 0);
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
lambda_body(codegen_scope *s, node *tree, int blk)
{
  codegen_scope *parent = s;
  /* Create a new scope for the lambda/block body. */
  s = scope_new(s->mrb, s, tree->car);

  /* `mscope` is false for blocks, true for lambdas/methods. */
  s->mscope = !blk;

  /* If it's a block, push a LOOP_BLOCK structure for break/next/return handling. */
  if (blk) {
    struct loopinfo *lp = loop_push(s, LOOP_BLOCK);
    lp->pc0 = new_label(s); /* Mark entry point for potential retry/redo. */
  }
  tree = tree->cdr;

  /* Argument processing */
  if (tree->car == NULL) { /* No arguments */
    genop_W(s, OP_ENTER, 0); /* Generate OP_ENTER with no argument specification. */
    s->ainfo = 0;
  }
  else { /* Has arguments */
    mrb_aspec a;
    int ma, oa, ra, pa, ka, kd, ba, i;
    uint32_t pos;
    node *opt;
    node *margs, *pargs;
    node *tail;

    /* mandatory arguments */
    ma = node_len(tree->car->car);
    margs = tree->car->car;
    tail = tree->car->cdr->cdr->cdr->cdr;

    /* optional arguments */
    oa = node_len(tree->car->cdr->car);
    /* rest argument? */
    ra = tree->car->cdr->cdr->car ? 1 : 0;
    /* mandatory arguments after rest argument */
    pa = node_len(tree->car->cdr->cdr->cdr->car);
    pargs = tree->car->cdr->cdr->cdr->car;
    /* keyword arguments */
    ka = tail ? node_len(tail->cdr->car) : 0;
    /* keyword dictionary? */
    kd = tail && tail->cdr->cdr->car? 1 : 0;
    /* block argument? */
    ba = tail && tail->cdr->cdr->cdr->car ? 1 : 0;

    if (ma > 0x1f || oa > 0x1f || pa > 0x1f || ka > 0x1f) {
      codegen_error(s, "too many formal arguments");
    }
    /* (23bits = 5:5:1:5:5:1:1) */
    a = MRB_ARGS_REQ(ma)
      | MRB_ARGS_OPT(oa)
      | (ra ? MRB_ARGS_REST() : 0)
      | MRB_ARGS_POST(pa)
      | MRB_ARGS_KEY(ka, kd)
      | (ba ? MRB_ARGS_BLOCK() : 0);
    genop_W(s, OP_ENTER, a);
    /* (12bits = 5:1:5:1) - Store argument counts for block argument passing (OP_BLKPUSH) */
    s->ainfo = (((ma+oa) & 0x3f) << 7)
      | ((ra & 0x1) << 6)
      | ((pa & 0x1f) << 1)
      | (ka || kd);

    /* Optional argument default value initialization */
    pos = new_label(s); /* Start of the optional argument jump table. */
    for (i=0; i<oa; i++) {
      new_label(s);
      genjmp_0(s, OP_JMP); /* Placeholder jump for each optional arg. */
    }
    if (oa > 0) {
      genjmp_0(s, OP_JMP); /* Jump to skip all default assignments if all optional args are provided. */
    }
    opt = tree->car->cdr->car; /* AST node for optional arguments. */
    i = 0;
    while (opt) { /* Iterate through optional arguments. */
      int idx;
      mrb_sym id = nsym(opt->car->car); /* Symbol of the optional argument. */

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
    if (tail) { /* `tail` contains keyword arguments and block argument */
      node *kwds = tail->cdr->car; /* AST node for keyword arguments. */
      int kwrest = 0; /* Flag for keyword rest argument (e.g., **kwargs) */

      if (tail->cdr->cdr->car) { /* Check if a keyword rest argument exists. */
        kwrest = 1;
      }
      mrb_assert(nint(tail->car) == NODE_ARGS_TAIL);
      mrb_assert(node_len(tail) == 4);

      while (kwds) {
        int jmpif_key_p, jmp_def_set = -1;
        node *kwd = kwds->car, *def_arg = kwd->cdr->cdr->car;
        mrb_sym kwd_sym = nsym(kwd->cdr->car);

        mrb_assert(nint(kwd->car) == NODE_KW_ARG);

        if (def_arg) {
          int idx;
          genop_2(s, OP_KEY_P, lv_idx(s, kwd_sym), new_sym(s, kwd_sym));
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
        genop_2(s, OP_KARG, lv_idx(s, kwd_sym), new_sym(s, kwd_sym));
        if (jmp_def_set != -1) {
          dispatch(s, jmp_def_set);
        }
        i++;

        kwds = kwds->cdr;
      }
      if (tail->cdr->car && !kwrest) { /* If there are keyword args but no keyword rest. */
        genop_0(s, OP_KEYEND); /* Signal end of keyword arguments. */
      }
      /* Block argument processing */
      if (ba) { /* If a block argument (e.g., &blk) is present. */
        mrb_sym bparam = nsym(tail->cdr->cdr->cdr->car); /* Symbol of the block parameter. */
        pos = ma+oa+ra+pa+(ka||kd); /* Calculate register offset for the block parameter. */
        if (bparam) { /* If it's a named block parameter. */
          int idx = lv_idx(s, bparam);
          genop_2(s, OP_MOVE, idx, pos+1); /* Move the block from its argument slot to the local variable. */
        }
      }
    }

    /* Argument destructuring for mandatory and post-mandatory arguments */
    if (margs) { /* Mandatory arguments */
      node *n = margs;
      pos = 1; /* Start from register 1 (after self). */
      while (n) {
        if (nint(n->car->car) == NODE_MASGN) { /* If the argument is a mass assignment (e.g., |(a,b)| ). */
          gen_massignment(s, n->car->cdr->car, pos, NOVAL);
        }
        pos++;
        n = n->cdr;
      }
    }
    if (pargs) { /* Post-mandatory arguments */
      node *n = pargs;
      pos = ma+oa+ra+1; /* Calculate starting register for post-mandatory args. */
      while (n) {
        if (nint(n->car->car) == NODE_MASGN) { /* If argument is a mass assignment. */
          gen_massignment(s, n->car->cdr->car, pos, NOVAL);
        }
        pos++;
        n = n->cdr;
      }
    }
  }

  /* Generate code for the actual body of the lambda/block. */
  codegen(s, tree->cdr->car, VAL);
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
scope_body(codegen_scope *s, node *tree, int val)
{
  /* Create a new scope, inheriting from `s`, with local variables from `tree->car`. */
  codegen_scope *scope = scope_new(s->mrb, s, tree->car);

  /* Generate code for the body of the scope. */
  codegen(scope, tree->cdr, VAL);
  /* Ensure the scope returns the value of its last expression. */
  gen_return(scope, OP_RETURN, scope->sp-1);

  /* If this is the outermost scope (e.g., top-level script), add OP_STOP. */
  if (!s->iseq) { /* s->iseq would be NULL for the initial dummy scope. */
    genop_0(scope, OP_STOP);
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

static mrb_bool
nosplat(node *t)
{
  while (t) {
    if (nint(t->car->car) == NODE_SPLAT) return FALSE;
    t = t->cdr;
  }
  return TRUE;
}

static mrb_sym
attrsym(codegen_scope *s, mrb_sym a)
{
  const char *name;
  mrb_int len;
  char *name2;

  name = mrb_sym_name_len(s->mrb, a, &len);
  name2 = (char*)codegen_palloc(s,
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
    int is_splat = nint(t->car->car) == NODE_SPLAT;

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
    if (nint(tree->car->car->car) == NODE_KW_REST_ARGS) {
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
gen_call(codegen_scope *s, node *tree, int val, int safe)
{
  mrb_sym sym = nsym(tree->cdr->car);
  int skip = 0, n = 0, nk = 0, noop = no_optimize(s), noself = 0, blk = 0, sp_save = cursp();
  enum mrb_insn opt_op = OP_NOP;

  if (!noop) {
    if (sym == MRB_OPSYM_2(s->mrb, add)) opt_op = OP_ADD;
    else if (sym == MRB_OPSYM_2(s->mrb, sub)) opt_op = OP_SUB;
    else if (sym == MRB_OPSYM_2(s->mrb, mul)) opt_op = OP_MUL;
    else if (sym == MRB_OPSYM_2(s->mrb, div)) opt_op = OP_DIV;
    else if (sym == MRB_OPSYM_2(s->mrb, lt)) opt_op = OP_LT;
    else if (sym == MRB_OPSYM_2(s->mrb, le)) opt_op = OP_LE;
    else if (sym == MRB_OPSYM_2(s->mrb, gt)) opt_op = OP_GT;
    else if (sym == MRB_OPSYM_2(s->mrb, ge)) opt_op = OP_GE;
    else if (sym == MRB_OPSYM_2(s->mrb, eq)) opt_op = OP_EQ;
    else if (sym == MRB_OPSYM_2(s->mrb, aref)) opt_op = OP_GETIDX;
    else if (sym == MRB_OPSYM_2(s->mrb, aset)) opt_op = OP_SETIDX;
  }
  if (!tree->car || (opt_op == OP_NOP && nint(tree->car->car) == NODE_SELF)) {
    noself = 1;
    push();
  }
  else {
    codegen(s, tree->car, VAL); /* receiver */
  }
  if (safe) {
    int recv = cursp()-1;
    gen_move(s, cursp(), recv, 1);
    skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
  }
  tree = tree->cdr->cdr->car;
  if (tree) {
    if (tree->car) {            /* positional arguments */
      n = gen_values(s, tree->car, VAL, 14);
      if (n < 0) {              /* variable length */
        noop = 1;               /* not operator */
        n = 15;
        push();
      }
    }
    if (tree->cdr->car) {       /* keyword arguments */
      noop = 1;
      nk = gen_hash(s, tree->cdr->car->cdr, VAL, 14);
      if (nk < 0) nk = 15;
    }
  }
  if (tree && tree->cdr && tree->cdr->cdr) {
    codegen(s, tree->cdr->cdr, VAL);
    pop();
    noop = 1;
    blk = 1;
  }
  push();
  s->sp = sp_save;
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
    genop_3(s, blk ? OP_SSENDB : OP_SSEND, cursp(), new_sym(s, sym), n|(nk<<4));
  }
  else {
    genop_3(s, blk ? OP_SENDB : OP_SEND, cursp(), new_sym(s, sym), n|(nk<<4));
  }
  if (safe) {
    dispatch(s, skip);
  }
  if (val) {
    push();
  }
}

static void
gen_assignment(codegen_scope *s, node *tree, node *rhs, int sp, int val)
{
  int idx;
  int type = nint(tree->car);

  switch (type) {
  case NODE_GVAR:
  case NODE_ARG:
  case NODE_LVAR:
  case NODE_IVAR:
  case NODE_CVAR:
  case NODE_CONST:
  case NODE_NIL:
  case NODE_MASGN:
    if (rhs) {
      codegen(s, rhs, VAL);
      pop();
      sp = cursp();
    }
    break;

  case NODE_COLON2:
  case NODE_COLON3:
  case NODE_CALL:
  case NODE_SCALL:
    /* keep evaluation order */
    break;

  case NODE_NVAR:
    /* never happens; should have already checked in the parser */
    codegen_error(s, "Can't assign to numbered parameter");
    break;

  default:
    codegen_error(s, "unknown lhs");
    break;
  }

  tree = tree->cdr;
  switch (type) {
  case NODE_GVAR:
    gen_setxv(s, OP_SETGV, sp, nsym(tree), val);
    break;
  case NODE_ARG:
  case NODE_LVAR:
    idx = lv_idx(s, nsym(tree));
    if (idx > 0) {
      if (idx != sp) {
        gen_move(s, idx, sp, val);
      }
      break;
    }
    else {                      /* upvar */
      gen_setupvar(s, sp, nsym(tree));
    }
    break;
  case NODE_IVAR:
    gen_setxv(s, OP_SETIV, sp, nsym(tree), val);
    break;
  case NODE_CVAR:
    gen_setxv(s, OP_SETCV, sp, nsym(tree), val);
    break;
  case NODE_CONST:
    gen_setxv(s, OP_SETCONST, sp, nsym(tree), val);
    break;
  case NODE_COLON2:
  case NODE_COLON3:
    if (sp) {
      gen_move(s, cursp(), sp, 0);
    }
    sp = cursp();
    push();
    if (type == NODE_COLON2) {
      codegen(s, tree->car, VAL);
      idx = new_sym(s, nsym(tree->cdr));
    }
    else {   /* NODE_COLON3 */
      genop_1(s, OP_OCLASS, cursp());
      push();
      idx = new_sym(s, nsym(tree));
    }
    if (rhs) {
      codegen(s, rhs, VAL); pop();
      gen_move(s, sp, cursp(), 0);
    }
    pop_n(2);
    genop_2(s, OP_SETMCNST, sp, idx);
    break;

  case NODE_CALL:
  case NODE_SCALL:
    {
      int noself = 0, safe = (type == NODE_SCALL), skip = 0, top, call, n = 0;
      mrb_sym mid = nsym(tree->cdr->car);

      top = cursp();
      if (val || sp == cursp()) {
        push();                   /* room for retval */
      }
      call = cursp();
      if (!tree->car) {
        noself = 1;
        push();
      }
      else {
        codegen(s, tree->car, VAL); /* receiver */
      }
      if (safe) {
        int recv = cursp()-1;
        gen_move(s, cursp(), recv, 1);
        skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
      }
      tree = tree->cdr->cdr->car;
      if (tree) {
        if (tree->car) {            /* positional arguments */
          n = gen_values(s, tree->car, VAL, (tree->cdr->car)?13:14);
          if (n < 0) {              /* variable length */
            n = 15;
            push();
          }
        }
        if (tree->cdr->car) {       /* keyword arguments */
          if (n == 13 || n == 14) {
            pop_n(n);
            genop_2(s, OP_ARRAY, cursp(), n);
            push();
            n = 15;
          }
          gen_hash(s, tree->cdr->car->cdr, VAL, 0);
          if (n < 14) {
            n++;
          }
          else {
            pop_n(2);
            genop_2(s, OP_ARYPUSH, cursp(), 1);
          }
          push();
        }
      }
      if (rhs) {
        codegen(s, rhs, VAL);
        pop();
      }
      else {
        gen_move(s, cursp(), sp, 0);
      }
      if (val) {
        gen_move(s, top, cursp(), 1);
      }
      if (n < 15) {
        n++;
        if (n == 15) {
          pop_n(14);
          genop_2(s, OP_ARRAY, cursp(), 15);
        }
      }
      else {
        pop();
        genop_2(s, OP_ARYPUSH, cursp(), 1);
      }
      push(); pop();
      s->sp = call;
      if (mid == MRB_OPSYM_2(s->mrb, aref) && n == 2) {
        push_n(4); pop_n(4); /* self + idx + value + (invisible block for OP_SEND) */
        genop_1(s, OP_SETIDX, cursp());
      }
      else {
        int st = 2 /* self + block */ +
                 (((n >> 0) & 0x0f) < 15 ? ((n >> 0) & 0x0f)     : 1) +
                 (((n >> 4) & 0x0f) < 15 ? ((n >> 4) & 0x0f) * 2 : 1);
        push_n(st); pop_n(st);
        genop_3(s, noself ? OP_SSEND : OP_SEND, cursp(), new_sym(s, attrsym(s, mid)), n);
      }
      if (safe) {
        dispatch(s, skip);
      }
      s->sp = top;
    }
    break;

  case NODE_MASGN:
    gen_massignment(s, tree->car, sp, val);
    break;

  /* splat without assignment */
  case NODE_NIL:
    break;

  default:
    codegen_error(s, "unknown lhs");
    break;
  }
  if (val) push();
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
    int i = 0, j = 0, gen = 0;

    while (tree) {
      switch (nint(tree->car->car)) {
      case NODE_STR:
        if ((tree->cdr == NULL) && (nint(tree->car->cdr->cdr) == 0))
          break;
        /* fall through */
      case NODE_STMTS:
      case NODE_BEGIN:
        codegen(s, tree->car, VAL);
        j++;
        break;

      case NODE_LITERAL_DELIM:
        if (j > 0) {
          j = 0;
          i++;
          if (sym)
            gen_intern(s);
        }
        break;
      }
      while (j >= 2) {
        pop(); pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
        j--;
      }
      if (i > GEN_LIT_ARY_MAX) {
        pop_n(i);
        if (gen) {
          pop();
          genop_2(s, OP_ARYPUSH, cursp(), i);
        }
        else {
          genop_2(s, OP_ARRAY, cursp(), i);
          gen = 1;
        }
        push();
        i = 0;
      }
      tree = tree->cdr;
    }
    if (j > 0) {
      i++;
      if (sym)
        gen_intern(s);
    }
    pop_n(i);
    if (gen) {
      pop();
      genop_2(s, OP_ARYPUSH, cursp(), i);
    }
    else {
      genop_2(s, OP_ARRAY, cursp(), i);
    }
    push();
  }
  else {
    while (tree) {
      switch (nint(tree->car->car)) {
      case NODE_STMTS: case NODE_BEGIN: case NODE_BLOCK:
        codegen(s, tree->car, NOVAL);
      }
      tree = tree->cdr;
    }
  }
}

static void
raise_error(codegen_scope *s, const char *msg)
{
  int idx = new_lit_cstr(s, msg);

  genop_1(s, OP_ERR, idx);
}

static mrb_int
readint(codegen_scope *s, const char *p, int base, mrb_bool neg, mrb_bool *overflow)
{
  const char *e = p + strlen(p);
  mrb_int result = 0;

  mrb_assert(base >= 2 && base <= 16);
  if (*p == '+') p++;
  while (p < e) {
    int n;
    char c = *p;
    switch (c) {
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      n = c - '0'; break;
    case '8': case '9':
      n = c - '0'; break;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      n = c - 'a' + 10; break;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      n = c - 'A' + 10; break;
    default:
      codegen_error(s, "malformed readint input");
      *overflow = TRUE;
      /* not reached */
      return result;
    }
    if (mrb_int_mul_overflow(result, base, &result)) {
    overflow:
      *overflow = TRUE;
      return 0;
    }
    mrb_uint tmp = ((mrb_uint)result)+n;
    if (neg && tmp == (mrb_uint)MRB_INT_MAX+1) {
      *overflow = FALSE;
      return MRB_INT_MIN;
    }
    if (tmp > MRB_INT_MAX) goto overflow;
    result = (mrb_int)tmp;
    p++;
  }
  *overflow = FALSE;
  if (neg) return -result;
  return result;
}

static void
gen_retval(codegen_scope *s, node *tree)
{
  if (nint(tree->car) == NODE_SPLAT) {
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
  switch (nint(tree->car)) {
  case NODE_TRUE:
  case NODE_INT:
  case NODE_STR:
  case NODE_SYM:
    return TRUE;
  default:
    return FALSE;
  }
}

static mrb_bool
false_always(node *tree)
{
  switch (nint(tree->car)) {
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
codegen(codegen_scope *s, node *tree, int val)
{
  int nt;
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
  if (s->irep && s->filename_index != tree->filename_index) {
    mrb_sym fname = mrb_parser_get_filename(s->parser, s->filename_index);
    const char *filename = mrb_sym_name_len(s->mrb, fname, NULL);

    mrb_debug_info_append_file(s->mrb, s->irep->debug_info,
                               filename, s->lines, s->debug_start_pos, s->pc);
    s->debug_start_pos = s->pc;
    s->filename_index = tree->filename_index;
    s->filename_sym = mrb_parser_get_filename(s->parser, tree->filename_index);
  }

  nt = nint(tree->car);
  s->lineno = tree->lineno;
  tree = tree->cdr;
  switch (nt) {
  case NODE_STMTS:
    if (val && !tree) {
      genop_1(s, OP_LOADNIL, cursp());
      push();
    }
    while (tree) {
      codegen(s, tree->car, tree->cdr ? NOVAL : val);
      tree = tree->cdr;
    }
    break;

  case NODE_BEGIN:
    /* NODE_BEGIN contains a single body node directly in cdr */
    codegen(s, tree, val);
    break;

  case NODE_RESCUE:
    {
      int noexc;
      uint32_t exend, pos1, pos2, tmp;
      struct loopinfo *lp;
      int catch_entry, begin, end;

      if (tree->car == NULL) goto exit;
      lp = loop_push(s, LOOP_BEGIN);
      lp->pc0 = new_label(s);
      catch_entry = catch_handler_new(s);
      begin = s->pc;
      codegen(s, tree->car, VAL);
      pop();
      lp->type = LOOP_RESCUE;
      end = s->pc;
      noexc = genjmp_0(s, OP_JMP);
      catch_handler_set(s, catch_entry, MRB_CATCH_RESCUE, begin, end, s->pc);
      tree = tree->cdr;
      exend = JMPLINK_START;
      pos1 = JMPLINK_START;
      if (tree->car) {
        node *n2 = tree->car;
        int exc = cursp();

        genop_1(s, OP_EXCEPT, exc);
        push();
        while (n2) {
          node *n3 = n2->car;
          node *n4 = n3->car;

          dispatch(s, pos1);
          pos2 = JMPLINK_START;
          do {
            if (n4 && n4->car && nint(n4->car->car) == NODE_SPLAT) {
              codegen(s, n4->car, VAL);
              gen_move(s, cursp(), exc, 0);
              push_n(2); pop_n(2); /* space for one arg and a block */
              pop();
              genop_3(s, OP_SEND, cursp(), new_sym(s, MRB_SYM_2(s->mrb, __case_eqq)), 1);
            }
            else {
              if (n4) {
                codegen(s, n4->car, VAL);
              }
              else {
                genop_2(s, OP_GETCONST, cursp(), new_sym(s, MRB_SYM_2(s->mrb, StandardError)));
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
      tree = tree->cdr;
      dispatch(s, noexc);
      if (tree->car) {
        codegen(s, tree->car, val);
      }
      else if (val) {
        push();
      }
      dispatch_linked(s, exend);
      loop_pop(s, NOVAL);
    }
    break;

  case NODE_ENSURE:
    if (!tree->cdr || !tree->cdr->cdr ||
        (nint(tree->cdr->cdr->car) == NODE_STMTS &&
         tree->cdr->cdr->cdr)) {
      int catch_entry, begin, end, target;
      int idx;

      catch_entry = catch_handler_new(s);
      begin = s->pc;
      codegen(s, tree->car, val);
      end = target = s->pc;
      push();
      idx = cursp();
      genop_1(s, OP_EXCEPT, idx);
      push();
      codegen(s, tree->cdr->cdr, NOVAL);
      pop();
      genop_1(s, OP_RAISEIF, idx);
      pop();
      catch_handler_set(s, catch_entry, MRB_CATCH_ENSURE, begin, end, target);
    }
    else {                      /* empty ensure ignored */
      codegen(s, tree->car, val);
    }
    break;

  case NODE_LAMBDA:
    if (val) {
      int idx = lambda_body(s, tree, 1);

      genop_2(s, OP_LAMBDA, cursp(), idx);
      push();
    }
    break;

  case NODE_BLOCK:
    if (val) {
      int idx = lambda_body(s, tree, 1);

      genop_2(s, OP_BLOCK, cursp(), idx);
      push();
    }
    break;

  case NODE_IF:
    {
      uint32_t pos1, pos2;
      mrb_bool nil_p = FALSE;
      node *elsepart = tree->cdr->cdr->car;

      if (!tree->car) {
        codegen(s, elsepart, val);
        goto exit;
      }
      if (true_always(tree->car)) {
        codegen(s, tree->cdr->car, val);
        goto exit;
      }
      if (false_always(tree->car)) {
        codegen(s, elsepart, val);
        goto exit;
      }
      if (nint(tree->car->car) == NODE_CALL) {
        node *n = tree->car->cdr;
        mrb_sym mid = nsym(n->cdr->car);
        mrb_sym sym_nil_p = MRB_SYM_Q_2(s->mrb, nil);
        if (mid == sym_nil_p && n->cdr->cdr->car == NULL) {
          nil_p = TRUE;
          codegen(s, n->car, VAL);
        }
      }
      if (!nil_p) {
        codegen(s, tree->car, VAL);
      }
      pop();
      if (val || tree->cdr->car) {
        if (nil_p) {
          pos2 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
          pos1 = genjmp_0(s, OP_JMP);
          dispatch(s, pos2);
        }
        else {
          pos1 = genjmp2_0(s, OP_JMPNOT, cursp(), val);
        }
        codegen(s, tree->cdr->car, val);
        if (val) pop();
        if (elsepart || val) {
          pos2 = genjmp_0(s, OP_JMP);
          dispatch(s, pos1);
          codegen(s, elsepart, val);
          dispatch(s, pos2);
        }
        else {
          dispatch(s, pos1);
        }
      }
      else {                    /* empty then-part */
        if (elsepart) {
          if (nil_p) {
            pos1 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
          }
          else {
            pos1 = genjmp2_0(s, OP_JMPIF, cursp(), val);
          }
          codegen(s, elsepart, val);
          dispatch(s, pos1);
        }
        else if (val && !nil_p) {
          genop_1(s, OP_LOADNIL, cursp());
          push();
        }
      }
    }
    break;

  case NODE_AND:
    {
      uint32_t pos;

      if (true_always(tree->car)) {
        codegen(s, tree->cdr, val);
        goto exit;
      }
      if (false_always(tree->car)) {
        codegen(s, tree->car, val);
        goto exit;
      }
      codegen(s, tree->car, VAL);
      pop();
      pos = genjmp2_0(s, OP_JMPNOT, cursp(), val);
      codegen(s, tree->cdr, val);
      dispatch(s, pos);
    }
    break;

  case NODE_OR:
    {
      uint32_t pos;

      if (true_always(tree->car)) {
        codegen(s, tree->car, val);
        goto exit;
      }
      if (false_always(tree->car)) {
        codegen(s, tree->cdr, val);
        goto exit;
      }
      codegen(s, tree->car, VAL);
      pop();
      pos = genjmp2_0(s, OP_JMPIF, cursp(), val);
      codegen(s, tree->cdr, val);
      dispatch(s, pos);
    }
    break;

  case NODE_WHILE_MOD:
  case NODE_UNTIL_MOD:
    /* Post-tested loops: execute body first, then check condition */
    if (false_always(tree->car)) {
      if (nt == NODE_WHILE_MOD) {
        /* begin...end while false - execute once then exit */
        codegen(s, tree->cdr, val);
        if (val) push();
        goto exit;
      }
    }
    else if (true_always(tree->car)) {
      if (nt == NODE_UNTIL_MOD) {
        /* begin...end until true - execute once then exit */
        codegen(s, tree->cdr, val);
        if (val) push();
        goto exit;
      }
    }
    genjmp_0(s, OP_JMP);
    /* fall through */
  case NODE_WHILE:
  case NODE_UNTIL:
    {
      uint32_t pos0 = JMPLINK_START;
      if (nt == NODE_WHILE_MOD || nt == NODE_UNTIL_MOD) {
        pos0 = s->pc - mrb_insn_size[OP_JMP] + 1;
      }

      if (true_always(tree->car)) {
        if (nt == NODE_UNTIL) {
          if (val) {
            genop_1(s, OP_LOADNIL, cursp());
            push();
          }
          goto exit;
        }
      }
      else if (false_always(tree->car)) {
        if (nt == NODE_WHILE) {
          if (val) {
            genop_1(s, OP_LOADNIL, cursp());
            push();
          }
          goto exit;
        }
      }

      struct loopinfo *lp = loop_push(s, LOOP_NORMAL);

      if (!val) lp->reg = -1;
      lp->pc0 = new_label(s);
      codegen(s, tree->car, VAL);
      pop();

      uint32_t pos;
      if (nt == NODE_WHILE || nt == NODE_WHILE_MOD) {
        pos = genjmp2_0(s, OP_JMPNOT, cursp(), NOVAL);
      }
      else { /* UNTIL */
        pos = genjmp2_0(s, OP_JMPIF, cursp(), NOVAL);
      }
      lp->pc1 = new_label(s);
      genop_0(s, OP_NOP); /* for redo */
      dispatch(s, pos0);
      codegen(s, tree->cdr, NOVAL);
      genjmp(s, OP_JMP, lp->pc0);
      dispatch(s, pos);
      loop_pop(s, val);
    }
    break;

  case NODE_FOR:
    for_body(s, tree);
    if (val) push();
    break;

  case NODE_CASE:
    {
      int head = 0;
      uint32_t pos1, pos2, pos3, tmp;
      node *n;

      pos3 = JMPLINK_START;
      if (tree->car) {
        head = cursp();
        codegen(s, tree->car, VAL);
      }
      tree = tree->cdr;
      while (tree) {
        n = tree->car->car;
        pos1 = pos2 = JMPLINK_START;
        while (n) {
          codegen(s, n->car, VAL);
          if (head) {
            gen_move(s, cursp(), head, 0);
            push(); push(); pop(); pop(); pop();
            if (nint(n->car->car) == NODE_SPLAT) {
              genop_3(s, OP_SEND, cursp(), new_sym(s, MRB_SYM_2(s->mrb, __case_eqq)), 1);
            }
            else {
              genop_3(s, OP_SEND, cursp(), new_sym(s, MRB_OPSYM_2(s->mrb, eqq)), 1);
            }
          }
          else {
            pop();
          }
          tmp = genjmp2(s, OP_JMPIF, cursp(), pos2, !head);
          pos2 = tmp;
          n = n->cdr;
        }
        if (tree->car->car) {
          pos1 = genjmp_0(s, OP_JMP);
          dispatch_linked(s, pos2);
        }
        codegen(s, tree->car->cdr, val);
        if (val) pop();
        tmp = genjmp(s, OP_JMP, pos3);
        pos3 = tmp;
        dispatch(s, pos1);
        tree = tree->cdr;
      }
      if (val) {
        uint32_t pos = cursp();
        genop_1(s, OP_LOADNIL, cursp());
        if (pos3 != JMPLINK_START) dispatch_linked(s, pos3);
        if (head) pop();
        if (cursp() != pos) {
          gen_move(s, cursp(), pos, 0);
        }
        push();
      }
      else {
        if (pos3 != JMPLINK_START) {
          dispatch_linked(s, pos3);
        }
        if (head) {
          pop();
        }
      }
    }
    break;

  case NODE_SCOPE:
    scope_body(s, tree, NOVAL);
    break;

  case NODE_CALL:
  case NODE_FCALL:
    gen_call(s, tree, val, 0);
    break;
  case NODE_SCALL:
    gen_call(s, tree, val, 1);
    break;

  case NODE_DOT2:
    codegen(s, tree->car, val);
    codegen(s, tree->cdr, val);
    if (val) {
      pop(); pop();
      genop_1(s, OP_RANGE_INC, cursp());
      push();
    }
    break;

  case NODE_DOT3:
    codegen(s, tree->car, val);
    codegen(s, tree->cdr, val);
    if (val) {
      pop(); pop();
      genop_1(s, OP_RANGE_EXC, cursp());
      push();
    }
    break;

  case NODE_COLON2:
    {
      int sym = new_sym(s, nsym(tree->cdr));

      codegen(s, tree->car, VAL);
      pop();
      genop_2(s, OP_GETMCNST, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_COLON3:
    {
      int sym = new_sym(s, nsym(tree));

      genop_1(s, OP_OCLASS, cursp());
      genop_2(s, OP_GETMCNST, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_ARRAY:
    {
      int n;

      n = gen_values(s, tree, val, 0);
      if (val) {
        if (n >= 0) {
          pop_n(n);
          genop_2(s, OP_ARRAY, cursp(), n);
        }
        push();
      }
    }
    break;

  case NODE_HASH:
  case NODE_KW_HASH:
    {
      int nk = gen_hash(s, tree, val, GEN_LIT_ARY_MAX);
      if (val && nk >= 0) {
        pop_n(nk*2);
        genop_2(s, OP_HASH, cursp(), nk);
        push();
      }
    }
    break;

  case NODE_SPLAT:
    codegen(s, tree, val);
    break;

  case NODE_ASGN:
    gen_assignment(s, tree->car, tree->cdr, 0, val);
    break;

  case NODE_MASGN:
    {
      int len = 0, n = 0, post = 0;
      node *t = tree->cdr, *p;
      int rhs = cursp();

      if (!val && nint(t->car) == NODE_ARRAY && t->cdr && nosplat(t->cdr)) {
        /* fixed rhs */
        t = t->cdr;
        while (t) {
          codegen(s, t->car, VAL);
          len++;
          t = t->cdr;
        }
        tree = tree->car;
        if (tree->car) {                /* pre */
          t = tree->car;
          n = 0;
          while (t) {
            if (n < len) {
              gen_assignment(s, t->car, NULL, rhs+n, NOVAL);
              n++;
            }
            else {
              genop_1(s, OP_LOADNIL, rhs+n);
              gen_assignment(s, t->car, NULL, rhs+n, NOVAL);
            }
            t = t->cdr;
          }
        }
        t = tree->cdr;
        if (t) {
          if (t->cdr) {         /* post count */
            p = t->cdr->car;
            while (p) {
              post++;
              p = p->cdr;
            }
          }
          if (t->car) {         /* rest (len - pre - post) */
            int rn;

            if (len < post + n) {
              rn = 0;
            }
            else {
              rn = len - post - n;
            }
            if (cursp() == rhs+n) {
              genop_2(s, OP_ARRAY, cursp(), rn);
            }
            else {
              genop_3(s, OP_ARRAY2, cursp(), rhs+n, rn);
            }
            gen_assignment(s, t->car, NULL, cursp(), NOVAL);
            n += rn;
          }
          if (t->cdr && t->cdr->car) {
            t = t->cdr->car;
            while (t) {
              if (n<len) {
                gen_assignment(s, t->car, NULL, rhs+n, NOVAL);
              }
              else {
                genop_1(s, OP_LOADNIL, cursp());
                gen_assignment(s, t->car, NULL, cursp(), NOVAL);
              }
              t = t->cdr;
              n++;
            }
          }
        }
        pop_n(len);
      }
      else {
        /* variable rhs */
        codegen(s, t, VAL);
        gen_massignment(s, tree->car, rhs, val);
        if (!val) {
          pop();
        }
      }
    }
    break;

  case NODE_OP_ASGN:
    {
      mrb_sym sym = nsym(tree->cdr->car);
      mrb_int len;
      const char *name = mrb_sym_name_len(s->mrb, sym, &len);
      int idx, callargs = -1, vsp = -1;

      if ((len == 2 && name[0] == '|' && name[1] == '|') &&
          (nint(tree->car->car) == NODE_CONST ||
           nint(tree->car->car) == NODE_CVAR)) {
        int catch_entry, begin, end;
        int noexc, exc;
        struct loopinfo *lp;

        lp = loop_push(s, LOOP_BEGIN);
        lp->pc0 = new_label(s);
        catch_entry = catch_handler_new(s);
        begin = s->pc;
        exc = cursp();
        codegen(s, tree->car, VAL);
        end = s->pc;
        noexc = genjmp_0(s, OP_JMP);
        lp->type = LOOP_RESCUE;
        catch_handler_set(s, catch_entry, MRB_CATCH_RESCUE, begin, end, s->pc);
        genop_1(s, OP_EXCEPT, exc);
        genop_1(s, OP_LOADF, exc);
        dispatch(s, noexc);
        loop_pop(s, NOVAL);
      }
      else if (nint(tree->car->car) == NODE_CALL) {
        node *n = tree->car->cdr;
        int base, i, nargs = 0;
        callargs = 0;

        if (val) {
          vsp = cursp();
          push();
        }
        codegen(s, n->car, VAL);   /* receiver */
        idx = new_sym(s, nsym(n->cdr->car));
        base = cursp()-1;
        if (n->cdr->cdr->car) {
          nargs = gen_values(s, n->cdr->cdr->car->car, VAL, 13);
          if (nargs >= 0) {
            callargs = nargs;
          }
          else { /* varargs */
            push();
            nargs = 1;
            callargs = CALL_MAXARGS;
          }
        }
        /* copy receiver and arguments */
        gen_move(s, cursp(), base, 1);
        for (i=0; i<nargs; i++) {
          gen_move(s, cursp()+i+1, base+i+1, 1);
        }
        push_n(nargs+2);pop_n(nargs+2); /* space for receiver, arguments and a block */
        genop_3(s, OP_SEND, cursp(), idx, callargs);
        push();
      }
      else {
        codegen(s, tree->car, VAL);
      }
      if (len == 2 &&
          ((name[0] == '|' && name[1] == '|') ||
           (name[0] == '&' && name[1] == '&'))) {
        uint32_t pos;

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
        codegen(s, tree->cdr->cdr->car, VAL);
        pop();
        if (val && vsp >= 0) {
          gen_move(s, vsp, cursp(), 1);
        }
        if (nint(tree->car->car) == NODE_CALL) {
          if (callargs == CALL_MAXARGS) {
            pop();
            genop_2(s, OP_ARYPUSH, cursp(), 1);
          }
          else {
            pop_n(callargs);
            callargs++;
          }
          pop();
          idx = new_sym(s, attrsym(s, nsym(tree->car->cdr->cdr->car)));
          genop_3(s, OP_SEND, cursp(), idx, callargs);
        }
        else {
          gen_assignment(s, tree->car, NULL, cursp(), val);
        }
        dispatch(s, pos);
        goto exit;
      }
      codegen(s, tree->cdr->cdr->car, VAL);
      push(); pop();
      pop(); pop();

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
        idx = new_sym(s, sym);
        genop_3(s, OP_SEND, cursp(), idx, 1);
      }
      if (callargs < 0) {
        gen_assignment(s, tree->car, NULL, cursp(), val);
      }
      else {
        if (val && vsp >= 0) {
          gen_move(s, vsp, cursp(), 0);
        }
        if (callargs == CALL_MAXARGS) {
          pop();
          genop_2(s, OP_ARYPUSH, cursp(), 1);
        }
        else {
          pop_n(callargs);
          callargs++;
        }
        pop();
        idx = new_sym(s, attrsym(s,nsym(tree->car->cdr->cdr->car)));
        genop_3(s, OP_SEND, cursp(), idx, callargs);
      }
    }
    break;

  case NODE_SUPER:
    {
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
        node *args = tree->car;
        if (args) {
          st = n = gen_values(s, args, VAL, 14);
          if (n < 0) {
            st = 1; n = 15;
            push();
          }
        }
        /* keyword arguments */
        if (tree->cdr->car) {
          nk = gen_hash(s, tree->cdr->car->cdr, VAL, 14);
          if (nk < 0) {st++; nk = 15;}
          else st += nk*2;
          n |= nk<<4;
        }
        /* block arguments */
        if (tree->cdr->cdr) {
          codegen(s, tree->cdr->cdr, VAL);
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
    break;

  case NODE_ZSUPER:
    {
      codegen_scope *s2 = s;
      int lv = 0;
      uint16_t ainfo = 0;
      int n = CALL_MAXARGS;
      int sp = cursp();

      push();        /* room for receiver */
      while (!s2->mscope) {
        lv++;
        s2 = s2->prev;
        if (!s2) break;
      }
      if (s2 && s2->ainfo > 0) {
        ainfo = s2->ainfo;
      }
      if (lv > 0xf) codegen_error(s, "too deep nesting");
      if (ainfo > 0) {
        genop_2S(s, OP_ARGARY, cursp(), (ainfo<<4)|(lv & 0xf));
        push(); push(); push();   /* ARGARY pushes 3 values at most */
        pop(); pop(); pop();
        /* keyword arguments */
        if (ainfo & 0x1) {
          n |= CALL_MAXARGS<<4;
          push();
        }
        /* block argument */
        if (tree && tree->cdr && tree->cdr->cdr) {
          push();
          codegen(s, tree->cdr->cdr, VAL);
        }
      }
      else {
        /* block argument */
        if (tree && tree->cdr && tree->cdr->cdr) {
          codegen(s, tree->cdr->cdr, VAL);
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
    break;

  case NODE_RETURN:
    if (tree) {
      gen_retval(s, tree);
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
    if (val) push();
    break;

  case NODE_YIELD:
    {
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
      if (tree) {
        if (tree->car) {
          n = gen_values(s, tree->car, VAL, 14);
          if (n < 0) {
            n = sendv = 1;
            push();
          }
        }

        if (tree->cdr->car) {
          nk = gen_hash(s, tree->cdr->car->cdr, VAL, 14);
          if (nk < 0) {
            nk = 15;
          }
        }
      }
      push();pop(); /* space for a block */
      pop_n(n + (nk == 15 ? 1 : nk * 2) + 1);
      genop_2S(s, OP_BLKPUSH, cursp(), (ainfo<<4)|(lv & 0xf));
      if (sendv) n = CALL_MAXARGS;
      genop_3(s, OP_SEND, cursp(), new_sym(s, MRB_SYM_2(s->mrb, call)), n|(nk<<4));
      if (val) push();
    }
    break;

  case NODE_BREAK:
    loop_break(s, tree);
    if (val) push();
    break;

  case NODE_NEXT:
    if (!s->loop) {
      raise_error(s, "unexpected next");
    }
    else if (s->loop->type == LOOP_NORMAL) {
      codegen(s, tree, NOVAL);
      genjmp(s, OP_JMPUW, s->loop->pc0);
    }
    else {
      if (tree) {
        codegen(s, tree, VAL);
        pop();
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
      }
      gen_return(s, OP_RETURN, cursp());
    }
    if (val) push();
    break;

  case NODE_REDO:
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
    if (val) push();
    break;

  case NODE_RETRY:
    {
      const struct loopinfo *lp = s->loop;

      while (lp && lp->type != LOOP_RESCUE) {
        lp = lp->prev;
      }
      if (!lp) {
        raise_error(s, "unexpected retry");
        break;
      }
      else {
        genjmp(s, OP_JMPUW, lp->pc0);
      }
      if (val) push();
    }
    break;

  case NODE_LVAR:
    if (val) {
      int idx = lv_idx(s, nsym(tree));

      if (idx > 0) {
        gen_move(s, cursp(), idx, val);
      }
      else {
        gen_getupvar(s, cursp(), nsym(tree));
      }
      push();
    }
    break;

  case NODE_NVAR:
    if (val) {
      int idx = nint(tree);

      gen_move(s, cursp(), idx, val);

      push();
    }
    break;

  case NODE_GVAR:
    {
      int sym = new_sym(s, nsym(tree));

      genop_2(s, OP_GETGV, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_IVAR:
    {
      int sym = new_sym(s, nsym(tree));

      genop_2(s, OP_GETIV, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_CVAR:
    {
      int sym = new_sym(s, nsym(tree));

      genop_2(s, OP_GETCV, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_CONST:
    {
      int sym = new_sym(s, nsym(tree));

      genop_2(s, OP_GETCONST, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_BACK_REF:
    if (val) {
      char buf[] = {'$', nchar(tree)};
      int sym = new_sym(s, mrb_intern(s->mrb, buf, sizeof(buf)));

      genop_2(s, OP_GETGV, cursp(), sym);
      push();
    }
    break;

  case NODE_NTH_REF:
    if (val) {
      mrb_state *mrb = s->mrb;
      mrb_value str;
      int sym;

      str = mrb_format(mrb, "$%d", nint(tree));
      sym = new_sym(s, mrb_intern_str(mrb, str));
      genop_2(s, OP_GETGV, cursp(), sym);
      push();
    }
    break;

  case NODE_ARG:
    /* should not happen */
    break;

  case NODE_BLOCK_ARG:
    if (!tree) {
      int idx = lv_idx(s, MRB_OPSYM_2(s->mrb, and));

      if (idx == 0) {
        gen_getupvar(s, cursp(), MRB_OPSYM_2(s->mrb, and));
      }
      else {
        gen_move(s, cursp(), idx, val);
      }
      if (val) push();
    }
    else {
      codegen(s, tree, val);
    }
    break;

  case NODE_INT:
    if (val) {
      char *p = (char*)tree->car;
      int base = nint(tree->cdr->car);
      mrb_int i;
      mrb_bool overflow;

      i = readint(s, p, base, FALSE, &overflow);
      if (overflow) {
        int off = new_litbint(s, p, base);
        genop_2(s, OP_LOADL, cursp(), off);
      }
      else {
        gen_int(s, cursp(), i);
      }
      push();
    }
    break;

#ifndef MRB_NO_FLOAT
  case NODE_FLOAT:
    if (val) {
      char *p = (char*)tree;
      double f;
      mrb_read_float(p, NULL, &f);
      int off = new_lit_float(s, (mrb_float)f);

      genop_2(s, OP_LOADL, cursp(), off);
      push();
    }
    break;
#endif

  case NODE_NEGATE:
    {
      nt = nint(tree->car);
      switch (nt) {
#ifndef MRB_NO_FLOAT
      case NODE_FLOAT:
        if (val) {
          char *p = (char*)tree->cdr;
          double f;
          mrb_read_float(p, NULL, &f);
          int off = new_lit_float(s, (mrb_float)-f);

          genop_2(s, OP_LOADL, cursp(), off);
          push();
        }
        break;
#endif

      case NODE_INT:
        if (val) {
          char *p = (char*)tree->cdr->car;
          int base = nint(tree->cdr->cdr->car);
          mrb_int i;
          mrb_bool overflow;

          i = readint(s, p, base, TRUE, &overflow);
          if (overflow) {
            base = -base;
            int off = new_litbint(s, p, base);
            genop_2(s, OP_LOADL, cursp(), off);
          }
          else {
            gen_int(s, cursp(), i);
          }
          push();
        }
        break;

      default:
        codegen(s, tree, VAL);
        pop();
        push_n(2);pop_n(2); /* space for receiver&block */
        mrb_sym minus = MRB_OPSYM_2(s->mrb, minus);
        if (!gen_uniop(s, minus, cursp())) {
          genop_3(s, OP_SEND, cursp(), new_sym(s, minus), 0);
        }
        if (val) push();
        break;
      }
    }
    break;

  case NODE_STR:
    if (val) {
      char *p = (char*)tree->car;
      mrb_int len = nint(tree->cdr);
      int off = new_lit_str(s, p, len);

      genop_2(s, OP_STRING, cursp(), off);
      push();
    }
    break;

  case NODE_HEREDOC:
    tree = ((struct mrb_parser_heredoc_info*)tree)->doc;
    /* fall through */
  case NODE_DSTR:
    if (val) {
      node *n = tree;

      if (!n) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
        break;
      }
      codegen(s, n->car, VAL);
      n = n->cdr;
      while (n) {
        codegen(s, n->car, VAL);
        pop(); pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
        n = n->cdr;
      }
    }
    else {
      node *n = tree;

      while (n) {
        if (nint(n->car->car) != NODE_STR) {
          codegen(s, n->car, NOVAL);
        }
        n = n->cdr;
      }
    }
    break;

  case NODE_WORDS:
    gen_literal_array(s, tree, FALSE, val);
    break;

  case NODE_SYMBOLS:
    gen_literal_array(s, tree, TRUE, val);
    break;

  case NODE_DXSTR:
    {
      node *n;
      int sym = new_sym(s, MRB_SYM_2(s->mrb, Kernel));

      push();
      codegen(s, tree->car, VAL);
      n = tree->cdr;
      while (n) {
        if (nint(n->car->car) == NODE_XSTR) {
          n->car->car = (struct mrb_ast_node*)(intptr_t)NODE_STR;
          mrb_assert(!n->cdr); /* must be the end */
        }
        codegen(s, n->car, VAL);
        pop(); pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
        n = n->cdr;
      }
      push();                   /* for block */
      pop_n(3);
      sym = new_sym(s, MRB_OPSYM_2(s->mrb, tick)); /* ` */
      genop_3(s, OP_SSEND, cursp(), sym, 1);
      if (val) push();
    }
    break;

  case NODE_XSTR:
    {
      char *p = (char*)tree->car;
      mrb_int len = nint(tree->cdr);
      int off = new_lit_str(s, p, len);
      int sym;

      push();
      genop_2(s, OP_STRING, cursp(), off);
      push(); push();
      pop_n(3);
      sym = new_sym(s, MRB_OPSYM_2(s->mrb, tick)); /* ` */
      genop_3(s, OP_SSEND, cursp(), sym, 1);
      if (val) push();
    }
    break;

  case NODE_REGX:
    if (val) {
      char *p1 = (char*)tree->car;
      char *p2 = (char*)tree->cdr->car;
      char *p3 = (char*)tree->cdr->cdr;
      int sym = new_sym(s, mrb_intern_lit(s->mrb, REGEXP_CLASS));
      int off = new_lit_cstr(s, p1);
      int argc = 1;

      genop_1(s, OP_OCLASS, cursp());
      genop_2(s, OP_GETMCNST, cursp(), sym);
      push();
      genop_2(s, OP_STRING, cursp(), off);
      push();
      if (p2 || p3) {
        if (p2) { /* opt */
          off = new_lit_cstr(s, p2);
          genop_2(s, OP_STRING, cursp(), off);
        }
        else {
          genop_1(s, OP_LOADNIL, cursp());
        }
        push();
        argc++;
        if (p3) { /* enc */
          off = new_lit_str(s, p3, 1);
          genop_2(s, OP_STRING, cursp(), off);
          push();
          argc++;
        }
      }
      push(); /* space for a block */
      pop_n(argc+2);
      sym = new_sym(s, MRB_SYM_2(s->mrb, compile));
      genop_3(s, OP_SEND, cursp(), sym, argc);
      push();
    }
    break;

  case NODE_DREGX:
    if (val) {
      node *n = tree->car;
      int sym = new_sym(s, mrb_intern_lit(s->mrb, REGEXP_CLASS));
      int argc = 1;
      int off;
      char *p;

      genop_1(s, OP_OCLASS, cursp());
      genop_2(s, OP_GETMCNST, cursp(), sym);
      push();
      codegen(s, n->car, VAL);
      n = n->cdr;
      while (n) {
        codegen(s, n->car, VAL);
        pop(); pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
        n = n->cdr;
      }
      n = tree->cdr->cdr;
      if (n->car) { /* tail */
        p = (char*)n->car;
        off = new_lit_cstr(s, p);
        codegen(s, tree->car, VAL);
        genop_2(s, OP_STRING, cursp(), off);
        pop();
        genop_1(s, OP_STRCAT, cursp());
        push();
      }
      if (n->cdr->car) { /* opt */
        char *p2 = (char*)n->cdr->car;
        off = new_lit_cstr(s, p2);
        genop_2(s, OP_STRING, cursp(), off);
        push();
        argc++;
      }
      if (n->cdr->cdr) { /* enc */
        char *p2 = (char*)n->cdr->cdr;
        off = new_lit_cstr(s, p2);
        genop_2(s, OP_STRING, cursp(), off);
        push();
        argc++;
      }
      push(); /* space for a block */
      pop_n(argc+2);
      sym = new_sym(s, MRB_SYM_2(s->mrb, compile));
      genop_3(s, OP_SEND, cursp(), sym, argc);
      push();
    }
    else {
      node *n = tree->car;

      while (n) {
        if (nint(n->car->car) != NODE_STR) {
          codegen(s, n->car, NOVAL);
        }
        n = n->cdr;
      }
    }
    break;

  case NODE_SYM:
    if (val) {
      int sym = new_sym(s, nsym(tree));

      genop_2(s, OP_LOADSYM, cursp(), sym);
      push();
    }
    break;

  case NODE_DSYM:
    codegen(s, tree, val);
    if (val) {
      gen_intern(s);
    }
    break;

  case NODE_SELF:
    if (val) {
      genop_1(s, OP_LOADSELF, cursp());
      push();
    }
    break;

  case NODE_NIL:
    if (val) {
      genop_1(s, OP_LOADNIL, cursp());
      push();
    }
    break;

  case NODE_TRUE:
    if (val) {
      genop_1(s, OP_LOADT, cursp());
      push();
    }
    break;

  case NODE_FALSE:
    if (val) {
      genop_1(s, OP_LOADF, cursp());
      push();
    }
    break;

  case NODE_ALIAS:
    {
      int a = new_sym(s, nsym(tree->car));
      int b = new_sym(s, nsym(tree->cdr));

      genop_2(s, OP_ALIAS, a, b);
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
    }
   break;

  case NODE_UNDEF:
    {
      node *t = tree;

      while (t) {
        int symbol = new_sym(s, nsym(t->car));
        genop_1(s, OP_UNDEF, symbol);
        t = t->cdr;
      }
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
    }
    break;

  case NODE_CLASS:
    {
      int idx;
      node *body;

      if (tree->car->car == (node*)0) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      else if (tree->car->car == (node*)1) {
        genop_1(s, OP_OCLASS, cursp());
        push();
      }
      else {
        codegen(s, tree->car->car, VAL);
      }
      if (tree->cdr->car) {
        codegen(s, tree->cdr->car, VAL);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      pop(); pop();
      idx = new_sym(s, nsym(tree->car->cdr));
      genop_2(s, OP_CLASS, cursp(), idx);
      body = tree->cdr->cdr->car;
      if (nint(body->cdr->car) == NODE_STMTS && body->cdr->cdr == NULL) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, body, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
    }
    break;

  case NODE_MODULE:
    {
      int idx;

      if (tree->car->car == (node*)0) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      else if (tree->car->car == (node*)1) {
        genop_1(s, OP_OCLASS, cursp());
        push();
      }
      else {
        codegen(s, tree->car->car, VAL);
      }
      pop();
      idx = new_sym(s, nsym(tree->car->cdr));
      genop_2(s, OP_MODULE, cursp(), idx);
      if (nint(tree->cdr->car->cdr->car) == NODE_STMTS &&
          tree->cdr->car->cdr->cdr == NULL) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, tree->cdr->car, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
    }
    break;

  case NODE_SCLASS:
    {
      int idx;

      codegen(s, tree->car, VAL);
      pop();
      genop_1(s, OP_SCLASS, cursp());
      if (nint(tree->cdr->car->cdr->car) == NODE_STMTS &&
          tree->cdr->car->cdr->cdr == NULL) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, tree->cdr->car, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
    }
    break;

  case NODE_DEF:
    {
      int sym = new_sym(s, nsym(tree->car));
      int idx = lambda_body(s, tree->cdr, 0);

      genop_1(s, OP_TCLASS, cursp());
      push();
      genop_2(s, OP_METHOD, cursp(), idx);
      push(); pop();
      pop();
      genop_2(s, OP_DEF, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_SDEF:
    {
      node *recv = tree->car;
      int sym = new_sym(s, nsym(tree->cdr->car));
      int idx = lambda_body(s, tree->cdr->cdr, 0);

      codegen(s, recv, VAL);
      pop();
      genop_1(s, OP_SCLASS, cursp());
      push();
      genop_2(s, OP_METHOD, cursp(), idx);
      push(); pop();
      pop();
      genop_2(s, OP_DEF, cursp(), sym);
      if (val) push();
    }
    break;

  case NODE_POSTEXE:
    codegen(s, tree, NOVAL);
    break;

  default:
    break;
  }
 exit:
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
  s->sp += node_len(nlv)+1;        /* add self */
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
  return generate_code(mrb, p, VAL);
}
