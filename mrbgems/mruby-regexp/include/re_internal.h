/*
** re_internal.h - internal definitions for regexp engine
**
** See Copyright Notice in mruby.h
*/

#ifndef MRB_RE_INTERNAL_H
#define MRB_RE_INTERNAL_H

#include <mruby.h>
#include <stdint.h>

/* Bytecode instructions for the NFA engine */
enum re_opcode {
  RE_CHAR,       /* match literal byte: operand = byte value */
  RE_ANY,        /* match any character (. without DOTALL) */
  RE_ANY_NL,     /* match any character including newline (. with DOTALL) */
  RE_CLASS,      /* match character class: operand = class_id */
  RE_NCLASS,     /* match negated character class: operand = class_id */
  RE_MATCH,      /* successful match */
  RE_JMP,        /* unconditional jump: operand = target offset */
  RE_SPLIT,      /* fork: operand = target offset (greedy: try next first) */
  RE_SPLITNG,    /* fork: operand = target offset (non-greedy: try jump first) */
  RE_SAVE,       /* save capture position: operand = slot number */
  RE_BOL,        /* assert beginning of line (^) */
  RE_EOL,        /* assert end of line ($) */
  RE_BOT,        /* assert beginning of text (\A) */
  RE_EOT,        /* assert end of text (\z) */
  RE_EOTNL,     /* assert end of text or before final \n (\Z) */
  RE_WBOUND,     /* assert word boundary (\b) */
  RE_NWBOUND,    /* assert non-word boundary (\B) */
  RE_BACKREF,    /* backreference: operand = group number */
  RE_LOOKAHEAD,  /* positive lookahead: offset = end of sub-pattern */
  RE_NEG_LOOKAHEAD, /* negative lookahead: offset = end of sub-pattern */
  RE_LOOKBEHIND,     /* positive lookbehind: a = byte length, offset = end */
  RE_NEG_LOOKBEHIND, /* negative lookbehind: a = byte length, offset = end */
};

/* Bytecode instruction (4 bytes each for alignment) */
typedef struct {
  uint8_t op;
  uint8_t a;       /* small operand or class id */
  uint16_t offset;  /* jump target or extended operand */
} re_inst;

/* Character class bitmap (ASCII range) */
#define RE_CLASS_BITMAP_SIZE 16  /* 128 bits = 16 bytes for ASCII */
typedef struct {
  uint8_t bitmap[RE_CLASS_BITMAP_SIZE];  /* bitmap for 0-127 */
  mrb_bool negated;
  mrb_bool utf8_any;  /* match any non-ASCII byte if true */
} re_charclass;

/* Named capture entry */
typedef struct {
  const char *name;
  uint16_t name_len;
  uint16_t group;
} re_named_capture;

/* Compiled regexp pattern */
typedef struct mrb_regexp_pattern {
  re_inst *code;          /* bytecode array */
  uint32_t code_len;      /* number of instructions */
  re_charclass *classes;   /* character class table */
  uint16_t num_classes;
  uint16_t num_captures;   /* number of capture groups (including group 0) */
  uint32_t flags;
  re_named_capture *named_captures;
  uint16_t num_named;
  mrb_bool has_backref;    /* true if pattern uses \1-\9 */
  mrb_bool needs_backtrack; /* true if pattern needs backtracking engine */
} mrb_regexp_pattern;

/* Regexp flags */
#define RE_FLAG_IGNORECASE  1
#define RE_FLAG_MULTILINE   2  /* ^ and $ match at \n boundaries */
#define RE_FLAG_DOTALL      4  /* . matches \n (Ruby's /m for dot behavior) */
#define RE_FLAG_EXTENDED    8  /* ignore whitespace and #comments in pattern */

/* Note: Ruby's /m flag means BOTH multiline anchors AND dotall.
   Ruby's /i flag is ignorecase.  Ruby's /x flag is extended. */

/* Step limit for ReDoS protection */
#ifndef MRB_REGEXP_STEP_LIMIT
#define MRB_REGEXP_STEP_LIMIT 1000000
#endif

/* Maximum captures */
#define RE_MAX_CAPTURES 32

/* Compile a pattern string into bytecode */
mrb_regexp_pattern* re_compile(mrb_state *mrb, const char *pattern, mrb_int len, uint32_t flags);

/* Free a compiled pattern */
void re_free(mrb_state *mrb, mrb_regexp_pattern *pat);

/* Execute a match.
   Returns number of captures filled (0 = no match).
   captures[2*n] = start, captures[2*n+1] = end for group n. */
int re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
            const char *str, mrb_int len, mrb_int start,
            int *captures, int captures_size);

/* UTF-8 helpers */
int re_utf8_charlen(const char *s, const char *end);
uint32_t re_utf8_decode(const char *s, int *len);
mrb_bool re_is_word_char(uint32_t c);

#endif /* MRB_RE_INTERNAL_H */
