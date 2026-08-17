/*
** re_internal.h - internal definitions for regexp engine
**
** See Copyright Notice in mruby.h
*/

#ifndef MRB_RE_INTERNAL_H
#define MRB_RE_INTERNAL_H

#include <mruby.h>
#include <mruby/internal.h>
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
  RE_BACKREF,    /* backreference: a = group number, offset = 1 if case-insensitive */
  RE_LOOKAHEAD,  /* positive lookahead: offset = end of sub-pattern */
  RE_NEG_LOOKAHEAD, /* negative lookahead: offset = end of sub-pattern */
  RE_LOOKBEHIND,     /* positive lookbehind: a = byte count, offset = end */
  RE_NEG_LOOKBEHIND, /* negative lookbehind: a = byte count, offset = end */
  RE_LB_WIDTH,       /* carrier after either lookbehind: a = character count.
                        The executor rewinds by bytes against a binary subject
                        and by characters otherwise, and the sub-pattern body
                        starts past this instruction, at pc + 2. */
};

/* Bytecode instruction (4 bytes each for alignment) */
typedef struct {
  uint8_t op;
  uint8_t a;       /* small operand or class id */
  uint16_t offset;  /* jump target or extended operand */
} re_inst;

/* Character class bitmap (ASCII range) */
#define RE_CLASS_BITMAP_SIZE 16  /* 128 bits = 16 bytes for ASCII */

/* A class member that is a byte rather than a codepoint, held in the same
   range list with this bit set. A pattern byte that starts no whole character
   is a byte, and the two spaces collide over U+0080 to U+00FF: the byte 0xB5
   and the character U+00B5 both arrive as the number 0xB5, so the number alone
   cannot say which was written. The tag sits above every codepoint, so a
   tagged range can never overlap an untagged one. */
#define RE_CLASS_BYTE 0x80000000u

typedef struct {
  uint8_t bitmap[RE_CLASS_BITMAP_SIZE];  /* bitmap for 0-127 */
  /* Non-ASCII codepoint ranges, and byte ranges tagged with RE_CLASS_BYTE.
     Stored as flat (lo, hi) pairs: ranges[2k] = lo, ranges[2k+1] = hi
     (inclusive). NULL when the class has no non-ASCII members (the common
     case). */
  uint32_t *ranges;
  uint32_t num_ranges;
  uint32_t range_capa;
  mrb_bool negated;
  mrb_bool utf8_any;  /* match any non-ASCII byte if true */
} re_charclass;

/* Longest capture name that fits in re_named_capture::name_len. The bound
   sits beside the field it describes so a later change of width is one line.
   The widening inside the macro is not cosmetic: on a target whose argument
   type is no wider than the field, the naive comparison is against a constant
   that type cannot exceed, and the compiler reports it. */
#define RE_MAX_NAME_LEN UINT32_MAX
#define RE_NAME_LEN_FITS(n) ((uintmax_t)(n) <= RE_MAX_NAME_LEN)

/* Named capture entry */
typedef struct {
  const char *name;
  uint32_t name_len;
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
  char *named_arena;       /* owned storage for named_captures[i].name; NULL if num_named == 0 */
  uint16_t num_named;
  mrb_bool has_backref;    /* true if pattern uses \1-\9 */
  mrb_bool needs_backtrack; /* true if pattern needs backtracking engine */
  uint8_t *prefix;         /* literal prefix bytes for fast skip (or NULL) */
  uint8_t prefix_len;      /* length of prefix (0 = no prefix) */
  uint8_t first_bytes[16]; /* bitmap of possible first bytes (128-bit, ASCII) */
  mrb_bool has_first_bytes; /* true if first_bytes is usable for skipping */
  mrb_bool is_literal;     /* true if pattern is pure literal (no metacharacters) */
  uint8_t loop_depth;      /* deepest nesting of repetitions whose body can
                              match empty (see RE_MAX_PASS) */
  /* Cached VM state for pike_vm (avoids malloc per mrb_re_exec call) */
  uint32_t *cached_visited;     /* generation-based visited array */
  void *cached_threads[2];      /* curr/next thread lists */
  int cached_list_capa;         /* capacity of cached thread lists */
  mrb_bool cache_in_use;        /* re-entrancy guard */
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

/* Recursion-depth limit for bt_match: bounds C stack growth on
   patterns like `(?=)+` that recurse without consuming input. */
#ifndef MRB_REGEXP_RECURSION_LIMIT
#define MRB_REGEXP_RECURSION_LIMIT 1000
#endif

/* Maximum captures */
#define RE_MAX_CAPTURES 32

/* Thread struct for Pike VM (also used for cache sizing). `sp` is the
   input position the thread is waiting for; the outer loop only dispatches
   a thread when its sp matches the loop's current sp, otherwise the thread
   is deferred to the next iteration. This keeps multi-byte consumers
   (RE_CLASS over a UTF-8 char, advancing 3 bytes) in sync with single-byte
   consumers (RE_CHAR, advancing 1 byte) without requiring a uniform
   char-step outer loop -- both varieties just enqueue at their own sp+N. */
typedef struct {
  uint32_t pc;
  int cap_slot;
  const char *sp;
} re_thread_cache;

/* A pike_vm step walks a repetition's body once per nesting level, so that a
   loop's final empty iteration can finish even when the closure resumed
   inside the body and already marked that iteration's tail (see add_thread).
   The cap keeps a pathologically nested pattern from growing the thread lists
   with the square of the program; past it, such a pattern keeps the older,
   stale-capture behaviour rather than costing memory. */
#define RE_MAX_PASS 4
#define RE_PASS_SPAN(depth) \
  ((uint32_t)((depth) < RE_MAX_PASS ? (depth) : RE_MAX_PASS) + 1)

/* Capacity of one pike_vm thread list, shared by the VM and by the cache the
   compiler pre-allocates for it so the two cannot drift. An instruction
   enqueues at most one thread per pass, and threads waiting on a later sp are
   carried over from the previous step on top of that. */
#define RE_LIST_CAPA(code_len, depth) \
  ((int)(code_len) * (int)(RE_PASS_SPAN(depth) + 1) + 16)

/* Compile a pattern string into `pat`, which the caller passes zero filled
   and keeps reachable from a GC object for the whole call. Every buffer the
   compile allocates hangs off `pat` from the moment it is allocated, so a
   compile that raises, over a bad pattern or a refused allocation, leaves
   them to mrb_re_free() rather than to the frame the longjmp abandons: the
   zero fill is what lets mrb_re_free() read a pattern that got that far.
   `pat->code_len` is written last and stays zero until the pattern is
   complete. */
void mrb_re_compile(mrb_state *mrb, mrb_regexp_pattern *pat, const char *pattern, mrb_int len, uint32_t flags);

/* Free a compiled pattern */
void mrb_re_free(mrb_state *mrb, mrb_regexp_pattern *pat);

/* Word character (\w) test */
mrb_bool mrb_re_is_word_char(uint32_t c);

/* The two foldings whose result is an ASCII letter. Every build carries them,
   whether or not it has the Unicode table, so that folding "ASCII only" covers
   the whole of the equivalence class an ASCII letter belongs to rather than
   the part of it that is ASCII: without them /k/i would miss U+212A and, the
   sign flipped, [^k] under /i would accept it. */
#define RE_FOLD_LONG_S 0x017F  /* to 's' */
#define RE_FOLD_KELVIN 0x212A  /* to 'k' */

/* The Unicode foldings /i reads are core's table, which only a build reading
   its strings as characters and converting their case by Unicode carries. /i
   therefore folds the way that build's own case conversion does and no other
   way, a pattern read as bytes having no character to fold in the first
   place. */
#if defined(MRB_UTF8_STRING) && !defined(MRB_USE_ASCII_CASE)
# define RE_UNICODE_CASE
#endif

/* Simple case folding: the folded codepoint, or cp itself when it folds to
   nothing else. With RE_UNICODE_CASE that is ASCII plus every 1:1 Unicode
   folding, read off core's table; without it, ASCII plus the two above.
   Neither build folds a codepoint that has no single counterpart to fold to
   (U+FB00 to "ff"). */
uint32_t mrb_re_case_fold(uint32_t cp);

/* True when [lo, hi] holds a codepoint that carries case folding data this
   build does not have. A pattern reaching one of those under /i is refused at
   compile time, since folding ASCII and carrying on would answer wrongly: the
   missing fold shows up as a missed match in `[X]` and, with the sign flipped,
   as a false accept in `[^X]`. The test is having the data rather than being
   foldable, so two kinds fall inside it that no build folds: a source whose
   fold expands into several codepoints (U+FB00 to "ff"), and the uncased
   neighbours the coarse ranges close over. A build with the table compiles
   both and matches them literally, so what the two builds differ in there is
   what they refuse rather than what they answer. A build with the data has
   nothing to refuse, so the test compiles away there. The arguments are
   evaluated at most once, but only by the definition that uses them, so pass
   plain values. */
#ifdef RE_UNICODE_CASE
#define mrb_re_needs_case_data(lo, hi) FALSE
#else
mrb_bool mrb_re_needs_case_data(uint32_t lo, uint32_t hi);
#endif

/* Walking a table takes data only the option build has, and there the table is
   core's: mrb_uni_case_unfold() and the two range walks beside it in
   mruby/internal.h are what the compiler reaches for. Without the option the
   compiler reaches the same two foldings directly, since there are only two. */

static inline int
mrb_re_charlen(const char *s, const char *end, mrb_bool binary)
{
  return binary ? 1 : (int)mrb_enc_charlen(s, end);
}

static inline uint32_t
mrb_re_decode_char(const char *s, const char *end, int *len, mrb_bool binary)
{
  if (binary) {
    if (len) *len = 1;
    return (uint8_t)*s;
  }
  mrb_int n;
  uint32_t cp = mrb_enc_decode(s, end, &n);
  if (len) *len = (int)n;
  return cp;
}

/* TRUE when s points into the middle of a character that starts earlier in
   the string, so it is not a place a match may start at. A byte that looks
   like a continuation byte but follows no lead byte that reaches it belongs
   to no character and stands on its own, and there the head of the character
   covering s is s itself. The matcher asks this at every position it tries,
   so keep the answer for a byte that starts a character here rather than in
   the call. */
static inline mrb_bool
mrb_re_char_interior_p(const char *str, const char *s, const char *end)
{
  if (s >= end || ((uint8_t)*s & 0xC0) != 0x80) return FALSE;
  return mrb_enc_char_head(str, s, end) != s;
}

/* Execute a match.
   Returns number of captures filled (0 = no match).
   captures[2*n] = start, captures[2*n+1] = end for group n. */
int mrb_re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
            const char *str, mrb_int len, mrb_int start,
            int *captures, int captures_size, mrb_bool binary);

/* Execute a match backward: the last match that starts at or before `limit`.
   Answers as mrb_re_exec() does, and clears the capture buffer itself before
   each of the searches it makes, having to make more than one. */
int mrb_re_rexec(mrb_state *mrb, const mrb_regexp_pattern *pat,
            const char *str, mrb_int len, mrb_int limit,
            int *captures, int captures_size, mrb_bool binary);

#endif /* MRB_RE_INTERNAL_H */
