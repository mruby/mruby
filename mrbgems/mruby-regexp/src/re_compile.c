/*
** re_compile.c - regexp pattern compiler
**
** Compiles a regular expression pattern string into bytecode
** for the NFA execution engine.
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"
#include <mruby/error.h>
#include <string.h>

/* Compiler state */
typedef struct {
  mrb_state *mrb;
  const char *src;     /* pattern source */
  const char *src_end;
  const char *p;       /* current position */
  re_inst *code;       /* instruction array */
  uint32_t code_len;
  uint32_t code_capa;
  re_charclass *classes;
  uint16_t num_classes;
  uint16_t class_capa;
  uint16_t num_captures;
  uint32_t flags;
  re_named_capture *named_captures;
  uint16_t num_named;
  mrb_bool has_backref;
  mrb_bool needs_backtrack;
  char *stripped;           /* allocated buffer for x-mode preprocessing */
} re_compiler;

static void compile_alt(re_compiler *c);  /* forward */

static void
compile_error(re_compiler *c, const char *msg)
{
  /* Format the message before freeing c->stripped (which may alias c->src
     in extended mode). c->src is not NUL-terminated, so use %l with the
     explicit length from c->src_end. */
  mrb_value emsg = mrb_format(c->mrb, "%s: /%l/",
                              msg, c->src, (size_t)(c->src_end - c->src));

  /* Free compile buffers before raising, since mrb_exc_raise longjmps out
     and the stack-local re_compiler is abandoned without a chance to clean
     up. mrb_free doesn't trigger GC, so emsg stays valid across these. */
  mrb_free(c->mrb, c->code);
  c->code = NULL;
  if (c->classes) {
    for (uint16_t i = 0; i < c->num_classes; i++) {
      mrb_free(c->mrb, c->classes[i].ranges);
    }
    mrb_free(c->mrb, c->classes);
    c->classes = NULL;
  }
  mrb_free(c->mrb, c->named_captures);
  c->named_captures = NULL;
  if (c->stripped) mrb_free(c->mrb, c->stripped);
  c->stripped = NULL;

  mrb_exc_raise(c->mrb,
    mrb_exc_new_str(c->mrb, mrb_exc_get_id(c->mrb, MRB_SYM(RegexpError)), emsg));
}

static uint32_t
emit(re_compiler *c, uint8_t op, uint8_t a, uint16_t offset)
{
  if (c->code_len >= c->code_capa) {
    c->code_capa = c->code_capa ? c->code_capa * 2 : 64;
    c->code = (re_inst*)mrb_realloc(c->mrb, c->code, sizeof(re_inst) * c->code_capa);
  }
  uint32_t pos = c->code_len++;
  c->code[pos].op = op;
  c->code[pos].a = a;
  c->code[pos].offset = offset;
  return pos;
}

static void
patch(re_compiler *c, uint32_t pos, uint16_t offset)
{
  c->code[pos].offset = offset;
}

/* Insert an instruction at position `pos` by shifting code.
   Adjusts jump targets so they still point at the same instructions. */
static void
insert_inst(re_compiler *c, uint32_t pos, uint8_t op, uint8_t a, uint16_t offset)
{
  emit(c, RE_JMP, 0, 0);  /* grow array */
  uint32_t len = c->code_len - 1 - pos;
  memmove(&c->code[pos + 1], &c->code[pos], sizeof(re_inst) * len);
  c->code[pos].op = op;
  c->code[pos].a = a;
  c->code[pos].offset = offset;

  /* Fix jump targets across the insertion. A target past `pos` shifts down by
     one. A target equal to `pos` is ambiguous:
     - code that moved (i > pos) is a backward jump -- e.g. the SPLIT that
       loops `\d+` back to its class -- and meant the instruction now at
       pos+1, so it must follow.
     - code before the insertion (i < pos) is a forward "skip to here"
       reference that should stay on the newly inserted instruction. */
  for (uint32_t i = 0; i < c->code_len; i++) {
    if (i == pos) continue;
    switch (c->code[i].op) {
    case RE_JMP: case RE_SPLIT: case RE_SPLITNG:
      if (c->code[i].offset >= 0xffff) break;
      if (c->code[i].offset > pos || (c->code[i].offset == pos && i > pos)) {
        c->code[i].offset++;
      }
      break;
    default:
      break;
    }
  }
}

static int
peek(re_compiler *c)
{
  if (c->p >= c->src_end) return -1;
  return (uint8_t)*c->p;
}

static int
next_char(re_compiler *c)
{
  if (c->p >= c->src_end) return -1;
  return (uint8_t)*c->p++;
}

/* Class IDs are stored in re_inst.a (uint8_t), so at most 256 distinct
   character classes can be encoded.  Without this cap, class_capa
   (uint16_t) overflows on doubling past 32768 (8 -> 16 -> ... -> 32768
   -> 0), mrb_realloc with size 0 returns NULL, and the next memset
   crashes; even before that, the (uint8_t)id cast at emit sites would
   silently alias different classes. */
#define RE_MAX_CLASSES 256

static uint16_t
add_class(re_compiler *c)
{
  if (c->num_classes >= RE_MAX_CLASSES) {
    compile_error(c, "too many character classes");
  }
  if (c->num_classes >= c->class_capa) {
    c->class_capa = c->class_capa ? c->class_capa * 2 : 8;
    c->classes = (re_charclass*)mrb_realloc(c->mrb, c->classes, sizeof(re_charclass) * c->class_capa);
  }
  uint16_t id = c->num_classes++;
  memset(&c->classes[id], 0, sizeof(re_charclass));
  return id;
}

static void
class_set_bit(re_charclass *cc, uint8_t ch)
{
  if (ch < 128) {
    cc->bitmap[ch >> 3] |= (1 << (ch & 7));
  }
}

/* Append a non-ASCII codepoint range [lo, hi]. Both bounds must be >= 128. */
static void
class_add_range(re_compiler *c, re_charclass *cc, uint32_t lo, uint32_t hi)
{
  if (cc->num_ranges >= cc->range_capa) {
    cc->range_capa = cc->range_capa ? cc->range_capa * 2 : 4;
    cc->ranges = (uint32_t*)mrb_realloc(c->mrb, cc->ranges, sizeof(uint32_t) * 2 * cc->range_capa);
  }
  cc->ranges[2 * cc->num_ranges] = lo;
  cc->ranges[2 * cc->num_ranges + 1] = hi;
  cc->num_ranges++;
}

/* Add a single non-ASCII codepoint to the class. */
static void
class_add_codepoint(re_compiler *c, re_charclass *cc, uint32_t cp)
{
  class_add_range(c, cc, cp, cp);
}

static void
class_set_range(re_charclass *cc, uint8_t lo, uint8_t hi)
{
  for (int i = lo; i <= hi; i++) {
    class_set_bit(cc, (uint8_t)i);
  }
}

static void
class_add_shorthand(re_charclass *cc, int ch)
{
  switch (ch) {
  case 'd':
    class_set_range(cc, '0', '9');
    break;
  case 'D':
    class_set_range(cc, 0, '0'-1);
    class_set_range(cc, '9'+1, 127);
    cc->utf8_any = TRUE;
    break;
  case 'w':
    class_set_range(cc, 'a', 'z');
    class_set_range(cc, 'A', 'Z');
    class_set_range(cc, '0', '9');
    class_set_bit(cc, '_');
    break;
  case 'W':
    for (int i = 0; i < 128; i++) {
      if (!mrb_re_is_word_char(i)) class_set_bit(cc, (uint8_t)i);
    }
    cc->utf8_any = TRUE;
    break;
  case 's':
    class_set_bit(cc, ' ');
    class_set_bit(cc, '\t');
    class_set_bit(cc, '\n');
    class_set_bit(cc, '\r');
    class_set_bit(cc, '\f');
    class_set_bit(cc, '\v');
    break;
  case 'S':
    for (int i = 0; i < 128; i++) {
      if (i != ' ' && i != '\t' && i != '\n' && i != '\r' && i != '\f' && i != '\v')
        class_set_bit(cc, (uint8_t)i);
    }
    cc->utf8_any = TRUE;
    break;
  case 'h':
    /* hex digit: [0-9a-fA-F] */
    class_set_range(cc, '0', '9');
    class_set_range(cc, 'a', 'f');
    class_set_range(cc, 'A', 'F');
    break;
  case 'H':
    /* non-hex-digit: complement of [0-9a-fA-F]. Built as an explicit
       positive set so the top-level dispatcher can emit it as RE_CLASS
       and the `[...]` path can add it directly -- both contexts need the
       complement bits present (the uppercase->RE_NCLASS auto-route used
       by \D/\W/\S is deliberately bypassed for \H). */
    for (int i = 0; i < 128; i++) {
      mrb_bool is_hex = (i >= '0' && i <= '9') ||
                        (i >= 'a' && i <= 'f') ||
                        (i >= 'A' && i <= 'F');
      if (!is_hex) class_set_bit(cc, (uint8_t)i);
    }
    cc->utf8_any = TRUE;
    break;
  }
}

/* Set ASCII bits for a POSIX class name (e.g. "alpha") into a 128-bit map.
   Returns FALSE for an unknown name. Semantics are ASCII, like this gem's
   \w/\d shorthands; non-ASCII codepoints are not classified. */
static mrb_bool
posix_class_bits(uint8_t *bits, const char *name, uint16_t len)
{
#define NAME_IS(s) (len == sizeof(s) - 1 && memcmp(name, s, len) == 0)
#define BSET(ch)   (bits[(ch) >> 3] |= (uint8_t)(1u << ((ch) & 7)))
#define BRANGE(lo, hi) do { for (int i = (lo); i <= (hi); i++) BSET(i); } while (0)
  if (NAME_IS("alpha")) { BRANGE('a','z'); BRANGE('A','Z'); }
  else if (NAME_IS("digit")) { BRANGE('0','9'); }
  else if (NAME_IS("alnum")) { BRANGE('a','z'); BRANGE('A','Z'); BRANGE('0','9'); }
  else if (NAME_IS("upper")) { BRANGE('A','Z'); }
  else if (NAME_IS("lower")) { BRANGE('a','z'); }
  else if (NAME_IS("space")) { BSET(' '); BRANGE('\t','\r'); }
  else if (NAME_IS("blank")) { BSET(' '); BSET('\t'); }
  else if (NAME_IS("xdigit")) { BRANGE('0','9'); BRANGE('a','f'); BRANGE('A','F'); }
  else if (NAME_IS("word")) { BRANGE('a','z'); BRANGE('A','Z'); BRANGE('0','9'); BSET('_'); }
  else if (NAME_IS("cntrl")) { BRANGE(0, 0x1f); BSET(0x7f); }
  else if (NAME_IS("print")) { BRANGE(0x20, 0x7e); }
  else if (NAME_IS("graph")) { BRANGE(0x21, 0x7e); }
  else if (NAME_IS("ascii")) { BRANGE(0, 0x7f); }
  else if (NAME_IS("punct")) {
    for (int i = 0x21; i <= 0x7e; i++) {
      mrb_bool alnum = (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z') ||
                       (i >= '0' && i <= '9');
      if (!alnum) BSET(i);
    }
  }
  else return FALSE;
  return TRUE;
#undef NAME_IS
#undef BSET
#undef BRANGE
}

static int
parse_escape(re_compiler *c)
{
  int ch = next_char(c);
  if (ch < 0) compile_error(c, "trailing backslash");
  switch (ch) {
  case 'n': return '\n';
  case 't': return '\t';
  case 'r': return '\r';
  case 'f': return '\f';
  case 'v': return '\v';
  case 'a': return '\a';
  case 'e': return 0x1b;
  case 'b': return '\b';  /* backspace; only reachable inside [...] since the
                             top-level dispatcher emits RE_WBOUND for `\b` */
  /* Octal escape `\NNN` (1-3 digits, value 0-255). The outer dispatcher
     consumes `\1`-`\9` as backref, so the only octal-leading digit that
     reaches here from the top level is `\0` -- but parse_escape also fires
     from read_class_atom inside `[...]`, where backref parsing does not
     apply, so the full 0-7 range needs handling. */
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7': {
    int val = ch - '0';
    int n = 1;
    while (n < 3) {
      int d = peek(c);
      if (d < '0' || d > '7') break;
      val = val * 8 + (d - '0');
      next_char(c);
      n++;
    }
    return val & 0xff;
  }
  /* Hex escape `\xHH` (1-2 hex digits, value 0-255). The `\x{HHHH}` form
     for codepoints above 0xff is not implemented. */
  case 'x': {
    int val = 0;
    int n = 0;
    while (n < 2) {
      int d = peek(c);
      int v;
      if (d >= '0' && d <= '9') v = d - '0';
      else if (d >= 'a' && d <= 'f') v = d - 'a' + 10;
      else if (d >= 'A' && d <= 'F') v = d - 'A' + 10;
      else break;
      val = val * 16 + v;
      next_char(c);
      n++;
    }
    return val & 0xff;
  }
  default: return ch;  /* literal: \., \\, \/, \(, etc. */
  }
}

/* Read one character class atom: either an ASCII byte (0-127), a
   `\escape`, or a full multi-byte UTF-8 codepoint. Returns the
   codepoint and advances c->p. */
static uint32_t
read_class_atom(re_compiler *c)
{
  if (peek(c) == '\\') {
    next_char(c);
    return (uint32_t)parse_escape(c);
  }
  uint8_t b = (uint8_t)*c->p;
  if (b < 0xC0) {
    /* ASCII or stray continuation byte. */
    return (uint32_t)next_char(c);
  }
  /* Multi-byte UTF-8 leader: decode the full codepoint. */
  int len = 0;
  uint32_t cp = mrb_re_utf8_decode(c->p, c->src_end, &len);
  c->p += len;
  return cp;
}

/* Parse [...] character class */
static void
compile_charclass(re_compiler *c)
{
  uint16_t id = add_class(c);
  re_charclass *cc = &c->classes[id];
  mrb_bool negated = FALSE;

  if (peek(c) == '^') {
    next_char(c);
    negated = TRUE;
  }

  mrb_bool first = TRUE;
  while (peek(c) != ']' || first) {
    if (peek(c) < 0) compile_error(c, "unterminated character class");
    first = FALSE;

    /* POSIX bracket class: [:name:] or negated [:^name:] inside [...]. */
    if (peek(c) == '[' && c->p + 1 < c->src_end && c->p[1] == ':') {
      const char *save = c->p;
      next_char(c);  /* '[' */
      next_char(c);  /* ':' */
      mrb_bool neg = FALSE;
      if (peek(c) == '^') { neg = TRUE; next_char(c); }
      const char *name = c->p;
      while (peek(c) >= 0 && peek(c) != ':' && peek(c) != ']') next_char(c);
      if (peek(c) == ':' && c->p + 1 < c->src_end && c->p[1] == ']') {
        uint8_t bits[16] = {0};
        if (!posix_class_bits(bits, name, (uint16_t)(c->p - name))) {
          compile_error(c, "invalid POSIX bracket class");
        }
        next_char(c);  /* ':' */
        next_char(c);  /* ']' */
        for (int i = 0; i < 128; i++) {
          mrb_bool in = (bits[i >> 3] >> (i & 7)) & 1;
          if (in != neg) class_set_bit(cc, (uint8_t)i);
        }
        if (neg) cc->utf8_any = TRUE;  /* [:^...:] matches non-ASCII too */
        continue;
      }
      c->p = save;  /* not a POSIX class; treat '[' as a literal below */
    }

    /* Shorthand classes (\d, \D, \w, \W, \s, \S, \h, \H) are handled
       before the codepoint-aware path so the single-byte semantics
       stay intact. */
    if (peek(c) == '\\') {
      int esc = (c->p + 1 < c->src_end) ? (uint8_t)c->p[1] : -1;
      if (esc == 'd' || esc == 'D' || esc == 'w' || esc == 'W' ||
          esc == 's' || esc == 'S' || esc == 'h' || esc == 'H') {
        next_char(c);  /* '\\' */
        next_char(c);  /* spec  */
        class_add_shorthand(cc, esc);
        continue;
      }
    }

    uint32_t cp = read_class_atom(c);

    /* check for range a-z (or U+xxxx-U+yyyy) */
    if (peek(c) == '-' && c->p + 1 < c->src_end && c->p[1] != ']') {
      next_char(c);  /* skip '-' */
      uint32_t hi = read_class_atom(c);
      if (cp < 128 && hi < 128) {
        class_set_range(cc, (uint8_t)cp, (uint8_t)hi);
      }
      else {
        /* Range that touches non-ASCII: store as codepoint range.
           Mixed ASCII/non-ASCII ranges are rare; stash the whole span
           in the codepoint list (the bitmap covers ASCII only, so a
           non-ASCII upper bound forces the codepoint path). */
        if (cp <= hi) class_add_range(c, cc, cp, hi);
      }
    }
    else {
      if (cp < 128) class_set_bit(cc, (uint8_t)cp);
      else class_add_codepoint(c, cc, cp);
    }
  }
  next_char(c);  /* skip ']' */

  cc->negated = negated;
  emit(c, negated ? RE_NCLASS : RE_CLASS, (uint8_t)id, 0);
}

/* Maximum value for {n}/{n,m} quantifiers. Each unit becomes (min-1) +
   (max-min) emitted copies of the inner atom; the cap keeps both the
   parse free of integer overflow and the bytecode size sane. */
#define RE_MAX_REPEAT 32768

/* Parse {n}, {n,}, {n,m} quantifier. Returns min,max via pointers. */
static mrb_bool
parse_quantifier(re_compiler *c, int *min_out, int *max_out)
{
  const char *save = c->p;
  int min = 0, max = -1;
  mrb_bool has_digit = FALSE;

  while (peek(c) >= '0' && peek(c) <= '9') {
    min = min * 10 + (next_char(c) - '0');
    has_digit = TRUE;
    if (min > RE_MAX_REPEAT) compile_error(c, "quantifier too large");
  }
  if (peek(c) == ',') {
    next_char(c);
    if (peek(c) >= '0' && peek(c) <= '9') {
      max = 0;
      while (peek(c) >= '0' && peek(c) <= '9') {
        max = max * 10 + (next_char(c) - '0');
        has_digit = TRUE;
        if (max > RE_MAX_REPEAT) compile_error(c, "quantifier too large");
      }
    }
    /* else max = -1 (unlimited) */
  }
  else {
    max = min;  /* {n} means exactly n */
  }
  /* {} and {,} carry no count and are literals in Ruby, not quantifiers. */
  if (!has_digit || peek(c) != '}') {
    c->p = save;  /* not a quantifier, treat { as literal */
    return FALSE;
  }
  next_char(c);  /* skip '}' */
  *min_out = min;
  *max_out = max;
  return TRUE;
}

/*
 * Compute the fixed byte length consumed by bytecode in range [start, end).
 * Returns -1 if the pattern has variable length (quantifiers, alternation
 * with different-length branches, etc.).
 * Used for lookbehind: we need to know exactly how far back to look.
 */
static int
compute_fixed_len(re_compiler *c, uint32_t start, uint32_t end)
{
  int len = 0;
  uint32_t pc = start;

  while (pc < end) {
    re_inst inst = c->code[pc];
    switch (inst.op) {
    case RE_CHAR:
    case RE_CLASS:
    case RE_NCLASS:
      len += 1;
      pc++;
      break;
    case RE_ANY:
    case RE_ANY_NL:
      /* . matches one character which can be 1-4 bytes in UTF-8.
         For ASCII-only mode this is 1 byte; for safety, only allow
         if we can determine it's ASCII context. Return -1 for now. */
      return -1;
    case RE_SAVE:
      pc++;
      break;  /* zero-width */
    case RE_BOL: case RE_EOL: case RE_BOT: case RE_EOT: case RE_EOTNL:
    case RE_WBOUND: case RE_NWBOUND:
      pc++;
      break;  /* zero-width assertions */
    case RE_JMP:
      pc = inst.offset;
      break;
    case RE_SPLIT: {
      /* alternation: both branches must have the same fixed length */
      /* branch 1: pc+1 to next JMP before branch 2 */
      /* branch 2: inst.offset to ... */
      /* For simplicity, reject alternation in lookbehind */
      return -1;
    }
    case RE_MATCH:
      return len;
    default:
      return -1;  /* unknown/variable-length instruction */
    }
  }
  return len;
}

/* Compile a single atom (character, class, group, etc.) */
static void
compile_atom(re_compiler *c)
{
  int ch = peek(c);

  switch (ch) {
  case '(':
    {
      next_char(c);
      mrb_bool capturing = TRUE;

      const char *cap_name = NULL;
      uint16_t cap_name_len = 0;

      if (peek(c) == '?' && c->p + 1 < c->src_end) {
        if (c->p[1] == ':') {
          next_char(c); next_char(c);  /* skip ?: */
          capturing = FALSE;
        }
        else if (c->p[1] == '=' || c->p[1] == '!') {
          /* lookahead (?=...) or (?!...) */
          mrb_bool negative = (c->p[1] == '!');
          next_char(c); next_char(c);  /* skip ?= or ?! */
          uint32_t la_pos = emit(c, negative ? RE_NEG_LOOKAHEAD : RE_LOOKAHEAD, 0, 0);
          compile_alt(c);
          emit(c, RE_MATCH, 0, 0);  /* end of lookahead sub-pattern */
          c->code[la_pos].offset = (uint16_t)c->code_len;  /* patch: skip past sub-pattern */
          if (peek(c) != ')') compile_error(c, "unmatched '('");
          next_char(c);
          c->needs_backtrack = TRUE;  /* needs backtracking engine */
          break;  /* done with this atom */
        }
        else if (c->p[1] == '<' && c->p + 2 < c->src_end && (c->p[2] == '=' || c->p[2] == '!')) {
          /* lookbehind (?<=...) or (?<!...) */
          mrb_bool negative = (c->p[2] == '!');
          next_char(c); next_char(c); next_char(c);  /* skip ?<= or ?<! */
          uint32_t lb_pos = emit(c, negative ? RE_NEG_LOOKBEHIND : RE_LOOKBEHIND, 0, 0);
          uint32_t sub_start = c->code_len;
          compile_alt(c);
          emit(c, RE_MATCH, 0, 0);
          c->code[lb_pos].offset = (uint16_t)c->code_len;

          /* compute fixed byte length of lookbehind sub-pattern */
          int fixed_len = compute_fixed_len(c, sub_start, c->code_len);
          if (fixed_len < 0) {
            compile_error(c, "lookbehind must be fixed length");
          }
          if (fixed_len > 255) {
            compile_error(c, "lookbehind too long (max 255 bytes)");
          }
          c->code[lb_pos].a = (uint8_t)fixed_len;

          if (peek(c) != ')') compile_error(c, "unmatched '('");
          next_char(c);
          c->needs_backtrack = TRUE;  /* needs backtracking engine */
          break;
        }
        else if (c->p[1] == '<' && c->p + 2 < c->src_end && c->p[2] != '=' && c->p[2] != '!') {
          next_char(c); next_char(c);  /* skip ?< */
          cap_name = c->p;
          while (peek(c) != '>' && peek(c) >= 0) next_char(c);
          if (peek(c) != '>') compile_error(c, "unterminated named capture");
          cap_name_len = (uint16_t)(c->p - cap_name);
          next_char(c);  /* skip > */
        }
      }

      uint16_t group = 0;
      if (capturing) {
        if (c->num_captures >= RE_MAX_CAPTURES) {
          compile_error(c, "too many capture groups");
        }
        group = c->num_captures++;
        emit(c, RE_SAVE, 0, group * 2);
        if (cap_name) {
          /* register named capture */
          c->named_captures = (re_named_capture*)mrb_realloc(c->mrb, c->named_captures,
            sizeof(re_named_capture) * (c->num_named + 1));
          c->named_captures[c->num_named].name = cap_name;
          c->named_captures[c->num_named].name_len = cap_name_len;
          c->named_captures[c->num_named].group = group;
          c->num_named++;
        }
      }

      compile_alt(c);

      if (peek(c) != ')') compile_error(c, "unmatched '('");
      next_char(c);

      if (capturing) {
        emit(c, RE_SAVE, 0, group * 2 + 1);
      }
    }
    break;

  case '[':
    next_char(c);
    compile_charclass(c);
    break;

  case '.':
    next_char(c);
    emit(c, (c->flags & RE_FLAG_DOTALL) ? RE_ANY_NL : RE_ANY, 0, 0);
    break;

  case '^':
    next_char(c);
    emit(c, RE_BOL, 0, 0);
    break;

  case '$':
    next_char(c);
    emit(c, RE_EOL, 0, 0);
    break;

  case '\\':
    next_char(c);
    ch = peek(c);
    if (ch >= '1' && ch <= '9') {
      next_char(c);
      emit(c, RE_BACKREF, (uint8_t)(ch - '0'), 0);
      c->has_backref = TRUE;
    }
    else if (ch == 'd' || ch == 'D' || ch == 'w' || ch == 'W' || ch == 's' || ch == 'S') {
      next_char(c);
      uint16_t id = add_class(c);
      class_add_shorthand(&c->classes[id], ch);
      /* class_add_shorthand already builds the complemented set for the
         uppercase forms (\D, \W, \S include utf8_any), so always emit
         RE_CLASS. Emitting RE_NCLASS here would negate a second time and
         make \D/\W/\S behave like \d/\w/\s (issue: negated shorthands). */
      emit(c, RE_CLASS, (uint8_t)id, 0);
    }
    else if (ch == 'h' || ch == 'H') {
      /* \h / \H both carry their full positive set (hex digits /
         non-hex-digits), so emit RE_CLASS for both rather than routing
         \H through an RE_NCLASS path that would double-negate. */
      next_char(c);
      uint16_t id = add_class(c);
      class_add_shorthand(&c->classes[id], ch);
      emit(c, RE_CLASS, (uint8_t)id, 0);
    }
    else if (ch == 'A') {
      next_char(c);
      emit(c, RE_BOT, 0, 0);
    }
    else if (ch == 'z') {
      next_char(c);
      emit(c, RE_EOT, 0, 0);
    }
    else if (ch == 'Z') {
      next_char(c);
      emit(c, RE_EOTNL, 0, 0);
    }
    else if (ch == 'b') {
      next_char(c);
      emit(c, RE_WBOUND, 0, 0);
    }
    else if (ch == 'B') {
      next_char(c);
      emit(c, RE_NWBOUND, 0, 0);
    }
    else if (ch == 'k' && c->p + 1 < c->src_end &&
             (c->p[1] == '<' || c->p[1] == '\'')) {
      /* \k<name> / \k'name': backreference to a named group. Numeric forms
         \k<2> (absolute) and \k<-1> (relative to the groups seen so far) are
         also accepted, like the \g/\k family in Onigmo. */
      next_char(c);  /* skip k */
      int close = (peek(c) == '<') ? '>' : '\'';
      next_char(c);  /* skip < or ' */
      const char *name = c->p;
      while (peek(c) != close && peek(c) >= 0) next_char(c);
      if (peek(c) != close) compile_error(c, "unterminated backreference name");
      uint16_t name_len = (uint16_t)(c->p - name);
      next_char(c);  /* skip the closing > or ' */

      int group = -1;
      if (name_len > 0 && (name[0] == '-' || (name[0] >= '0' && name[0] <= '9'))) {
        mrb_bool relative = (name[0] == '-');
        int n = 0;
        for (uint16_t i = (relative ? 1 : 0); i < name_len; i++) {
          if (name[i] < '0' || name[i] > '9') compile_error(c, "invalid backreference");
          n = n * 10 + (name[i] - '0');
        }
        group = relative ? (int)c->num_captures - n : n;
      }
      else {
        for (uint16_t i = 0; i < c->num_named; i++) {
          if (c->named_captures[i].name_len == name_len &&
              memcmp(c->named_captures[i].name, name, name_len) == 0) {
            group = c->named_captures[i].group;
            break;
          }
        }
      }
      if (group < 1 || group >= (int)c->num_captures) {
        compile_error(c, "undefined group name reference");
      }
      emit(c, RE_BACKREF, (uint8_t)group, 0);
      c->has_backref = TRUE;
    }
    else {
      ch = parse_escape(c);
      if (c->flags & RE_FLAG_IGNORECASE) {
        if (ch >= 'A' && ch <= 'Z') {
          uint16_t id = add_class(c);
          class_set_bit(&c->classes[id], (uint8_t)ch);
          class_set_bit(&c->classes[id], (uint8_t)(ch + 32));
          emit(c, RE_CLASS, (uint8_t)id, 0);
          break;
        }
        else if (ch >= 'a' && ch <= 'z') {
          uint16_t id = add_class(c);
          class_set_bit(&c->classes[id], (uint8_t)ch);
          class_set_bit(&c->classes[id], (uint8_t)(ch - 32));
          emit(c, RE_CLASS, (uint8_t)id, 0);
          break;
        }
      }
      emit(c, RE_CHAR, (uint8_t)ch, 0);
    }
    break;

  case '{':
    {
      /* `{` opens a repeat only as a valid quantifier, which compile_quantified
         consumes after an atom. Reaching it here means there is no atom to
         repeat: a real quantifier (e.g. {2}) is an error, like CRuby, and
         anything else (e.g. {a}, a lone {) is a literal `{`. parse_quantifier
         consumes the `{...}` on success and restores the position on failure,
         so without this case a literal `{` was never consumed and the
         sequence loop spun forever (issue #6914). */
      next_char(c);  /* consume `{` for the trial parse */
      int qmin, qmax;
      if (parse_quantifier(c, &qmin, &qmax)) {
        compile_error(c, "target of repeat operator is not specified");
      }
      emit(c, RE_CHAR, '{', 0);
    }
    break;

  default:
    if (ch < 0 || ch == ')' || ch == '|' || ch == '*' || ch == '+' || ch == '?') {
      return;  /* not an atom */
    }
    next_char(c);
    if ((c->flags & RE_FLAG_IGNORECASE) && ch < 128) {
      if (ch >= 'A' && ch <= 'Z') {
        uint16_t id = add_class(c);
        class_set_bit(&c->classes[id], (uint8_t)ch);
        class_set_bit(&c->classes[id], (uint8_t)(ch + 32));
        emit(c, RE_CLASS, (uint8_t)id, 0);
        break;
      }
      else if (ch >= 'a' && ch <= 'z') {
        uint16_t id = add_class(c);
        class_set_bit(&c->classes[id], (uint8_t)ch);
        class_set_bit(&c->classes[id], (uint8_t)(ch - 32));
        emit(c, RE_CLASS, (uint8_t)id, 0);
        break;
      }
    }
    emit(c, RE_CHAR, (uint8_t)ch, 0);
    break;
  }
}

/* Append a copy of the atom bytecode in [start, start+size) at the current
   position. Internal jump/split targets are relocated to the copy, so a
   repeated group like (a{2,3}){2} keeps each iteration self-contained instead
   of jumping back into the first copy (which corrupted its captures). Capture
   slots (RE_SAVE) are shared across copies on purpose: a repeated group keeps
   only its last iteration, like CRuby. */
static void
emit_atom_copy(re_compiler *c, uint32_t start, uint32_t size)
{
  int32_t delta = (int32_t)c->code_len - (int32_t)start;
  uint32_t atom_end = start + size;
  for (uint32_t j = 0; j < size; j++) {
    re_inst in = c->code[start + j];
    switch (in.op) {
    case RE_JMP: case RE_SPLIT: case RE_SPLITNG:
      if (in.offset >= start && in.offset <= atom_end) {
        in.offset = (uint16_t)((int32_t)in.offset + delta);
      }
      break;
    default:
      break;
    }
    emit(c, in.op, in.a, in.offset);
  }
}

/* Compile atom with quantifiers (*, +, ?, {n,m}) */
static void
compile_quantified(re_compiler *c)
{
  uint32_t start = c->code_len;
  compile_atom(c);
  if (c->code_len == start) return;  /* no atom emitted */

  int ch = peek(c);
  if (ch == '*' || ch == '+' || ch == '?') {
    next_char(c);
    mrb_bool nongreedy = (peek(c) == '?');
    if (nongreedy) {
      next_char(c);
      c->needs_backtrack = TRUE;
    }


    if (ch == '*') {
      /* e* → L: SPLIT(body, end); body; JMP L; end:
         SPLIT offset = end (after JMP), patched after JMP is emitted */
      insert_inst(c, start, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
      emit(c, RE_JMP, 0, start);
      c->code[start].offset = (uint16_t)c->code_len;  /* patch: skip to end */
    }
    else if (ch == '+') {
      /* e+ → body; SPLIT/SPLITNG(start)
         SPLIT: first=pc+1(end), second=offset(start) → non-greedy
         SPLITNG: first=offset(start), second=pc+1(end) → greedy */
      emit(c, nongreedy ? RE_SPLIT : RE_SPLITNG, 0, start);
    }
    else { /* ? */
      /* e? → SPLIT(body, end); body; end: */
      insert_inst(c, start, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
      c->code[start].offset = (uint16_t)c->code_len;  /* patch: skip to end */
    }
  }
  else if (ch == '{') {
    const char *save = c->p;
    next_char(c);
    int min, max;
    if (!parse_quantifier(c, &min, &max)) {
      c->p = save;
      return;  /* not a quantifier */
    }
    mrb_bool nongreedy = (peek(c) == '?');
    if (nongreedy) {
      next_char(c);
      c->needs_backtrack = TRUE;
    }

    /* For {n,m}: repeat atom min times, then optional (max-min) times */
    uint32_t atom_end = c->code_len;
    uint32_t atom_size = atom_end - start;

    if (min == 0 && max == 0) {
      /* {0}: the atom matches zero times, so drop the copy we emitted. */
      c->code_len = start;
    }
    else {
      /* {0,m} and {0,} compile as {1,m}/{1,} wrapped in an optional, so the
         single already-emitted copy is not forced to match. lo is the lower
         bound used while laying out copies (1 in the wrapped case). */
      mrb_bool wrap_optional = (min == 0);
      int lo = wrap_optional ? 1 : min;

      /* We have one copy already; emit lo-1 more mandatory copies. */
      for (int i = 1; i < lo; i++) {
        emit_atom_copy(c, start, atom_size);
      }
      /* Then optional copies */
      if (max < 0) {
        /* {n,} = lo copies + * */
        uint32_t loop_start = c->code_len;
        uint32_t split_pos = emit(c, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
        emit_atom_copy(c, start, atom_size);
        emit(c, RE_JMP, 0, loop_start);
        patch(c, split_pos, c->code_len);
      }
      else {
        for (int i = lo; i < max; i++) {
          uint32_t split_pos = emit(c, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
          emit_atom_copy(c, start, atom_size);
          patch(c, split_pos, c->code_len);
        }
      }
      if (wrap_optional) {
        /* Make the whole {1,m}/{1,} body skippable so it matches zero times. */
        insert_inst(c, start, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
        c->code[start].offset = (uint16_t)c->code_len;
      }
    }
  }
}

/* Compile a sequence of quantified atoms */
static void
compile_seq(re_compiler *c)
{
  while (peek(c) >= 0 && peek(c) != ')' && peek(c) != '|') {
    compile_quantified(c);
  }
}

/* Compile alternation: seq | seq | ... */
static void
compile_alt(re_compiler *c)
{
  uint32_t alt_start = c->code_len;
  compile_seq(c);

  if (peek(c) != '|') return;

  /* a|b → SPLIT L1 L2; L1: a; JMP END; L2: b; END:
     We need to insert SPLIT before already-emitted code for first alt.
     Strategy: emit JMP after first alt, then for each subsequent alt,
     insert a SPLIT before it by shifting code. */

  /* Collect all alternatives, then emit SPLIT chain at the end.
     This avoids insert_inst offset corruption for multi-way alternation. */
  uint32_t alt_starts[64];  /* start positions of each alternative */
  int num_alts = 0;
  alt_starts[num_alts++] = alt_start;

  while (peek(c) == '|') {
    next_char(c);
    emit(c, RE_JMP, 0, 0);  /* placeholder: jump to end */
    alt_starts[num_alts++] = c->code_len;
    if (num_alts >= 64) compile_error(c, "too many alternatives");
    compile_seq(c);
  }

  if (num_alts <= 1) return;  /* shouldn't happen, but safety */

  /* Now insert SPLIT chain before the alternatives.
     For n alternatives: n-1 SPLIT instructions, each pointing to
     their respective alternative. */
  uint32_t split_count = (uint32_t)(num_alts - 1);
  /* Insert split_count instructions at alt_starts[0] */
  for (uint32_t i = 0; i < split_count; i++) {
    insert_inst(c, alt_starts[0], RE_JMP, 0, 0);  /* placeholder */
    /* adjust all alt_starts by +1 due to insertion */
    for (int j = 0; j < num_alts; j++) {
      alt_starts[j]++;
    }
  }

  /* Now set up the SPLIT chain. Each SPLIT falls through to the next, and the
     chain's final fall-through reaches the first alternative, so the engines
     (which rank a SPLIT's fall-through above its jump) explore alternative 0
     first. The jump targets are then unwound in reverse, so SPLIT i must jump
     to alternative (split_count - i) to keep the remaining alternatives in
     source order -- i.e. leftmost-first across three or more branches. */
  for (uint32_t i = 0; i < split_count; i++) {
    uint32_t pos = alt_starts[0] - split_count + i;
    c->code[pos].op = RE_SPLIT;
    c->code[pos].a = 0;
    c->code[pos].offset = (uint16_t)alt_starts[split_count - i];
  }

  /* Patch JMPs (they are right before each alt_starts[1..n-1]) to point to end */
  uint32_t end = c->code_len;
  for (int i = 1; i < num_alts; i++) {
    uint32_t jmp_pos = alt_starts[i] - 1;
    c->code[jmp_pos].op = RE_JMP;
    c->code[jmp_pos].offset = (uint16_t)end;
  }
}

/*
 * Strip whitespace and #comments for extended mode (/x flag).
 * Whitespace inside [...] character classes is preserved.
 * Escaped characters (\ followed by anything) are preserved.
 */
static char*
strip_extended(mrb_state *mrb, const char *src, mrb_int len, mrb_int *out_len)
{
  char *buf = (char*)mrb_malloc(mrb, len);
  mrb_int o = 0;
  mrb_bool in_class = FALSE;
  const char *end = src + len;

  while (src < end) {
    char ch = *src;
    if (ch == '\\' && src + 1 < end) {
      buf[o++] = *src++;
      buf[o++] = *src++;
      continue;
    }
    if (in_class) {
      if (ch == ']') in_class = FALSE;
      buf[o++] = *src++;
      continue;
    }
    if (ch == '[') {
      in_class = TRUE;
      buf[o++] = *src++;
      continue;
    }
    if (ch == '#') {
      /* skip to end of line */
      while (src < end && *src != '\n') src++;
      continue;
    }
    if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f' || ch == '\v') {
      src++;
      continue;
    }
    buf[o++] = *src++;
  }
  *out_len = o;
  return buf;
}

/*
 * Compute the set of bytes that could be the first consumed byte of a match.
 * Walks bytecode from pc=0, following epsilon transitions (SAVE, JMP, SPLIT).
 * Returns TRUE if the set is narrower than "any byte" (i.e., useful for skip).
 */
static mrb_bool
first_set_walk(const re_inst *code, uint32_t code_len,
               const re_charclass *classes, uint32_t pc,
               uint8_t *bm, uint8_t *seen)
{
  while (pc < code_len) {
    if (seen[pc]) return TRUE;  /* already visited */
    seen[pc] = 1;
    switch (code[pc].op) {
    case RE_SAVE:
    case RE_BOL: case RE_EOL: case RE_BOT: case RE_EOT: case RE_EOTNL:
    case RE_WBOUND: case RE_NWBOUND:
      pc++;
      continue;  /* zero-width, keep walking */
    case RE_JMP:
      pc = code[pc].offset;
      continue;
    case RE_SPLIT:
      /* both branches: pc+1 and offset */
      if (!first_set_walk(code, code_len, classes, code[pc].offset, bm, seen))
        return FALSE;
      pc++;
      continue;
    case RE_SPLITNG:
      if (!first_set_walk(code, code_len, classes, pc + 1, bm, seen))
        return FALSE;
      pc = code[pc].offset;
      continue;
    case RE_CHAR:
      if (code[pc].a >= 128) return FALSE;  /* non-ASCII: bm covers ASCII only */
      bm[code[pc].a >> 3] |= (1 << (code[pc].a & 7));
      return TRUE;
    case RE_CLASS: {
      const re_charclass *cc = &classes[code[pc].a];
      for (int i = 0; i < 16; i++) bm[i] |= cc->bitmap[i];
      if (cc->utf8_any) return FALSE;  /* non-ASCII possible */
      if (cc->num_ranges > 0) return FALSE;  /* non-ASCII codepoints possible */
      return TRUE;
    }
    case RE_NCLASS: {
      /* negated class: complement of bitmap. Too many bits; not useful. */
      return FALSE;
    }
    case RE_ANY: case RE_ANY_NL:
      return FALSE;  /* any byte possible */
    case RE_MATCH:
      /* Reaching MATCH via epsilon transitions means the regex can match
         zero characters at any position. Skipping bytes that aren't in the
         first-byte set would skip past valid empty-match positions, so the
         optimization isn't safe -- bail out and accept any starting byte. */
      return FALSE;
    default:
      return FALSE;
    }
  }
  /* Walked off the end without hitting MATCH or a consuming op. Treat as
     empty-matchable, same as RE_MATCH. */
  return FALSE;
}

static mrb_bool
compute_first_set(const re_inst *code, uint32_t code_len,
                  const re_charclass *classes, uint8_t *bm)
{
  uint8_t seen[4096];
  if (code_len >= sizeof(seen)) return FALSE;  /* pattern too large */
  memset(seen, 0, code_len + 1);
  if (!first_set_walk(code, code_len, classes, 0, bm, seen))
    return FALSE;
  /* Check if bitmap is all-ones (no benefit to skip) */
  int set_bits = 0;
  for (int i = 0; i < 16; i++) {
    for (int b = 0; b < 8; b++) {
      if (bm[i] & (1 << b)) set_bits++;
    }
  }
  return set_bits < 96;  /* useful only if fewer than 75% of bytes match */
}

mrb_regexp_pattern*
mrb_re_compile(mrb_state *mrb, const char *pattern, mrb_int len, uint32_t flags)
{
  re_compiler c;
  memset(&c, 0, sizeof(c));

  if (flags & RE_FLAG_EXTENDED) {
    mrb_int slen;
    c.stripped = strip_extended(mrb, pattern, len, &slen);
    pattern = c.stripped;
    len = slen;
  }
  c.mrb = mrb;
  c.src = pattern;
  c.src_end = pattern + len;
  c.p = pattern;
  c.flags = flags;
  c.num_captures = 1;  /* group 0 = whole match */

  /* group 0 start */
  emit(&c, RE_SAVE, 0, 0);

  compile_alt(&c);

  if (c.p < c.src_end) {
    compile_error(&c, "unmatched ')'");
  }

  /* group 0 end */
  emit(&c, RE_SAVE, 0, 1);
  emit(&c, RE_MATCH, 0, 0);

  mrb_regexp_pattern *pat = (mrb_regexp_pattern*)mrb_malloc(mrb, sizeof(mrb_regexp_pattern));
  pat->code = c.code;
  pat->code_len = c.code_len;
  pat->classes = c.classes;
  pat->num_classes = c.num_classes;
  pat->num_captures = c.num_captures;
  pat->flags = flags;
  pat->named_captures = c.named_captures;
  pat->named_arena = NULL;
  pat->num_named = c.num_named;

  /* Copy capture names into an owned arena. Until this point the names
     point into the pattern source (or into c.stripped, which gets freed
     below in /x mode). After this loop the regexp owns its names. */
  if (c.num_named > 0) {
    size_t total = 0;
    for (uint16_t i = 0; i < c.num_named; i++) total += c.named_captures[i].name_len;
    if (total > 0) {
      pat->named_arena = (char*)mrb_malloc(mrb, total);
      size_t off = 0;
      for (uint16_t i = 0; i < c.num_named; i++) {
        uint16_t n = c.named_captures[i].name_len;
        memcpy(pat->named_arena + off, c.named_captures[i].name, n);
        pat->named_captures[i].name = pat->named_arena + off;
        off += n;
      }
    }
  }
  pat->has_backref = c.has_backref;
  pat->needs_backtrack = c.needs_backtrack;

  /* Extract literal prefix for fast search skip.
     Walk bytecode from the start, skipping SAVE, collecting RE_CHAR. */
  {
    uint8_t pbuf[256];
    int plen = 0;
    for (uint32_t i = 0; i < pat->code_len && plen < 255; i++) {
      if (pat->code[i].op == RE_SAVE) continue;
      if (pat->code[i].op == RE_CHAR) {
        pbuf[plen++] = pat->code[i].a;
      }
      else break;
    }
    if (plen > 0) {
      pat->prefix = (uint8_t*)mrb_malloc(mrb, plen);
      memcpy(pat->prefix, pbuf, plen);
      pat->prefix_len = (uint8_t)plen;
    }
    else {
      pat->prefix = NULL;
      pat->prefix_len = 0;
    }
  }

  /* Check if pattern is pure literal: SAVE CHAR* SAVE MATCH only.
     prefix_len already holds the literal char count if so. */
  pat->is_literal = FALSE;
  if (pat->prefix_len > 0 && pat->num_captures == 1 &&
      !pat->has_backref && !pat->needs_backtrack) {
    /* bytecode should be: SAVE(0), CHAR*N, SAVE(1), MATCH
       = 2 + prefix_len + 2 = prefix_len + 2 instructions
       (SAVE(0) at 0, CHARs at 1..N, SAVE(1) at N+1, MATCH at N+2) */
    if (pat->code_len == (uint32_t)(pat->prefix_len + 3) &&
        pat->code[0].op == RE_SAVE &&
        pat->code[pat->code_len - 2].op == RE_SAVE &&
        pat->code[pat->code_len - 1].op == RE_MATCH) {
      pat->is_literal = TRUE;
    }
  }

  /* Compute first-byte bitmap: set of bytes that could start a match.
     Used when prefix is empty (e.g. alternation, character class patterns). */
  {
    uint8_t bm[16];
    memset(bm, 0, sizeof(bm));
    pat->has_first_bytes = compute_first_set(pat->code, pat->code_len, pat->classes, bm);
    if (pat->has_first_bytes) {
      memcpy(pat->first_bytes, bm, 16);
    }
  }

  /* Pre-allocate VM state cache for pike_vm */
  {
    int list_capa = (int)pat->code_len * 2 + 16;
    pat->cached_visited = (uint32_t*)mrb_calloc(mrb, pat->code_len + 1, sizeof(uint32_t));
    pat->cached_threads[0] = mrb_malloc(mrb, sizeof(re_thread_cache) * list_capa);
    pat->cached_threads[1] = mrb_malloc(mrb, sizeof(re_thread_cache) * list_capa);
    pat->cached_list_capa = list_capa;
    pat->cache_in_use = FALSE;
  }

  if (c.stripped) mrb_free(mrb, c.stripped);
  return pat;
}

void
mrb_re_free(mrb_state *mrb, mrb_regexp_pattern *pat)
{
  if (pat) {
    mrb_free(mrb, pat->code);
    if (pat->classes) {
      for (uint16_t i = 0; i < pat->num_classes; i++) {
        mrb_free(mrb, pat->classes[i].ranges);
      }
      mrb_free(mrb, pat->classes);
    }
    mrb_free(mrb, pat->named_captures);
    mrb_free(mrb, pat->named_arena);
    mrb_free(mrb, pat->prefix);
    mrb_free(mrb, pat->cached_visited);
    mrb_free(mrb, pat->cached_threads[0]);
    mrb_free(mrb, pat->cached_threads[1]);
    mrb_free(mrb, pat);
  }
}
