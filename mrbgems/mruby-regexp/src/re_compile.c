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
#include <mruby/internal.h>
#include <string.h>

/* Class IDs are stored in re_inst.a (uint8_t), so at most 256 distinct
   character classes can be encoded.  Without this cap, class_capa
   (uint16_t) overflows on doubling past 32768 (8 -> 16 -> ... -> 32768
   -> 0), mrb_realloc with size 0 returns NULL, and the next memset
   crashes; even before that, the (uint8_t)id cast at emit sites would
   silently alias different classes. */
#define RE_MAX_CLASSES 256

/* Compiler state.

   Everything the compile allocates and the finished pattern goes on owning
   hangs off `pat`, which mrb_re_compile()'s caller has already handed to a
   Regexp. That is what makes the buffers survivable: raising is how this
   compiler reports a bad pattern and how mrb_realloc() reports a refused
   allocation, and either longjmps past this frame, which nothing then
   unwinds. Held here instead, the buffers would go with the frame.

   What is left here is the parse's own state: the cursor, the capacities the
   pattern does not record, and the counts that reach `pat` at the end. */
typedef struct {
  mrb_state *mrb;
  mrb_regexp_pattern *pat;  /* the pattern being built; owns code/classes/names */
  const char *src;     /* pattern source, preprocessed (see preprocess_pattern) */
  const char *src_end;
  const char *orig;    /* pattern as written, for error messages */
  const char *orig_end;
  const char *p;       /* current position */
  uint32_t code_len;
  uint32_t code_capa;
  uint16_t class_capa;
  uint16_t num_captures;
  uint32_t flags;
  uint16_t num_named;
  mrb_bool has_backref;
  mrb_bool needs_backtrack;
  mrb_bool dont_capture;    /* pattern declares a named group: plain (...) does not capture */
  uint16_t max_backref;     /* the largest `\NN` the pattern refers back to,
                               checked against the group count once the whole
                               pattern is read: CRuby takes a reference to a
                               group written later, `\1(a)`, so the count it
                               compares with is the pattern's, not the one
                               standing where the reference is */
  uint16_t num_groups;      /* groups opened so far, counting the plain ones a
                               named pattern demotes: what decides whether
                               `\NN` is a backreference or an octal escape */
  uint32_t num_cuts;        /* atomic groups and lookarounds numbered so far,
                               the possessive repeats among them: each takes
                               the next number, so no two that nest share one;
                               see RE_ATOMIC and RE_LOOK_END */
  uint32_t atom_start;      /* where the atom a quantifier binds to begins;
                               compile_quantified sets it to the position
                               before the atom, and a `\u{...}` list moves it
                               forward so the quantifier repeats the last
                               codepoint alone */
  uint32_t literal_cp[RE_MAX_CLASSES];  /* by class id: the codepoint whose
                               /i literal the class stands for, 0 for a class
                               made by anything else; see literal_class() */
} re_compiler;

static void compile_alt(re_compiler *c);  /* forward */

/* Take the message as a String rather than as a C string, for the messages
   that quote a group name: the name is a length-counted slice of the pattern
   and may hold a NUL, which a C string would cut short. Callers with a fixed
   message use compile_error() below. */
static void
compile_error_str(re_compiler *c, mrb_value msg)
{
  /* Quote c->orig, the pattern as written: when the pattern is preprocessed
     c->src points at the buffer preprocess_pattern() returned, so quoting it
     would drop the comments and the (?#...) groups from the message. c->orig
     is the caller's buffer, which outlives the compile. It is not
     NUL-terminated, so use %l with the explicit length from c->orig_end. */
  mrb_value emsg = mrb_format(c->mrb, "%v: /%l/",
                              msg, c->orig, (size_t)(c->orig_end - c->orig));

  mrb_exc_raise(c->mrb,
    mrb_exc_new_str(c->mrb, mrb_exc_get_id(c->mrb, MRB_SYM(RegexpError)), emsg));
}

static void
compile_error(re_compiler *c, const char *msg)
{
  compile_error_str(c, mrb_str_new_cstr(c->mrb, msg));
}

/* Maximum number of instructions in a compiled pattern. Every jump target
   lives in re_inst.offset (uint16_t) and a target may be one past the last
   instruction, so the whole program has to be addressable by that field.
   Without the cap the targets wrap on the way in and the engine jumps to an
   unrelated instruction: no exception, no memory error, just a pattern that
   stops matching text it describes. The check sits in emit(), the one place
   code_len grows, so it covers every producer including insert_inst(). */
#define RE_MAX_CODE_LEN 0xffff

static uint32_t
emit(re_compiler *c, uint8_t op, uint8_t a, uint16_t offset)
{
  if (c->code_len >= RE_MAX_CODE_LEN) compile_error(c, "regexp too large");
  if (c->code_len >= c->code_capa) {
    c->code_capa = c->code_capa ? c->code_capa * 2 : 64;
    c->pat->code = (re_inst*)mrb_realloc(c->mrb, c->pat->code, sizeof(re_inst) * c->code_capa);
  }
  uint32_t pos = c->code_len++;
  c->pat->code[pos].op = op;
  c->pat->code[pos].a = a;
  c->pat->code[pos].offset = offset;
  return pos;
}

static void
patch(re_compiler *c, uint32_t pos, uint16_t offset)
{
  c->pat->code[pos].offset = offset;
}

/* True for the opcodes whose `offset` field holds an absolute code index, so
   that relocating code has to carry it along. The jumps are the obvious ones;
   the four lookarounds hold the end of their sub-pattern there
   (re_internal.h), which is a code index just as much as a jump target and is
   just as wrong when the code it names moves. Every relocator asks this one
   question, because the two that used to carry their own list disagreed with
   each other and with the opcode set: both relocated the jumps alone, and a
   lookaround inside a quantified, repeated or alternated group kept an offset
   pointing at whatever the shift left in its place.
   RE_LB_WIDTH is not here: it carries a character count in `a`, and RE_SAVE
   and RE_BACKREF put a slot number and a case-fold flag in `offset` rather
   than an index. */
static mrb_bool
op_holds_code_index(uint8_t op)
{
  switch (op) {
  case RE_JMP: case RE_SPLIT: case RE_SPLITNG:
  case RE_LOOKAHEAD: case RE_NEG_LOOKAHEAD:
  case RE_LOOKBEHIND: case RE_NEG_LOOKBEHIND:
    return TRUE;
  default:
    return FALSE;
  }
}

/* Insert an instruction at position `pos` by shifting code.
   Adjusts code indices so they still point at the same instructions. */
static void
insert_inst(re_compiler *c, uint32_t pos, uint8_t op, uint8_t a, uint16_t offset)
{
  emit(c, RE_JMP, 0, 0);  /* grow array */
  uint32_t len = c->code_len - 1 - pos;
  memmove(&c->pat->code[pos + 1], &c->pat->code[pos], sizeof(re_inst) * len);
  c->pat->code[pos].op = op;
  c->pat->code[pos].a = a;
  c->pat->code[pos].offset = offset;

  /* Fix code indices across the insertion. An index past `pos` shifts down by
     one. An index equal to `pos` is ambiguous:
     - code that moved (i > pos) is a backward jump -- e.g. the SPLIT that
       loops `\d+` back to its class -- and meant the instruction now at
       pos+1, so it must follow.
     - code before the insertion (i < pos) is a forward "skip to here"
       reference that should stay on the newly inserted instruction. A
       lookaround is always that second case, since the end of its sub-pattern
       lies ahead of it: leaving the end at `pos` puts the inserted
       instruction after the sub-pattern rather than inside it, which is what
       the quantifier wrapping the whole group wants. */
  for (uint32_t i = 0; i < c->code_len; i++) {
    if (i == pos) continue;
    if (op_holds_code_index(c->pat->code[i].op) &&
        (c->pat->code[i].offset > pos || (c->pat->code[i].offset == pos && i > pos))) {
      c->pat->code[i].offset++;
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

/* Under /x, step over the whitespace between two tokens. Called where one
   token ends and the next may begin, in compile_seq() and before the
   quantifier in compile_quantified(), and nowhere else: whitespace inside a
   token is the token's own, so `a{1, 2}` is not an interval and `(?<a b>x)`
   names the group "a b", as in CRuby, whose tokenizer skips whitespace only
   where it fetches a token. The bytes are the five Onigmo skips; a vertical
   tab is a literal under /x there, and here.

   `#` comments are not skipped here: preprocess_pattern() removes them, as
   CRuby's own pre-pass does before it tokenizes, which is what lets a removed
   comment join `\1` to the `0` after it. */
static mrb_bool
extended_space(int ch)
{
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f';
}

static void
skip_extended_space(re_compiler *c)
{
  if (!(c->flags & RE_FLAG_EXTENDED)) return;
  while (extended_space(peek(c))) next_char(c);
}

static uint16_t
add_class(re_compiler *c)
{
  if (c->pat->num_classes >= RE_MAX_CLASSES) {
    compile_error(c, "too many character classes");
  }
  if (c->pat->num_classes >= c->class_capa) {
    c->class_capa = c->class_capa ? c->class_capa * 2 : 8;
    c->pat->classes = (re_charclass*)mrb_realloc(c->mrb, c->pat->classes, sizeof(re_charclass) * c->class_capa);
  }
  /* num_classes counts the entries mrb_re_free() reads a `ranges` pointer
     out of, so the slot is cleared before it is counted rather than after. */
  uint16_t id = c->pat->num_classes;
  memset(&c->pat->classes[id], 0, sizeof(re_charclass));
  c->pat->num_classes = id + 1;
  return id;
}

/* The class a /i literal for `cp` compiles to, whether it exists yet or not.

   The class holds `cp` and its case counterparts, and nothing else reaches it:
   not the flags in force, not the pattern around it, and no writer once the
   emitter that made it has returned. What it holds is a function of `cp`, so
   the second occurrence of a codepoint can name the class the first one made
   rather than make another. Each occurrence used to make its own, and a class
   id is a uint8_t, so a phrase of a few hundred letters under /i ran out of
   ids and was refused as having too many character classes, where the
   classes it needed were as many as its distinct letters.

   Every class is recorded by id, which keeps the record the size of the id
   space and lets it be searched to `num_classes` alone. It sits in the
   compiler's own frame rather than behind `pat`, since nothing outlives the
   compile that would want it. Zero marks a class that stands for no literal:
   it cannot be mistaken for one, since U+0000 has no case and neither caller
   folds it. Only the id is handed back, and it is for the caller to fill a
   class that is new, so that a class this function made is never taken for
   one it found. */
static uint16_t
literal_class(re_compiler *c, uint32_t cp, mrb_bool *found)
{
  for (uint16_t id = 0; id < c->pat->num_classes; id++) {
    if (c->literal_cp[id] == cp) {
      *found = TRUE;
      return id;
    }
  }
  uint16_t id = add_class(c);
  c->literal_cp[id] = cp;
  *found = FALSE;
  return id;
}

static void
class_set_bit(re_charclass *cc, uint8_t ch)
{
  if (ch < 128) {
    cc->bitmap[ch >> 3] |= (1 << (ch & 7));
  }
}

static mrb_bool
class_get_bit(const re_charclass *cc, uint8_t ch)
{
  if (ch >= 128) return FALSE;
  return (cc->bitmap[ch >> 3] >> (ch & 7)) & 1;
}

/* Add a non-ASCII codepoint range [lo, hi]. Both bounds must be >= 128.

   The list is held sorted by `lo`, with no two entries that overlap or touch.
   A range the class already covers then costs nothing to add again: the
   search below finds the entry holding it and widens that one where it has
   to, rather than appending a second entry naming what the first already
   accepts. Merging with the entry appended last left that to scan order, and
   closing a class under folding has none: the folds of [U+0080-U+2FFF] land
   inside the range that was written, and each of them appended an entry that
   range already covered, 527 of them for that class alone.

   Sorting them is the same answer at match time, where class_match() reads
   the list through, and at emit time, where every entry is bytecode.

   A byte range carries RE_CLASS_BYTE in both bounds, which is the top bit, so
   the tagged entries sort above every codepoint and no gap of one can close
   between the two kinds. */
static void
class_add_range(re_compiler *c, re_charclass *cc, uint32_t lo, uint32_t hi)
{
  uint32_t n = cc->num_ranges;

  /* Within one walk the ranges arrive ascending, so the entry standing
     highest is usually the one the next range joins. Nothing stands above it
     to close a gap to, which is what lets it be widened where it is. */
  if (n > 0) {
    uint32_t *last = &cc->ranges[2 * (n - 1)];
    if (lo >= last[0] && lo <= last[1] + 1) {
      if (hi > last[1]) last[1] = hi;
      return;
    }
  }

  /* The first entry that reaches [lo, hi] or begins after it: everything
     before it ends more than one below `lo` and cannot join. */
  uint32_t i = 0, j = n;
  while (i < j) {
    uint32_t mid = i + (j - i) / 2;
    if (cc->ranges[2 * mid + 1] + 1 < lo) i = mid + 1;
    else j = mid;
  }

  if (i < n && cc->ranges[2 * i] <= hi + 1) {
    /* It joins entry i. Widening it can close the gap to the entries above,
       so they are swallowed and what is left of the list is closed up. */
    uint32_t *r = &cc->ranges[2 * i];
    if (lo < r[0]) r[0] = lo;
    if (hi > r[1]) r[1] = hi;
    uint32_t k = i + 1;
    while (k < n && cc->ranges[2 * k] <= r[1] + 1) {
      if (cc->ranges[2 * k + 1] > r[1]) r[1] = cc->ranges[2 * k + 1];
      k++;
    }
    if (k > i + 1) {
      memmove(&cc->ranges[2 * (i + 1)], &cc->ranges[2 * k],
              sizeof(uint32_t) * 2 * (n - k));
      cc->num_ranges = n - (k - i - 1);
    }
    return;
  }

  if (n >= cc->range_capa) {
    /* range_capa/num_ranges are uint32_t: doubling from 32768 no longer
       wraps to 0 (which fed a size-0 realloc and a write through NULL). */
    uint32_t new_capa = cc->range_capa ? cc->range_capa * 2 : 4;
    cc->ranges = (uint32_t*)mrb_realloc(c->mrb, cc->ranges, sizeof(uint32_t) * 2 * new_capa);
    cc->range_capa = new_capa;
  }
  memmove(&cc->ranges[2 * (i + 1)], &cc->ranges[2 * i],
          sizeof(uint32_t) * 2 * (n - i));
  cc->ranges[2 * i] = lo;
  cc->ranges[2 * i + 1] = hi;
  cc->num_ranges = n + 1;
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

/* Add the case counterparts of an ASCII letter to a class that already holds
   both of its ASCII cases. Used for the single-literal paths, where the class
   exists only to express the choice between one character and its other
   cases. Only 'k' and 's' have a counterpart outside ASCII, which is why a
   build without the Unicode table still has something to do here: folding
   half of their equivalence classes is what makes [^k] under /i accept
   U+212A. */
static void
class_add_fold_counterparts(re_compiler *c, uint16_t id, uint32_t cp)
{
#ifdef RE_UNICODE_CASE
  uint32_t alt[MRB_UNI_MAX_UNFOLD];
  int n = mrb_uni_case_unfold(cp, alt, MRB_UNI_MAX_UNFOLD);
  for (int i = 0; i < n; i++) {
    if (alt[i] < 128) class_set_bit(&c->pat->classes[id], (uint8_t)alt[i]);
    else class_add_codepoint(c, &c->pat->classes[id], alt[i]);
  }
#else
  if (cp == 'k' || cp == 'K') class_add_codepoint(c, &c->pat->classes[id], RE_FOLD_KELVIN);
  else if (cp == 's' || cp == 'S') class_add_codepoint(c, &c->pat->classes[id], RE_FOLD_LONG_S);
#endif
}

#ifdef RE_UNICODE_CASE
/* The part of the class at or above `cp`, as the first range holding a member
   that high. FALSE where the class holds none. The list is sorted, so this is
   a search rather than a scan, and it answers against the list as it stands
   rather than against a position taken before it moved. */
static mrb_bool
class_next_range(const re_charclass *cc, uint32_t cp, uint32_t *lo, uint32_t *hi)
{
  uint32_t n = cc->num_ranges, i = 0, j = n;
  while (i < j) {
    uint32_t mid = i + (j - i) / 2;
    if (cc->ranges[2 * mid + 1] < cp) i = mid + 1;
    else j = mid;
  }
  if (i >= n) return FALSE;
  *lo = cc->ranges[2 * i] > cp ? cc->ranges[2 * i] : cp;
  *hi = cc->ranges[2 * i + 1];
  return TRUE;
}

/* Closure for the range walks, which report counterpart spans one at a time.
   A span can straddle 128 (U+017F folds to 's'), so it is split the same way
   a written range is. */
typedef struct {
  re_compiler *c;
  re_charclass *cc;
} class_fold_sink;

static void
class_fold_add(void *user, uint32_t lo, uint32_t hi)
{
  class_fold_sink *s = (class_fold_sink*)user;
  if (lo < 128) {
    class_set_range(s->cc, (uint8_t)lo, (uint8_t)(hi < 128 ? hi : 127));
  }
  if (hi >= 128) {
    class_add_range(s->c, s->cc, lo < 128 ? 128 : lo, hi);
  }
}
#endif

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

/* TRUE when every character the class can match is ASCII, so it always
   consumes exactly one byte. Non-ASCII codepoint ranges, a type read off the
   table and the utf8_any catch-all (set by \D, \W, \S, \H and [[:^ascii:]])
   all admit multibyte characters, whose width is not known until match
   time. */
static mrb_bool
class_is_ascii_only(const re_charclass *cc)
{
#ifdef RE_UNICODE_CTYPE
  if (cc->ctype_yes || cc->ctype_no) return FALSE;
#endif
  return cc->num_ranges == 0 && !cc->utf8_any;
}

/* Set ASCII bits for a POSIX class name (e.g. "alpha") into a 128-bit map.
   Returns FALSE for an unknown name. What the name holds above ASCII is
   answered separately, by the re_ctype bit written to *ctype: a build with
   the table reads the rest of the type off it, and one without has ASCII and
   nothing more, like this gem's \w/\d shorthands, so it writes nothing there
   and the caller does not read. [:xdigit:] and [:ascii:] are sets ASCII
   defines, so they hold nothing above it on any build and have no bit.

   *ascii_set is TRUE for a name whose ASCII set is one ASCII defines, [:word:]
   and [:ascii:], as opposed to one ASCII merely bounds here; the distinction
   is what compile_charclass() folds by. */
static mrb_bool
posix_class_bits(uint8_t *bits, const char *name, size_t len, mrb_bool *ascii_set,
                 uint16_t *ctype)
{
#define NAME_IS(s) (len == sizeof(s) - 1 && memcmp(name, s, len) == 0)
#define BSET(ch)   (bits[(ch) >> 3] |= (uint8_t)(1u << ((ch) & 7)))
#define BRANGE(lo, hi) do { for (int i = (lo); i <= (hi); i++) BSET(i); } while (0)
#ifdef RE_UNICODE_CTYPE
#define TYPE(t)    (*ctype = (t))
#else
#define TYPE(t)    ((void)0)
#endif
  *ascii_set = NAME_IS("word") || NAME_IS("ascii");
  TYPE(0);
  if (NAME_IS("alpha")) { BRANGE('a','z'); BRANGE('A','Z'); TYPE(RE_CTYPE_ALPHA); }
  else if (NAME_IS("digit")) { BRANGE('0','9'); TYPE(RE_CTYPE_DIGIT); }
  else if (NAME_IS("alnum")) { BRANGE('a','z'); BRANGE('A','Z'); BRANGE('0','9'); TYPE(RE_CTYPE_ALNUM); }
  else if (NAME_IS("upper")) { BRANGE('A','Z'); TYPE(RE_CTYPE_UPPER); }
  else if (NAME_IS("lower")) { BRANGE('a','z'); TYPE(RE_CTYPE_LOWER); }
  else if (NAME_IS("space")) { BSET(' '); BRANGE('\t','\r'); TYPE(RE_CTYPE_SPACE); }
  else if (NAME_IS("blank")) { BSET(' '); BSET('\t'); TYPE(RE_CTYPE_BLANK); }
  else if (NAME_IS("xdigit")) { BRANGE('0','9'); BRANGE('a','f'); BRANGE('A','F'); }
  else if (NAME_IS("word")) { BRANGE('a','z'); BRANGE('A','Z'); BRANGE('0','9'); BSET('_'); TYPE(RE_CTYPE_WORD); }
  else if (NAME_IS("cntrl")) { BRANGE(0, 0x1f); BSET(0x7f); TYPE(RE_CTYPE_CNTRL); }
  else if (NAME_IS("print")) { BRANGE(0x20, 0x7e); TYPE(RE_CTYPE_PRINT); }
  else if (NAME_IS("graph")) { BRANGE(0x21, 0x7e); TYPE(RE_CTYPE_GRAPH); }
  else if (NAME_IS("ascii")) { BRANGE(0, 0x7f); }
  else if (NAME_IS("punct")) {
    for (int i = 0x21; i <= 0x7e; i++) {
      mrb_bool alnum = (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z') ||
                       (i >= '0' && i <= '9');
      if (!alnum) BSET(i);
    }
    TYPE(RE_CTYPE_PUNCT);
  }
  else return FALSE;
  return TRUE;
#undef NAME_IS
#undef BSET
#undef BRANGE
#undef TYPE
}

/* Value of one hex digit, or -1 for anything else (including the -1 that
   peek() returns at the end of the pattern). */
static int
hex_value(int ch)
{
  if (ch >= '0' && ch <= '9') return ch - '0';
  if (ch >= 'a' && ch <= 'f') return ch - 'a' + 10;
  if (ch >= 'A' && ch <= 'F') return ch - 'A' + 10;
  return -1;
}

static int parse_escape(re_compiler *c);

/* Read the character a control escape names and return the control character
   it stands for. `\cX` and `\C-X` name the same one, and a `\` in the X
   position opens an escape of its own, so `\c\n` is a control newline.

   The mask is the one this build's own lexer uses for a string, which is what
   a regexp literal is read by: /\cA/ reaches the engine as the byte already.
   Only Regexp.new() with a written-out backslash arrives here, and the two
   spellings have to name the same character. That settles `\c?`, where CRuby
   disagrees with itself: its lexer answers DEL and Onig answers 0x1f, so
   /\c?/ and Regexp.new("\\c?") are two different patterns there. Following
   the lexer keeps the pair together here. */
static int
parse_control_escape(re_compiler *c)
{
  int ch = next_char(c);

  if (ch < 0) compile_error(c, "too short control escape");
  if (ch == '\\') ch = parse_escape(c);
  if (ch == '?') return 0x7f;
  return ch & 0x1f;
}

static int
parse_escape(re_compiler *c)
{
  int ch = next_char(c);
  if (ch < 0) compile_error(c, "trailing backslash");
  switch (ch) {
  case 'c':
    return parse_control_escape(c);
  case 'C':
    /* Only the `\C-X` spelling names a control character; a `\C` with
       anything else after it is the escape ending early, as it is to CRuby. */
    if (peek(c) != '-') compile_error(c, "too short control escape");
    next_char(c);
    return parse_control_escape(c);
  case 'M':
    /* `\M-X` sets the high bit, making a byte that starts no character. This
       engine has no encoding to read one against (see README.md), and CRuby
       refuses the escape in a pattern that is not binary, so it is refused
       rather than answered with a byte nothing matches. */
    compile_error(c, "meta escape is not supported");
  case 'n': return '\n';
  case 't': return '\t';
  case 'r': return '\r';
  case 'f': return '\f';
  case 'v': return '\v';
  case 'a': return '\a';
  case 'e': return 0x1b;
  case 'b': return '\b';  /* backspace; only reachable inside [...] since the
                             top-level dispatcher emits RE_WBOUND for `\b` */
  /* Octal escape `\NNN` (1-3 digits, value 0-255). From the top level a
     digit other than `0` reaches here only once the dispatcher has ruled out
     a backreference; inside `[...]` read_class_atom sends every digit here,
     since a class has no backreferences. Three digits can spell up to
     0777, and CRuby refuses what is past a byte rather than fold it. */
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
    if (val > 0xff) compile_error(c, "invalid escape code");
    return val;
  }
  /* Hex escape `\xHH` (1-2 hex digits, value 0-255). The `\x{HHHH}` form
     for codepoints above 0xff is not implemented, and it is not read as
     `\x` either: a `\x` that no hex digit follows used to come out as
     `\x00`, so `\x{41}` compiled to a NUL and a quantifier. CRuby rejects
     it, as it rejects a bare `\x` and `\xZ`. */
  case 'x': {
    int val = 0;
    int n = 0;
    while (n < 2) {
      int v = hex_value(peek(c));
      if (v < 0) break;
      val = val * 16 + v;
      next_char(c);
      n++;
    }
    if (n == 0) compile_error(c, "invalid hex escape");
    return val & 0xff;
  }
  default: return ch;  /* literal: \., \\, \/, \(, etc. */
  }
}

/* Reject what a pattern may not name. CRuby reports both a surrogate and a
   value past the last plane as "invalid Unicode range", and reports it where
   the pattern is read rather than where it is emitted, so the check stays
   here: mrb_utf8_to_buf() refuses the second on its own but spells the first,
   and neither reaches it anyway. */
static void
check_unicode_cp(re_compiler *c, uint32_t cp)
{
  if (cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff)) {
    compile_error(c, "invalid Unicode range");
  }
}

/* Separator between the codepoints of a `\u{...}` list. */
static mrb_bool
unicode_list_space(int ch)
{
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\v' || ch == '\f' || ch == '\r';
}

/* Read the next codepoint of an open `\u{...}` list, or close it. Returns
   FALSE once the `}` is consumed, leaving *more FALSE so the caller's loop
   ends. */
static mrb_bool
unicode_escape_next(re_compiler *c, mrb_bool *more, uint32_t *out)
{
  if (!*more) return FALSE;
  while (unicode_list_space(peek(c))) next_char(c);
  if (peek(c) == '}') {
    next_char(c);
    *more = FALSE;
    return FALSE;
  }

  uint32_t cp = 0;
  int n = 0;
  for (;;) {
    int v = hex_value(peek(c));
    if (v < 0) break;
    next_char(c);
    cp = cp * 16 + (uint32_t)v;
    /* Six digits reach U+FFFFFF, past the last plane, so a seventh can only
       be an overlong spelling. CRuby rejects `\u{0000061}` rather than
       reading it as U+0061. */
    if (++n > 6) compile_error(c, "invalid Unicode range");
  }
  /* Anything that is neither a hex digit nor the closing brace ends the
     list: a separator CRuby does not take (`\u{61,62}`), or the end of the
     pattern (`\u{61`). */
  if (n == 0) compile_error(c, "invalid Unicode list");
  check_unicode_cp(c, cp);
  *out = cp;
  return TRUE;
}

/* Read a `\u` escape and return its first codepoint; the backslash and the
   `u` are already consumed. `\uXXXX` is exactly four hex digits and yields
   one codepoint. `\u{...}` holds one or more, so *more is set and the rest
   come from unicode_escape_next(). */
static uint32_t
unicode_escape_first(re_compiler *c, mrb_bool *more)
{
  *more = FALSE;
  if (peek(c) == '{') {
    next_char(c);
    *more = TRUE;
    uint32_t cp;
    /* The list has to hold something: `\u{}` and `\u{ }` are errors, not an
       escape that contributes nothing. */
    if (!unicode_escape_next(c, more, &cp)) compile_error(c, "invalid Unicode list");
    return cp;
  }

  /* Nothing at all after `\u` is reported apart from a bad digit, as CRuby
     does: /\u/ is "too short escape sequence" while /\u6/ is not. */
  if (peek(c) < 0) compile_error(c, "too short escape sequence");
  uint32_t cp = 0;
  for (int i = 0; i < 4; i++) {
    int v = hex_value(peek(c));
    if (v < 0) compile_error(c, "invalid Unicode escape");
    next_char(c);
    cp = cp * 16 + (uint32_t)v;
  }
  check_unicode_cp(c, cp);
  return cp;
}

/* Add one member to the class: the ASCII bitmap and the range list each hold
   one side of 128, and class_match() picks the side to read from the value
   alone. Above 128 the value is a codepoint or a byte, which the tag records
   because the number cannot: see RE_CLASS_BYTE. */
static void
class_add_member(re_compiler *c, re_charclass *cc, uint32_t cp, mrb_bool is_byte)
{
  if (cp < 128) class_set_bit(cc, (uint8_t)cp);
  else class_add_codepoint(c, cc, (is_byte ? RE_CLASS_BYTE : 0) | cp);
}

/* What a `\u` escape names, in the members a class can hold. On a build whose
   characters are single bytes, a codepoint above ASCII is the bytes that spell
   it, which is already what a character written out in the class comes to
   there: read_class_atom() decodes one byte at a time, so `[Ā]` holds `\xC4`
   and `\x80`. Naming the same character rather than spelling it out cannot
   mean something else, so the escape contributes those bytes too. All but the
   last join the class here, and the last is returned, so it can open a range
   as any other atom would.

   A range so opened is a range of bytes, since that is what both ends are.
   The written out spelling reaches byte ends by its own route and comes to a
   different span, which is what a range between two characters neither
   spelling can express comes to on a build like this. */
static uint32_t
class_named_cp(re_compiler *c, re_charclass *cc, uint32_t cp, mrb_bool *is_byte)
{
  if (MRB_ENC_MULTIBYTE_P || cp < 0x80) return cp;

  char buf[4];
  int len = (int)mrb_utf8_to_buf(buf, (mrb_int)cp);
  for (int i = 0; i < len - 1; i++) {
    class_add_member(c, cc, (uint8_t)buf[i], TRUE);
  }
  *is_byte = TRUE;
  return (uint8_t)buf[len - 1];
}

/* Read one character class atom: either an ASCII byte (0-127), a
   `\escape`, or a full multi-byte UTF-8 codepoint. Returns the value and
   advances c->p. *is_byte says which of the two the value is: TRUE for a
   byte at or above 0x80 that starts no whole character, FALSE for ASCII, for
   a decoded codepoint and for `\u`, which names a codepoint outright.
   closes_range says the atom follows a `-`, which matters to a `\u{...}`
   list alone: the codepoint next to the `-` is the range end, and the rest
   of the list are members.

   The question is the one the literal path already answers: emit_char_folded()
   decodes and stands aside when the decode consumed one byte, so `\xB5` and a
   raw 0xB5 both compile to the byte outside [...]. Reading the same byte as
   U+00B5 inside [...] made the two halves of one pattern disagree about what
   the pattern holds. A byte and a codepoint of the same number are different
   members, which is what the tag on the stored value records. */
static uint32_t
read_class_atom(re_compiler *c, re_charclass *cc, mrb_bool *is_byte, mrb_bool closes_range)
{
  *is_byte = FALSE;
  if (peek(c) == '\\') {
    next_char(c);
    if (peek(c) == 'u') {
      next_char(c);
      mrb_bool more;
      uint32_t cp = unicode_escape_first(c, &more);
      uint32_t nx;
      /* Every codepoint of a `\u{...}` list is a member of its own, apart
         from the one next to a `-`, which is a range end as any other atom
         would be. That is the last of the list before the `-` and the first
         after it: `[\u{61 62}-z]` is `a` plus `b-z`, and `[a-\u{63 7a}]` is
         `a-c` plus `z`. The rest join the class here. */
      if (closes_range) {
        uint32_t end = class_named_cp(c, cc, cp, is_byte);
        while (unicode_escape_next(c, &more, &nx)) {
          mrb_bool member_byte = FALSE;
          uint32_t member = class_named_cp(c, cc, nx, &member_byte);
          class_add_member(c, cc, member, member_byte);
        }
        return end;
      }
      while (unicode_escape_next(c, &more, &nx)) {
        mrb_bool member_byte = FALSE;
        uint32_t member = class_named_cp(c, cc, cp, &member_byte);
        class_add_member(c, cc, member, member_byte);
        cp = nx;
      }
      return class_named_cp(c, cc, cp, is_byte);
    }
    /* A backslash before a multibyte character has no escape meaning, so let
       the decode below read the whole codepoint: [\Ā] is [Ā]. parse_escape()
       returns one byte, which left the continuation byte as a class atom of
       its own. A trailing backslash (peek < 0) still reaches parse_escape(),
       which reports it. */
    if (peek(c) < 0xC0) {
      uint32_t esc = (uint32_t)parse_escape(c);
      /* \xNN and octal \NNN name a byte, and the literal path emits one. */
      if (esc >= 0x80) *is_byte = TRUE;
      return esc;
    }
  }
  uint8_t b = (uint8_t)*c->p;
  if (b < 0xC0) {
    /* ASCII, or a continuation byte that starts nothing. */
    if (b >= 0x80) *is_byte = TRUE;
    return (uint32_t)next_char(c);
  }
  /* Multi-byte UTF-8 leader: decode the full codepoint. An invalid leader
     decodes as itself over one byte, so it is a byte like the rest. */
  int len = 0;
  uint32_t cp = mrb_re_decode_char(c->p, c->src_end, &len, FALSE);
  c->p += len;
  if (len == 1) *is_byte = TRUE;
  return cp;
}

/* Parse [...] character class */
static void
compile_charclass(re_compiler *c)
{
  uint16_t id = add_class(c);
  re_charclass *cc = &c->pat->classes[id];
  mrb_bool negated = FALSE;

  if (peek(c) == '^') {
    next_char(c);
    negated = TRUE;
  }

  /* What \w, \W, [:word:] and [:ascii:] add is held apart until the class
     has been closed under folding, and joins the bitmap after; see the fold
     below for why. Only the bitmap and utf8_any are ever written here. */
  re_charclass ascii_set;
  memset(&ascii_set, 0, sizeof(ascii_set));

  mrb_bool first = TRUE;
  while (peek(c) != ']' || first) {
    if (peek(c) < 0) compile_error(c, "unterminated character class");
    first = FALSE;

    /* `&&` takes the intersection of what is written either side of it, which
       this engine does not do. Read as members it is the opposite of what was
       asked: [a&&b] would hold a, & and b where it names nothing at all, so a
       class written to narrow one would widen it instead. A lone `&` is a
       member here as it is in CRuby, and an escaped one (`\&&`) is that
       member followed by whatever comes next. */
    if (peek(c) == '&' && c->p + 1 < c->src_end && c->p[1] == '&') {
      compile_error(c, "character class intersection is not supported");
    }

    /* A '[' inside a class opens something in CRuby rather than standing for
       itself: a POSIX bracket, a collating element, an equivalence class, or
       a class nested in this one. Only the bracket is read here and the rest
       are refused, since taken as members they compile to a different pattern
       than the one written: [[a][b]] is the union of two classes there and
       was `[` or `a`, then b, then `]` here. `[\[]` holds the bracket itself,
       in CRuby as well. A '[' with nothing after it leaves the class
       unterminated, which the loop reports on its own. */
    if (peek(c) == '[' && c->p + 1 < c->src_end) {
      if (c->p[1] == '.') compile_error(c, "POSIX collating element is not supported");
      if (c->p[1] == '=') compile_error(c, "POSIX equivalence class is not supported");
      if (c->p[1] != ':') compile_error(c, "nested character class is not supported");
    }

    /* POSIX bracket class: [:name:] or negated [:^name:] inside [...]. */
    if (peek(c) == '[' && c->p + 1 < c->src_end && c->p[1] == ':') {
      const char *save = c->p;
      next_char(c);  /* '[' */
      next_char(c);  /* ':' */
      mrb_bool neg = FALSE;
      if (peek(c) == '^') { neg = TRUE; next_char(c); }
      const char *name = c->p;
      while (peek(c) >= 0 && peek(c) != ':' && peek(c) != ']') next_char(c);
      mrb_bool stopped_at_bracket_end = (peek(c) == ']');
      if (peek(c) == ':' && c->p + 1 < c->src_end && c->p[1] == ']') {
        uint8_t bits[16] = {0};
        mrb_bool by_ascii;
        uint16_t ctype;
        if (!posix_class_bits(bits, name, (size_t)(c->p - name), &by_ascii, &ctype)) {
          compile_error(c, "invalid POSIX bracket class");
        }
        next_char(c);  /* ':' */
        next_char(c);  /* ']' */
        re_charclass *dst = by_ascii ? &ascii_set : cc;
        for (int i = 0; i < 128; i++) {
          mrb_bool in = (bits[i >> 3] >> (i & 7)) & 1;
          if (in != neg) class_set_bit(dst, (uint8_t)i);
        }
        /* Above ASCII a type is read off the table at match time, in either
           polarity. A set ASCII defines holds nothing there, so its negation
           holds everything there: [:^ascii:] is every character above ASCII
           and every byte that is no character. Without the table, every
           bracket is such a set. */
#ifdef RE_UNICODE_CTYPE
        if (ctype) {
          if (neg) cc->ctype_no |= ctype;
          else cc->ctype_yes |= ctype;
        }
        else if (neg) dst->utf8_any = TRUE;
#else
        if (neg) dst->utf8_any = TRUE;
#endif
        continue;
      }
      /* The '[' opened a bracket, so a name that does not close is the
         bracket ending early rather than a literal '[', as it is to CRuby.
         Which of the two things went wrong depends on where the scan
         stopped: at a ']' the class does close and only the bracket ended
         early, and anywhere else (a ':' with nothing after it, or the end of
         the pattern) the class never closes either, which is the older and
         more particular complaint of the two. */
      c->p = save;
      compile_error(c, stopped_at_bracket_end
                    ? "premature end of char-class"
                    : "unterminated character class");
    }

    /* Shorthand classes (\d, \D, \w, \W, \s, \S, \h, \H) are handled
       before the codepoint-aware path so the single-byte semantics
       stay intact. */
    if (peek(c) == '\\') {
      int esc = (c->p + 1 < c->src_end) ? (uint8_t)c->p[1] : -1;
      /* The engine reads no character property, and the members below would
         make one of every letter of the name: [\p{Han}] would hold H, a and
         n. Refused rather than answered with the text of the request. */
      if ((esc == 'p' || esc == 'P') && c->p + 2 < c->src_end && c->p[2] == '{') {
        compile_error(c, "character property is not supported");
      }
      if (esc == 'd' || esc == 'D' || esc == 'w' || esc == 'W' ||
          esc == 's' || esc == 'S' || esc == 'h' || esc == 'H') {
        next_char(c);  /* '\\' */
        next_char(c);  /* spec  */
        class_add_shorthand((esc == 'w' || esc == 'W') ? &ascii_set : cc, esc);
        continue;
      }
    }

    mrb_bool cp_byte;
    uint32_t cp = read_class_atom(c, cc, &cp_byte, FALSE);

    /* check for range a-z (or U+xxxx-U+yyyy) */
    if (peek(c) == '-' && c->p + 1 < c->src_end && c->p[1] != ']') {
      next_char(c);  /* skip '-' */
      mrb_bool hi_byte;
      uint32_t hi = read_class_atom(c, cc, &hi_byte, TRUE);
      /* An endpoint at or above 128 is a byte or a character, and a span from
         one to the other names neither: [\x80-µ] would run from a byte to a
         codepoint. ASCII belongs to both, so it pairs with either. */
      if (cp >= 128 && hi >= 128 && cp_byte != hi_byte) {
        compile_error(c, "character class range mixes a byte and a character");
      }
      /* A range written backwards holds nothing, and CRuby reports it rather
         than compiling a class that silently lacks the span, or in the
         negated form admits everything: [b-a] and [^b-a] both raise. The
         numbers compare, since the check above leaves no byte paired with a
         character and ASCII sits below either. */
      if (cp > hi) compile_error(c, "empty range in char class");
      /* A range that straddles the ASCII boundary is split in two: the
         bitmap takes the half below 128 and the codepoint list the rest.
         Neither half can hold the other, and class_match() picks the side
         to read from the codepoint alone, so a span left whole in the
         codepoint list is unreachable below 128. */
      if (cp < 128) class_set_range(cc, (uint8_t)cp, (uint8_t)(hi < 128 ? hi : 127));
      if (hi >= 128) {
        uint32_t tag = hi_byte ? RE_CLASS_BYTE : 0;
        class_add_range(c, cc, tag | (cp < 128 ? 128 : cp), tag | hi);
      }
    }
    else {
      class_add_member(c, cc, cp, cp_byte);
    }
  }
  next_char(c);  /* skip ']' */

  /* Close the class under case folding for /i. This runs once the class is
     complete, so it covers every form the loop above merges in: POSIX
     brackets, ranges and single literals. Negation is applied at match time
     against the same class (RE_NCLASS), so closing the positive set is also
     what keeps [^a-c] and [^Ā] from accepting what they were written to
     reject.

     Closing means: x belongs to the class whenever some written member folds
     the same way x does. A byte member has no case: it stands for no character,
     so nothing folds to it and it folds to nothing. Every walk below steps over
     the tagged ranges, which is also what keeps /i from refusing a class of
     continuation bytes on a build without the folding tables.

     The word class and [:ascii:] are still held apart here, so the closure
     never sees them. Each is a set ASCII defines: \w is [a-zA-Z0-9_] and no
     more, so a fold that leaves ASCII leaves the set, and [\w] under /i is
     the ASCII word characters where [k] under /i reaches U+212A. CRuby reads
     them the same way, keeping the two out of the class it folds across the
     boundary from, and it is what makes [^\w] under /i accept U+017F. Both
     hold both cases of every letter they hold, so the ASCII part of the
     closure has nothing to add to them, and joining them after it costs
     nothing. The other POSIX brackets are ASCII here only for want of a
     table, and stay in: CRuby folds them too, and there their members above
     ASCII hold what the fold adds anyway. */
  if (c->flags & RE_FLAG_IGNORECASE) {
#ifdef RE_UNICODE_CASE
    /* That takes two rounds rather than one walk in each direction, because a
       fold can have more than one source (U+03A3 and U+03C2 both fold to
       U+03C3), and a class written with one of them reaches the others only
       through the fold they share. The first round puts that shared fold in
       the class; the second pulls in everything that folds to it. A third
       round would find nothing: whatever the second adds folds to something
       the first already added. */
    class_fold_sink sink = { c, cc };

    /* Round one: the fold of every member joins the class. Both rounds walk
       the list by codepoint rather than by index, since class_add_range()
       inserts where the order puts an entry and an index would name a
       different entry after one did. A walk that starts each step above the
       range it just handed over reaches every entry the round began with, and
       terminates whatever the additions do.

       What it may skip is an entry inserted behind the cursor, and neither
       round has anything to say about one. Folding is idempotent, so the fold
       of a fold this round added is the fold itself; and nothing folds to a
       character that folds elsewhere, so a source the next round adds has no
       source of its own. */
    for (uint32_t cp = 0;;) {
      uint32_t lo, hi;
      /* The tagged byte ranges sort above every codepoint, so the first one
         reached ends the walk rather than being stepped over. */
      if (!class_next_range(cc, cp, &lo, &hi) || (lo & RE_CLASS_BYTE)) break;
      mrb_uni_case_fold_range(lo, hi, class_fold_add, &sink);
      cp = hi + 1;
    }
    for (int ch = 'A'; ch <= 'Z'; ch++) {
      if (class_get_bit(cc, (uint8_t)ch)) class_set_bit(cc, (uint8_t)(ch + 32));
    }

    /* Round two: every source of a member joins it too. */
    for (uint32_t cp = 0;;) {
      uint32_t lo, hi;
      if (!class_next_range(cc, cp, &lo, &hi) || (lo & RE_CLASS_BYTE)) break;
      mrb_uni_case_unfold_range(lo, hi, class_fold_add, &sink);
      cp = hi + 1;
    }
    /* An ASCII member can have a non-ASCII source (U+212A folds to 'k'),
       which the range walk cannot reach: the bitmap holds no ranges. A run of
       set bits is asked about in one question, since a question costs a walk
       of the tables whatever it spans. The tables hold no ASCII source, ASCII
       being what they are the rest of, so nothing this walk finds lands in
       the bitmap and no run of it grows while it is being read. */
    for (int ch = 0; ch < 128; ch++) {
      if (!class_get_bit(cc, (uint8_t)ch)) continue;
      int end = ch;
      while (end + 1 < 128 && class_get_bit(cc, (uint8_t)(end + 1))) end++;
      mrb_uni_case_unfold_range((uint32_t)ch, (uint32_t)end, class_fold_add, &sink);
      ch = end;
    }
    /* The upper case letter of a lower case member is the source the tables
       do not hold, so it is set here. Nothing folds to an upper case letter,
       which is what lets this come after the walk above rather than adding to
       what that walk is asked about. */
    for (int ch = 'a'; ch <= 'z'; ch++) {
      if (class_get_bit(cc, (uint8_t)ch)) class_set_bit(cc, (uint8_t)(ch - 32));
    }
#else
    /* The same closure, restricted to the foldings this build has. Refusing
       first is what makes the restriction sound: whatever is left in the
       codepoint list after this loop either folds to an ASCII letter or folds
       to nothing, so the rounds collapse into the ASCII pass with one exchange
       across the boundary in each direction. */
    for (uint32_t i = 0; i < cc->num_ranges; i++) {
      uint32_t lo = cc->ranges[2 * i], hi = cc->ranges[2 * i + 1];
      if (lo & RE_CLASS_BYTE) continue;
      if (mrb_re_needs_case_data(lo, hi)) {
        compile_error(c, "/i needs Unicode case folding for this character class");
      }
      if (lo <= RE_FOLD_LONG_S && RE_FOLD_LONG_S <= hi) class_set_bit(cc, 's');
      if (lo <= RE_FOLD_KELVIN && RE_FOLD_KELVIN <= hi) class_set_bit(cc, 'k');
    }
    for (int ch = 'a'; ch <= 'z'; ch++) {
      if (class_get_bit(cc, (uint8_t)ch)) class_set_bit(cc, (uint8_t)(ch - 32));
      else if (class_get_bit(cc, (uint8_t)(ch - 32))) class_set_bit(cc, (uint8_t)ch);
    }
    if (class_get_bit(cc, 'k')) class_add_codepoint(c, cc, RE_FOLD_KELVIN);
    if (class_get_bit(cc, 's')) class_add_codepoint(c, cc, RE_FOLD_LONG_S);
#endif
  }

  for (int i = 0; i < RE_CLASS_BITMAP_SIZE; i++) cc->bitmap[i] |= ascii_set.bitmap[i];
  if (ascii_set.utf8_any) cc->utf8_any = TRUE;

#ifdef RE_UNICODE_CTYPE
  /* A type is not spelled out as members, so the closure above never saw it;
     it is closed at match time instead, by reading the type of every
     character sharing the folding of the one in hand. The bitmap holds the
     ASCII of every bracket, and the closure has already reached across the
     boundary from there: U+017F is in the class once 's' is, so a type
     found only through an ASCII counterpart is found through the ranges. */
  cc->ctype_fold = (c->flags & RE_FLAG_IGNORECASE) && (cc->ctype_yes || cc->ctype_no);
#endif

  cc->negated = negated;
  emit(c, negated ? RE_NCLASS : RE_CLASS, (uint8_t)id, 0);
}

/* Maximum value for {n}/{n,m} quantifiers. Each unit becomes (min-1) +
   (max-min) emitted copies of the inner atom; the cap keeps both the
   parse free of integer overflow and the bytecode size sane. */
#define RE_MAX_REPEAT 32768

/* Parse {n}, {n,}, {n,m} quantifier. Returns min,max via pointers. */
static mrb_bool
parse_quantifier(re_compiler *c, int *min_out, int *max_out, mrb_bool *ranged)
{
  const char *save = c->p;
  int min = 0, max = -1;
  mrb_bool has_digit = FALSE;

  *ranged = FALSE;

  while (peek(c) >= '0' && peek(c) <= '9') {
    min = min * 10 + (next_char(c) - '0');
    has_digit = TRUE;
    if (min > RE_MAX_REPEAT) compile_error(c, "quantifier too large");
  }
  if (peek(c) == ',') {
    next_char(c);
    *ranged = TRUE;
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
  /* A range written backwards names no repeat count, and CRuby reports it
     rather than compiling a repeat that silently drops the upper bound:
     compile_quantified() would lay out `min` mandatory copies and then a
     `max - min` loop that runs zero times, which is `{min}`. The check sits
     here, once the `}` is consumed, so that a `{...}` which is no quantifier
     at all is still restored as a literal brace above, and it lets the
     unlimited upper bound through, which is spelled -1. */
  if (max >= 0 && max < min) {
    compile_error(c, "upper is smaller than lower in repeat range");
  }
  *min_out = min;
  *max_out = max;
  return TRUE;
}

/*
 * Measure the bytecode in range [start, end) for a lookbehind, which must
 * know exactly how far back to rewind. Returns the byte count a binary
 * subject needs, one per consuming instruction since such a subject
 * advances one byte whatever the instruction is, and stores in *chars_out
 * the character count a UTF-8 subject needs. Returns -1 if the sub-pattern
 * has no fixed width (quantifiers, alternation, etc.).
 */
static int
compute_fixed_len(re_compiler *c, uint32_t start, uint32_t end, int *chars_out)
{
  int len = 0;
  int chars = 0;
  uint32_t pc = start;

  while (pc < end) {
    re_inst inst = c->pat->code[pc];
    switch (inst.op) {
    case RE_CHAR: {
      /* A multibyte literal is a run of one-byte RE_CHAR instructions, and
         what a byte spells depends on the bytes after it, so hand the run to
         mrb_re_charlen() rather than read the lead bit alone: a continuation
         byte no lead reaches is a character of its own, which is the rule the
         executor rewinds by. Four bytes is the longest character there is,
         and a run never splits one. */
      char buf[4];
      int n = 0;
      while (n < 4 && pc + (uint32_t)n < end && c->pat->code[pc + n].op == RE_CHAR) {
        buf[n] = (char)c->pat->code[pc + n].a;
        n++;
      }
      int clen = mrb_re_charlen(buf, buf + n, FALSE);
      len += clen;
      chars += 1;
      pc += (uint32_t)clen;
      break;
    }
    case RE_CLASS:
    case RE_NCLASS:
    case RE_ANY:
    case RE_ANY_NL:
      /* one character whatever its members can be, since the executor
         hands a class one decoded character at a time */
      len += 1;
      chars += 1;
      pc++;
      break;
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
    case RE_LOOK_END:
      *chars_out = chars;
      return len;
    default:
      return -1;  /* unknown/variable-length instruction */
    }
  }
  *chars_out = chars;
  return len;
}

/* Parse the option letters of an inline (?...) group. The parser is
   positioned just past the '?'; it reads i, m, x and '-' in any order until
   the terminator (':' or ')'), a '-' switching the letters after it off.
   `base` is the option set in effect on entry; the resulting set is
   returned. Ruby's inline letters are i (IGNORECASE), m (DOTALL),
   x (EXTENDED). All three are scoped alike by the group they are read in:
   skip_extended_space() reads x from c->flags, which compile_atom() saves
   at every '(' and restores at its ')'. preprocess_pattern() tracks the
   same scopes over the pattern as written for the one thing it still does
   under /x, removing `#` comments.

   No letter has to come at all, and a second '-' is read like the first:
   Onigmo's loop asks only that what it reads is one of these bytes and that
   what stops it is the terminator, so `(?-)` is a toggle that changes
   nothing, `(?-:a)` is `(?:a)` and `(?--i)` is `(?-i)`. A generator that
   writes `(?#{on}-#{off}:...)` reaches the first two with both lists empty.
   The caller reports what stops the loop when it is neither ':' nor ')',
   which is where a letter that is not an option is refused, and `(?)` never
   gets here at all: compile_atom() takes this arm only on i, m, x or '-'. */
static uint32_t
parse_inline_flags(re_compiler *c, uint32_t base)
{
  uint32_t on = 0, off = 0;
  mrb_bool negate = FALSE;
  for (;;) {
    int oc = peek(c);
    uint32_t bit;
    if (oc == 'i') bit = RE_FLAG_IGNORECASE;
    else if (oc == 'm') bit = RE_FLAG_DOTALL;
    else if (oc == 'x') bit = RE_FLAG_EXTENDED;
    else if (oc == '-') { negate = TRUE; next_char(c); continue; }
    else break;
    if (negate) off |= bit;
    else on |= bit;
    next_char(c);
  }
  return (base | on) & ~off;
}

/* Emit one byte as an atom, for a character that is a single byte. Under /i an
   ASCII letter becomes a class of its case counterparts instead, so any of them
   matches. The multibyte spelling of the same job is emit_char_bytes() below. */
static void
emit_char(re_compiler *c, uint8_t ch)
{
  if ((c->flags & RE_FLAG_IGNORECASE) &&
      ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))) {
    mrb_bool found;
    uint16_t id = literal_class(c, ch, &found);
    if (!found) {
      class_set_bit(&c->pat->classes[id], ch);
      class_set_bit(&c->pat->classes[id], (uint8_t)(ch ^ 0x20));  /* the other case */
      class_add_fold_counterparts(c, id, ch);
    }
    emit(c, RE_CLASS, (uint8_t)id, 0);
    return;
  }
  emit(c, RE_CHAR, ch, 0);
}

/* Emit every byte of the character whose lead byte `ch` was just consumed, so
   the whole character is one atom. Leaving the continuation bytes to the parse
   loop made each of them an atom of its own, and a quantifier binds to the last
   atom emitted: /Ā+/ compiled as \xC4(\x80)+ and matched one Ā in "ĀĀ". An
   invalid lead byte has a charlen of 1 and still emits alone. Every atom that
   consumes a character has to emit all of its bytes before returning, since
   what compile_quantified() repeats is the bytes that atom emitted. */
static void
emit_char_bytes(re_compiler *c, int ch)
{
  int len = mrb_re_charlen(c->p - 1, c->src_end, FALSE);
  emit(c, RE_CHAR, (uint8_t)ch, 0);
  for (int i = 1; i < len; i++) {
    int b = next_char(c);
    if (b < 0) break;
    emit(c, RE_CHAR, (uint8_t)b, 0);
  }
}

/* Emit a non-ASCII codepoint under /i as a class rather than a run of bytes,
   and report whether it did. A counterpart need not have the same byte length
   (U+212A folds to 'k'), which a byte-wise RE_CHAR run cannot express, while
   RE_CLASS decodes one codepoint and compares that whatever its width. A
   character with no counterpart, which is most of the non-ASCII range and
   every script without case in it, falls back to the bytes and costs /i
   nothing. A character this build cannot fold is refused rather than answered
   without the folding it needs. The caller has already established /i. */
static mrb_bool
emit_cp_folded(re_compiler *c, uint32_t cp)
{
  if (mrb_re_needs_case_data(cp, cp)) {
    compile_error(c, "/i needs Unicode case folding for this character");
  }
#ifdef RE_UNICODE_CASE
  uint32_t alt[MRB_UNI_MAX_UNFOLD];
  int n = mrb_uni_case_unfold(cp, alt, MRB_UNI_MAX_UNFOLD);
#else
  /* Whatever survived the refusal folds to an ASCII letter or to nothing at
     all, so the counterparts are that letter and its upper case: the first two
     steps of the general walk, and the only two this build can take. */
  uint32_t alt[2];
  int n = 0;
  uint32_t f = mrb_re_case_fold(cp);
  if (f != cp) { alt[n++] = f; alt[n++] = f - 32; }
#endif
  if (n == 0) return FALSE;
  mrb_bool found;
  uint16_t id = literal_class(c, cp, &found);
  if (!found) {
    class_add_codepoint(c, &c->pat->classes[id], cp);
    for (int i = 0; i < n; i++) {
      class_add_member(c, &c->pat->classes[id], alt[i], FALSE);
    }
  }
  emit(c, RE_CLASS, (uint8_t)id, 0);
  return TRUE;
}

/* The same for a character the pattern spells out, whose bytes the caller is
   partway through: the codepoint comes from the pattern, and the bytes it took
   are consumed only once the class is emitted.

   A byte that starts no whole character is not a character to fold. It decodes
   as one byte and hands back its own value, which would read a lone 0xB5 as
   U+00B5 and answer /i for a character the pattern does not hold, so it falls
   back to the bytes like every other invalid sequence in the literal path. */
static mrb_bool
emit_char_folded(re_compiler *c, int ch)
{
  if (ch < 128 || !(c->flags & RE_FLAG_IGNORECASE)) return FALSE;
  int len = 0;
  uint32_t cp = mrb_re_decode_char(c->p - 1, c->src_end, &len, FALSE);
  if (len == 1) return FALSE;
  if (!emit_cp_folded(c, cp)) return FALSE;
  c->p += len - 1;
  return TRUE;
}

/* Emit one codepoint as an atom: a run of RE_CHAR, one per UTF-8 byte. This is
   emit_char_bytes() for a codepoint the pattern names rather than spells, so
   the bytes come from the encoder instead of from the pattern. The run has to
   be a single atom just the same, or a following quantifier binds to the last
   byte alone. Naming a character does not change what /i does with it, so the
   folded spelling is tried first, as it is for a character the pattern
   spells. */
static void
emit_codepoint(re_compiler *c, uint32_t cp)
{
  if (cp < 128) {
    emit_char(c, (uint8_t)cp);
    return;
  }
  char buf[4];
  int len = (int)mrb_utf8_to_buf(buf, (mrb_int)cp);
  /* Fold only a spelling the engine reads back as the one character it spells.
     What the fold emits is a class, and a class compares one decoded
     character; where the build decodes bytes it never sees this one, so the
     class would answer for a lone byte of the same number rather than for the
     character the pattern names. The bytes below name it on either build,
     which is the fallback a character the pattern spells already takes there,
     through the length emit_char_folded() reads. */
  if ((c->flags & RE_FLAG_IGNORECASE) &&
      mrb_re_charlen(buf, buf + len, FALSE) == len &&
      emit_cp_folded(c, cp)) {
    return;
  }
  for (int i = 0; i < len; i++) {
    emit(c, RE_CHAR, (uint8_t)buf[i], 0);
  }
}

/* Largest number a \k<n> / \k<-n> backreference may spell. The bound is far
   above RE_MAX_CAPTURES because it is not a capacity: it separates two of
   CRuby's messages, `too big number` for a number past it and `invalid backref
   number/name` for one within it that names no group. */
#define RE_MAX_BACKREF_NUM 2147483647

/* Compile the sub-pattern of a lookaround, whose opener has just been
   emitted, up to and including the RE_LOOK_END that closes it. The end
   carries the lookaround's number, from the count the atomic groups use, so
   that a cut aimed at this lookaround is told from one aimed at a group
   around or inside it; see bt_match(). */
static void
compile_look_body(re_compiler *c, mrb_bool negative)
{
  uint16_t cut = (uint16_t)++c->num_cuts;
  compile_alt(c);
  emit(c, RE_LOOK_END, negative, cut);
}

/* Compile a single atom (character, class, group, etc.). Returns whether one
   was read: FALSE when what stands at the parse point is not an atom, either a
   quantifier metacharacter or the end of the sequence, neither of them
   consumed, or an option toggle `(?i)`, consumed but nothing a quantifier can
   repeat. An empty group `(?:)` emits nothing and is still an atom. */
static mrb_bool
compile_atom(re_compiler *c)
{
  int ch = peek(c);

  switch (ch) {
  case '(':
    {
      next_char(c);
      mrb_bool capturing = TRUE;

      /* Options in effect on entry. A group restores them on exit so an
         inline toggle like (?i) inside it (which sets c->flags for the rest
         of the group) does not leak past the closing ')'. */
      uint32_t saved_flags = c->flags;

      const char *cap_name = NULL;
      uint32_t cap_name_len = 0;

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
          compile_look_body(c, negative);
          c->pat->code[la_pos].offset = (uint16_t)c->code_len;  /* patch: skip past sub-pattern */
          if (peek(c) != ')') compile_error(c, "unmatched '('");
          next_char(c);
          c->needs_backtrack = TRUE;  /* needs backtracking engine */
          c->flags = saved_flags;
          break;  /* done with this atom */
        }
        else if (c->p[1] == '<' && c->p + 2 < c->src_end && (c->p[2] == '=' || c->p[2] == '!')) {
          /* lookbehind (?<=...) or (?<!...) */
          mrb_bool negative = (c->p[2] == '!');
          next_char(c); next_char(c); next_char(c);  /* skip ?<= or ?<! */
          uint32_t lb_pos = emit(c, negative ? RE_NEG_LOOKBEHIND : RE_LOOKBEHIND, 0, 0);
          emit(c, RE_LB_WIDTH, 0, 0);
          uint32_t sub_start = c->code_len;
          compile_look_body(c, negative);
          c->pat->code[lb_pos].offset = (uint16_t)c->code_len;

          /* measure the sub-pattern for both rewind units */
          int fixed_chars;
          int fixed_len = compute_fixed_len(c, sub_start, c->code_len, &fixed_chars);
          if (fixed_len < 0) {
            compile_error(c, "lookbehind must be fixed length");
          }
          if (fixed_len > 255) {
            compile_error(c, "lookbehind too long (max 255 bytes)");
          }
          c->pat->code[lb_pos].a = (uint8_t)fixed_len;
          /* the character count never exceeds the byte count, so it fits */
          c->pat->code[lb_pos + 1].a = (uint8_t)fixed_chars;

          if (peek(c) != ')') compile_error(c, "unmatched '('");
          next_char(c);
          c->needs_backtrack = TRUE;  /* needs backtracking engine */
          c->flags = saved_flags;
          break;
        }
        else if (c->p[1] == '>') {
          /* atomic group (?>...): the body is a non-capturing group whose
             first match is its only one. The two instructions bracketing it
             carry a number of the group's own, which is how the executor
             pairs the end of a body with the group it closes when a failure
             after the body has to fail the group; see bt_match(). The number
             is the count of groups numbered before it rather than the
             nesting depth: a possessive repeat wraps a group around code
             already emitted, and with depths that wrapper and a group inside
             it, compiled with the same groups open around them, shared a
             depth, so a cut of the wrapper was read as the inner group's.
             Every numbered group emits two instructions, so the number fits
             the field. */
          next_char(c); next_char(c);  /* skip ?> */
          uint16_t cut = (uint16_t)++c->num_cuts;
          emit(c, RE_ATOMIC, 0, cut);
          compile_alt(c);
          emit(c, RE_ATOMIC_END, 0, cut);
          if (peek(c) != ')') compile_error(c, "unmatched '('");
          next_char(c);
          c->needs_backtrack = TRUE;  /* the Pike VM cannot cut a thread */
          c->flags = saved_flags;
          break;
        }
        else if (c->p[1] == '\'' ||
                 (c->p[1] == '<' && c->p + 2 < c->src_end &&
                  c->p[2] != '=' && c->p[2] != '!')) {
          /* A named capture, in either spelling: (?<name>...) or (?'name'...).
             The quoted one needs none of the ruling out the angled one does
             above, since no lookbehind is spelled with a quote. The name runs
             to its own terminator, so a '>' inside quotes and a quote inside
             angles are both members of the name. */
          int close = (c->p[1] == '<') ? '>' : '\'';
          next_char(c); next_char(c);  /* skip ?< or ?' */
          cap_name = c->p;
          while (peek(c) != close && peek(c) >= 0) {
            /* CRuby's fetch_name() ends the scan at a ')' instead of taking it
               into the name, and a name no delimiter ended is `invalid group
               name`. The first byte is exempt, the test starting one byte
               in, so (?<)>x) names ")" in both engines. The message quotes
               to the end of the pattern, as CRuby does when the scan never
               reached a delimiter. */
            if (peek(c) == ')' && c->p > cap_name) {
              compile_error_str(c, mrb_format(c->mrb, "invalid group name <%l>",
                                              cap_name, (size_t)(c->src_end - cap_name)));
            }
            next_char(c);
          }
          if (peek(c) != close) compile_error(c, "unterminated named capture");
          if (c->p == cap_name) compile_error(c, "group name is empty");
          /* A definition names a group, it never numbers one: CRuby reads a
             leading digit or '-' as the number spelling, which only a
             reference may use, and refuses it here. Everything after the first
             byte is a name to both engines, `-` and spaces included, so this
             is not a restriction to word characters. Without it, (?<1>x)
             defined a group that no \k could reach, the reference side
             reading digits as a number. */
          if (*cap_name == '-' || (*cap_name >= '0' && *cap_name <= '9')) {
            compile_error_str(c, mrb_format(c->mrb, "invalid group name <%l>",
                                            cap_name, (size_t)(c->p - cap_name)));
          }
          if (!RE_NAME_LEN_FITS(c->p - cap_name)) compile_error(c, "group name too long");
          cap_name_len = (uint32_t)(c->p - cap_name);
          next_char(c);  /* skip the closing > or ' */
        }
        else if (c->p[1] == 'i' || c->p[1] == 'm' || c->p[1] == 'x' || c->p[1] == '-') {
          /* Inline options: the toggle form (?imx) / (?-imx) changes the
             options for the rest of the enclosing group, and the scoped
             form (?imx:...) is a non-capturing group whose options apply
             only to its body. */
          next_char(c);  /* skip '?' */
          uint32_t new_flags = parse_inline_flags(c, c->flags);
          if (peek(c) == ')') {
            next_char(c);
            c->flags = new_flags;  /* rest of the group; restored at its ')' */
            return FALSE;          /* consumed the token; no atom emitted */
          }
          else if (peek(c) == ':') {
            next_char(c);
            c->flags = new_flags;
            compile_alt(c);
            c->flags = saved_flags;
            if (peek(c) != ')') compile_error(c, "unmatched '('");
            next_char(c);
            return TRUE;
          }
          else {
            compile_error(c, "undefined (?...) sequence");
          }
        }
        else if (c->p[1] == '#') {
          /* preprocess_pattern() removes a terminated comment group before
             the parser runs, so one reaching here was never closed. */
          compile_error(c, "unterminated comment group");
        }
        else {
          /* (?X) with an unsupported X: not one of the recognized (?: (?= (?!
             (?<= (?<! (?<name> (?'name' (?> (?imx forms. Comment groups (?#...)
             never get here either, having been removed by
             preprocess_pattern(). The absent operator (?~...) and
             conditionals (?(...)) are not implemented. Raise here rather
             than falling through to the capturing-group path, which would
             leave the stray `?` for compile_seq to spin on forever (A1). */
          compile_error(c, "undefined (?...) sequence");
        }
      }

      /* Onigmo's ONIG_OPTION_DONT_CAPTURE_GROUP, which CRuby turns on for a
         pattern that declares a named group: a plain (...) then groups
         without capturing, so the numbered side counts only the named
         groups. The named group itself keeps its number. The count of groups
         opened is taken before that demotion: CRuby demotes plain groups
         only once the parse is done, so while it reads the pattern every one
         of them is still a group that a `\NN` may refer to. */
      if (capturing && c->num_groups < UINT16_MAX) c->num_groups++;
      if (c->dont_capture && cap_name == NULL) capturing = FALSE;

      uint16_t group = 0;
      if (capturing) {
        if (c->num_captures >= RE_MAX_CAPTURES) {
          compile_error(c, "too many capture groups");
        }
        group = c->num_captures++;
        emit(c, RE_SAVE, 0, group * 2);
        if (cap_name) {
          /* register named capture */
          c->pat->named_captures = (re_named_capture*)mrb_realloc(c->mrb, c->pat->named_captures,
            sizeof(re_named_capture) * (c->num_named + 1));
          c->pat->named_captures[c->num_named].name = cap_name;
          c->pat->named_captures[c->num_named].name_len = cap_name_len;
          c->pat->named_captures[c->num_named].group = group;
          c->num_named++;
        }
      }

      compile_alt(c);

      if (peek(c) != ')') compile_error(c, "unmatched '('");
      next_char(c);

      if (capturing) {
        emit(c, RE_SAVE, 0, group * 2 + 1);
      }
      c->flags = saved_flags;  /* inline toggles inside the group end here */
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
      /* A digit run after the backslash is read as one decimal number first,
         and is a backreference when that number is at most 9 or at most the
         number of groups opened so far, as in CRuby (Onigmo's fetch_token):
         `\1` and `\12` after twelve groups refer back, `\12` before them is
         octal 012, a newline. What is not a backreference is an octal escape
         of up to three digits (`\101` is `A`, `\1234` is `S4`), or the digit
         itself when it starts with 8 or 9 (`\81` is `81`). Only the
         comparison with the group count needs the number, so accumulation
         stops once it is past every count a pattern can reach. */
      uint32_t num = 0;
      const char *q = c->p;
      while (q < c->src_end && *q >= '0' && *q <= '9') {
        if (num <= UINT16_MAX) num = num * 10 + (uint32_t)(*q - '0');
        q++;
      }
      if (num <= 9 || num <= c->num_groups) {
        if (c->dont_capture) {
          compile_error(c, "numbered backref/call is not allowed. (use name)");
        }
        /* Not dont_capture, so every group counted captures and num is
           within RE_MAX_CAPTURES. */
        c->p = q;
        emit(c, RE_BACKREF, (uint8_t)num, (c->flags & RE_FLAG_IGNORECASE) ? 1 : 0);
        c->has_backref = TRUE;
        if (num > c->max_backref) c->max_backref = (uint16_t)num;
      }
      else {
        /* parse_escape() reads `\1`-`\7` as octal and `\8`, `\9` as the
           digit itself, its default for a byte with no escape meaning. */
        emit_char(c, (uint8_t)parse_escape(c));
      }
    }
    else if (ch == 'd' || ch == 'D' || ch == 'w' || ch == 'W' || ch == 's' || ch == 'S') {
      next_char(c);
      uint16_t id = add_class(c);
      class_add_shorthand(&c->pat->classes[id], ch);
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
      class_add_shorthand(&c->pat->classes[id], ch);
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
      while (peek(c) != close && peek(c) >= 0) {
        /* The reference reads a name the same way a definition does, and stops
           at a ')' the same way too, from the second byte on: \k<)> reaches the
           group (?<)>x) opened, while \k<a)b> is `invalid group name`. */
        if (peek(c) == ')' && c->p > name) {
          compile_error_str(c, mrb_format(c->mrb, "invalid group name <%l>",
                                          name, (size_t)(c->src_end - name)));
        }
        next_char(c);
      }
      if (peek(c) != close) compile_error(c, "unterminated backreference name");
      if (c->p == name) compile_error(c, "group name is empty");
      if (!RE_NAME_LEN_FITS(c->p - name)) compile_error(c, "group name too long");
      uint32_t name_len = (uint32_t)(c->p - name);
      next_char(c);  /* skip the closing > or ' */

      int group = -1;
      if (name_len > 0 && (name[0] == '-' || (name[0] >= '0' && name[0] <= '9'))) {
        mrb_bool relative = (name[0] == '-');
        uint32_t first = relative ? 1 : 0;

        /* CRuby reads the whole name before converting it, so a name that is
           not `-`? followed by digits is a malformed name whatever the digits
           it does hold would come to: \k<99999999999999999999x> is `invalid
           group name`, not `too big number`. A lone `-` is malformed too. */
        mrb_bool numeric = (first < name_len);
        for (uint32_t i = first; numeric && i < name_len; i++) {
          if (name[i] < '0' || name[i] > '9') numeric = FALSE;
        }

        int n = 0;
        for (uint32_t i = first; numeric && i < name_len; i++) {
          int digit = name[i] - '0';
          /* CRuby's scanner stops at RE_MAX_BACKREF_NUM, and a number past it
             is too big rather than a reference to a group that is missing. */
          if (n > (RE_MAX_BACKREF_NUM - digit) / 10) compile_error(c, "too big number");
          n = n * 10 + digit;
        }
        /* n == 0 names group 0, the whole match, which \k cannot reference */
        if (!numeric || n == 0) {
          compile_error_str(c, mrb_format(c->mrb, "invalid group name <%l>",
                                          name, (size_t)name_len));
        }

        /* CRuby rejects a numbered backreference in a named pattern whatever
           its spelling, and it has to be rejected here too: once plain groups
           stop consuming numbers, both the absolute bound and the relative
           form's `num_captures - n` below would silently resolve to a
           different group instead of erroring. */
        if (c->dont_capture) {
          compile_error(c, "numbered backref/call is not allowed. (use name)");
        }
        group = relative ? (int)c->num_captures - n : n;
        if (group < 1 || group >= (int)c->num_captures) {
          compile_error(c, "invalid backref number/name");
        }
      }
      else {
        for (uint16_t i = 0; i < c->num_named; i++) {
          if (c->pat->named_captures[i].name_len == name_len &&
              memcmp(c->pat->named_captures[i].name, name, name_len) == 0) {
            group = c->pat->named_captures[i].group;
            break;
          }
        }
        if (group < 1) {
          compile_error_str(c, mrb_format(c->mrb, "undefined name <%l> reference",
                                          name, (size_t)name_len));
        }
      }
      emit(c, RE_BACKREF, (uint8_t)group, (c->flags & RE_FLAG_IGNORECASE) ? 1 : 0);
      c->has_backref = TRUE;
    }
    else if (ch == 'g' && c->p + 1 < c->src_end &&
             (c->p[1] == '<' || c->p[1] == '\'')) {
      /* `\g<name>` calls a group's sub-pattern again, which this engine does
         not do. Left to the fall-through it was the letter `g` and the name
         was literal text, so /(a)\g<1>/ matched "ag<1>" rather than "aa".
         A bare `\g` is the letter in CRuby too, and stays one. */
      compile_error(c, "subexpression call is not supported");
    }
    else if (ch == 'G' || ch == 'K' || ch == 'R' || ch == 'X') {
      /* Each of these means something in CRuby that this engine does not do:
         `\G` anchors at where the search began, `\K` drops what was matched
         before it, `\R` is any linebreak and `\X` is a whole grapheme
         cluster. Left to the fall-through each was simply its own letter, so
         /\R/ matched an R rather than a newline. Inside a character class
         CRuby reads them as the letter too, which is what the class parser
         already does, so only the escape outside one is refused here. */
      compile_error_str(c, mrb_format(c->mrb, "\\\\%c is not supported", (char)ch));
    }
    else if ((ch == 'p' || ch == 'P') && c->p + 1 < c->src_end && c->p[1] == '{') {
      /* The engine reads no character property. Without this the escape is
         the letter it names and the braces are literal too, so /\p{Alpha}/
         would answer a pattern that asked for a letter with the text of the
         request. `[[:alpha:]]` is how to ask for one; see README.md.
         Only the braced spelling is a property: CRuby reads a bare `\p`, and
         `\pL` as well, as the letter, and so does the fall-through below. */
      compile_error(c, "character property is not supported");
    }
    else if (ch == 'u') {
      next_char(c);  /* skip u */
      mrb_bool more;
      uint32_t cp = unicode_escape_first(c, &more);
      uint32_t nx;
      /* A `\u{...}` list is a sequence of atoms rather than one, so a
         quantifier after it repeats the last codepoint only: /\u{61 62}+/
         is `a` followed by `b+`. Moving atom_start past the codepoints
         already emitted is what leaves the last one as the target. */
      while (unicode_escape_next(c, &more, &nx)) {
        emit_codepoint(c, cp);
        c->atom_start = c->code_len;
        cp = nx;
      }
      emit_codepoint(c, cp);
    }
    else if (ch >= 0xC0) {
      /* A backslash before a multibyte character has no escape meaning: \Ā is
         Ā. parse_escape() returns one byte, which left the continuation bytes
         to the parse loop as atoms of their own, so emit the whole character
         here as the unescaped spelling does. The dispatch has to happen before
         parse_escape() reads the letter, since \xNN and octal \NNN name a byte
         rather than a character. */
      next_char(c);
      if (!emit_char_folded(c, ch)) emit_char_bytes(c, ch);
    }
    else {
      emit_char(c, (uint8_t)parse_escape(c));
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
      mrb_bool qranged;
      if (parse_quantifier(c, &qmin, &qmax, &qranged)) {
        compile_error(c, "target of repeat operator is not specified");
      }
      emit(c, RE_CHAR, '{', 0);
    }
    break;

  default:
    if (ch < 0 || ch == ')' || ch == '|' || ch == '*' || ch == '+' || ch == '?') {
      return FALSE;  /* not an atom */
    }
    next_char(c);
    if (ch >= 128) {
      if (!emit_char_folded(c, ch)) emit_char_bytes(c, ch);
      break;
    }
    emit_char(c, (uint8_t)ch);
    break;
  }
  return TRUE;
}

/* Append a copy of the atom bytecode in [start, start+size) at the current
   position. Internal code indices are relocated to the copy, so a repeated
   group like (a{2,3}){2} keeps each iteration self-contained instead of
   jumping back into the first copy (which corrupted its captures). A
   lookaround's sub-pattern end is one of those indices: left pointing into
   the original, the copy runs its assertion against the first iteration's
   code and ends the outer match early. Capture slots (RE_SAVE) are shared
   across copies on purpose: a repeated group keeps only its last iteration,
   like CRuby. */
static void
emit_atom_copy(re_compiler *c, uint32_t start, uint32_t size)
{
  int32_t delta = (int32_t)c->code_len - (int32_t)start;
  uint32_t atom_end = start + size;
  for (uint32_t j = 0; j < size; j++) {
    re_inst in = c->pat->code[start + j];
    if (op_holds_code_index(in.op) && in.offset >= start && in.offset <= atom_end) {
      in.offset = (uint16_t)((int32_t)in.offset + delta);
    }
    emit(c, in.op, in.a, in.offset);
  }
}

/* Read the quantifiers that follow a body which matches empty and emit
   nothing for them: `a{0}` and `(?:)` leave no code, and repeating what
   matches empty any number of times still matches empty, so `a{0}*`,
   `a{0}{2}?` and `(?:)+` are the empty match CRuby gives. */
static void
skip_quantifiers(re_compiler *c)
{
  for (;;) {
    skip_extended_space(c);
    int ch = peek(c);
    if (ch == '*' || ch == '+' || ch == '?') {
      next_char(c);
      if (peek(c) == '?') next_char(c);
      continue;
    }
    if (ch == '{') {
      const char *save = c->p;
      int min, max;
      mrb_bool ranged;
      next_char(c);
      if (!parse_quantifier(c, &min, &max, &ranged)) { c->p = save; return; }
      if (peek(c) == '?') next_char(c);
      continue;
    }
    return;
  }
}

/* Compile atom with quantifiers (*, +, ?, {n,m}) */
static void
compile_quantified(re_compiler *c)
{
  uint32_t begin = c->code_len;
  /* atom_start normally stays at `begin`; compile_atom moves it only for a
     `\u{...}` list, whose leading codepoints are atoms of their own. Saving
     and restoring it keeps a nested compile_quantified (inside a group) from
     leaving its own atom behind for this one. */
  uint32_t saved_atom_start = c->atom_start;
  c->atom_start = begin;
  mrb_bool atom = compile_atom(c);
  uint32_t start = c->atom_start;
  c->atom_start = saved_atom_start;
  if (c->code_len == begin) {
    /* Nothing emitted. An empty group `(?:)` is an atom all the same, and
       one that matches empty matches empty however it is repeated, so its
       quantifiers are read and emit nothing, as those of `a{0}` are; CRuby
       compiles `(?:)*` the same way. What is not an atom, an option toggle
       `(?i)` or a stray metacharacter, leaves a quantifier after it for
       compile_seq() to refuse, as CRuby refuses `(?i)*`. */
    if (atom) skip_quantifiers(c);
    return;
  }

  /* Under /x the quantifier may stand apart from its atom, `a +` being `a+`.
     A `?` making it non-greedy is read right after it, though, as CRuby
     reads it.

     A quantifier that follows a quantifier binds everything emitted so far
     rather than the atom alone, so the loop below comes round with `start`
     back at `begin`: `a**` is `(?:a*)*` and `a{2}{3}` is `(?:a{2}){3}`. Two
     spellings are read before it comes round, as CRuby reads them:

     - `?` right after `*`, `+`, `?` or a `{n,m}` written with a comma is the
       non-greedy marker. `{n}` has no non-greedy form, so its `?` is a
       quantifier of its own: `a{3}?` is `(?:a{3})?` and matches empty, where
       the lazy `a{3,3}?` does not.
     - `+` right after `*`, `+` or `?` is possessive, `a*+` being `(?>a*)`.
       That is why `a?+` takes one `a` out of "aa" where `(?:a?)+` takes two.
       After a `{...}` it is a quantifier again, `a{2}+` matching "aaaa".

     `greedy_rep` says the last thing read was a greedy `*`, `+` or `?`, which
     is the only place a `+` is possessive: the `+` of `a+` follows the atom,
     the one of `a+?+` follows a lazy repeat, and the one of `a?++` follows a
     possessive, and CRuby reads all three of those as quantifiers. */
  mrb_bool greedy_rep = FALSE;

  for (;;) {
    skip_extended_space(c);
    int ch = peek(c);

    if (greedy_rep && ch == '+') {
      /* Possessive: what stands emitted becomes an atomic group of its own,
         numbered after every group inside it, since those are emitted. */
      next_char(c);
      uint16_t cut = (uint16_t)++c->num_cuts;
      insert_inst(c, begin, RE_ATOMIC, 0, cut);
      emit(c, RE_ATOMIC_END, 0, cut);
      c->needs_backtrack = TRUE;  /* the Pike VM cannot cut a thread */
      greedy_rep = FALSE;
      start = begin;
      continue;
    }

    if (ch == '*' || ch == '+' || ch == '?') {
      next_char(c);
      mrb_bool nongreedy = (peek(c) == '?');
      if (nongreedy) {
        next_char(c);
        c->needs_backtrack = TRUE;
      }

      if (ch == '*') {
        /* e* -> L: SPLIT(body, end); body; JMP L; end:
           SPLIT offset = end (after JMP), patched after JMP is emitted */
        insert_inst(c, start, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
        emit(c, RE_JMP, 0, start);
        c->pat->code[start].offset = (uint16_t)c->code_len;  /* patch: skip to end */
      }
      else if (ch == '+') {
        /* e+ -> body; SPLIT/SPLITNG(start)
           SPLIT: first=pc+1(end), second=offset(start) -> non-greedy
           SPLITNG: first=offset(start), second=pc+1(end) -> greedy */
        emit(c, nongreedy ? RE_SPLIT : RE_SPLITNG, 0, start);
      }
      else { /* ? */
        /* e? -> SPLIT(body, end); body; end: */
        insert_inst(c, start, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
        c->pat->code[start].offset = (uint16_t)c->code_len;  /* patch: skip to end */
      }
      greedy_rep = !nongreedy;
      start = begin;
      continue;
    }

    if (ch != '{') return;

    const char *save = c->p;
    next_char(c);
    int min, max;
    mrb_bool ranged;
    if (!parse_quantifier(c, &min, &max, &ranged)) {
      c->p = save;
      return;  /* not a quantifier */
    }
    /* `{n}` has no non-greedy form, so a `?` after it is a quantifier and the
       loop takes it; `{n,m}` takes it as the marker. */
    mrb_bool nongreedy = (ranged && peek(c) == '?');
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
        c->pat->code[start].offset = (uint16_t)c->code_len;
      }
    }
    greedy_rep = FALSE;
    start = begin;
    /* `{0}` left nothing to repeat, and a quantifier over a body that matches
       empty matches empty, so what follows is read and emits nothing. */
    if (c->code_len == begin) {
      skip_quantifiers(c);
      return;
    }
  }
}

/* Compile a sequence of quantified atoms */
static void
compile_seq(re_compiler *c)
{
  for (;;) {
    /* One token ends here and the next may begin, so under /x this is where
       the whitespace between them goes: `a b` is `ab`, and the blanks before
       a '|' or a ')' are gone by the time compile_alt() looks for one. */
    skip_extended_space(c);
    int ch = peek(c);
    if (ch < 0 || ch == ')' || ch == '|') return;
    uint32_t code_before = c->code_len;
    const char *p_before = c->p;
    compile_quantified(c);
    if (c->code_len == code_before && c->p == p_before) {
      /* compile_quantified neither consumed input nor emitted code: the
         current character is a quantifier metacharacter with no atom to
         repeat (a leading `*`, `+`, `?`, or one after the toggle `(?i)`).
         CRuby raises RegexpError here; without this guard peek() never
         advances and the loop spins forever (A1). */
      compile_error(c, "target of repeat operator is not specified");
    }
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
    c->pat->code[pos].op = RE_SPLIT;
    c->pat->code[pos].a = 0;
    c->pat->code[pos].offset = (uint16_t)alt_starts[split_count - i];
  }

  /* Patch JMPs (they are right before each alt_starts[1..n-1]) to point to end */
  uint32_t end = c->code_len;
  for (int i = 1; i < num_alts; i++) {
    uint32_t jmp_pos = alt_starts[i] - 1;
    c->pat->code[jmp_pos].op = RE_JMP;
    c->pat->code[jmp_pos].offset = (uint16_t)end;
  }
}

/* Inside a character class, is `src` the start of a POSIX bracket [:name:]?
   Returns the position just past its closing "]", or NULL if it is not one.
   compile_charclass() consumes such a bracket as a unit, so its ']' does not
   end the class; a malformed one falls through and the '[' is an ordinary
   member. The scan below has to agree with the parser on this. */
static const char*
skip_posix_bracket(const char *src, const char *end)
{
  if (!(*src == '[' && src + 1 < end && src[1] == ':')) return NULL;
  const char *q = src + 2;
  while (q < end && *q != ':' && *q != ']') q++;
  /* Compare the distance rather than q + 1: the loop above stops with
     q == end for a bracket the pattern truncates, as in /[[:alpha/, and
     forming q + 1 from a one-past-the-end pointer is undefined even where
     the && never reads through it. */
  if (end - q >= 2 && q[0] == ':' && q[1] == ']') return q + 2;
  return NULL;
}

/*
 * Step over the one construct at `src` that a pattern scan must not read
 * into: an escape sequence, or a character class from its '[' through its
 * ']'. Returns the position just past it, having updated *in_class, or NULL
 * when the byte at `src` is neither and the caller has to handle it itself.
 * A class spans several calls, with *in_class carrying the state between
 * them, so the caller keeps one flag and starts it FALSE.
 *
 * An escape is stepped over at the width CRuby's pre-pass (re.c) reads it,
 * because preprocess_pattern() copies what is stepped over and removes
 * comments from what is not, and a comment removed from inside an escape
 * leaves the parser a different escape: `\u12(?#c)34` would reach it as the
 * valid `\u1234`, where CRuby has read `\u12(?` and rejected it before the
 * comment is removed. So the escapes spelled with digits take their digits
 * here:
 *
 * - `\u{...}` runs through its '}' (an unterminated list to the end): a `#`
 *   inside it is a malformed list for the parser to reject, not a comment
 *   for the pass to remove, and a "(?<" inside it declares nothing.
 * - `\uXXXX` takes the next four bytes whatever they are, so that a byte
 *   that is not a hex digit reaches the parser and is rejected there rather
 *   than replaced by whatever a removed comment brings next.
 * - `\x` takes up to two hex digits, and with none of them the one byte
 *   after, which the parser rejects. `\0` takes up to two more octal digits.
 *   Both stop short of their full width at the first byte that is not a
 *   digit, and a digit a removed comment brings next would lengthen them:
 *   `\x6(?#c)1` is `\x06` and `1` in CRuby, whose pre-pass has read `\x6`
 *   whole and written it at full width before it reaches the comment. *pad
 *   says how many '0' bytes bring the escape to full width, for the pass to
 *   write after the letter; a full-width or non-numeric escape sets it to 0.
 *
 * `\1`-`\9` are the backslash and the digit, and the digits after them are
 * plain bytes, as they are to CRuby's pre-pass: a removed comment does join
 * them, and `\1(?#c)0` is `\10` there and here.
 *
 * preprocess_pattern() and has_named_group() both walk the pattern hunting
 * for a "(?" opener, and both have to agree with the parser on when a '(' is
 * an opener rather than an escaped or bracketed byte. The rules for that live
 * here alone, so a correction to them cannot be made in one walk and missed
 * in the other.
 */
static const char*
skip_uninterpreted(const char *src, const char *end, mrb_bool *in_class, int *pad)
{
  char ch = *src;

  *pad = 0;
  if (ch == '\\' && src + 1 < end) {
    char kind = src[1];
    src += 2;
    if (kind == 'u') {
      if (src < end && *src == '{') {
        while (src < end) {
          if (*src++ == '}') break;
        }
      }
      else {
        src += (end - src < 4) ? end - src : 4;
      }
    }
    else if (kind == 'x') {
      int n = 0;
      while (n < 2 && src < end && hex_value(*src) >= 0) { src++; n++; }
      if (n == 0) { if (src < end) src++; }
      else *pad = 2 - n;
    }
    else if (kind == '0') {
      int n = 0;
      while (n < 2 && src < end && *src >= '0' && *src <= '7') { src++; n++; }
      *pad = 2 - n;
    }
    return src;
  }

  if (*in_class) {
    /* A POSIX bracket is consumed as a unit by compile_charclass(), so the
       ']' that closes it does not close the class. */
    const char *q = skip_posix_bracket(src, end);
    if (q) return q;
    if (ch == ']') *in_class = FALSE;
    return src + 1;
  }

  if (ch == '[') {
    *in_class = TRUE;
    src++;
    /* A ']' written first is a literal member, optionally after '^',
       mirroring the `first` flag in compile_charclass(). */
    if (src < end && *src == '^') src++;
    if (src < end && *src == ']') src++;
    return src;
  }

  return NULL;
}

/* Read the letters of an inline option group whose "(?" starts at `src`,
   as far as the comment pass needs them: whether the group is one,
   whether it is the toggle form (?imx) rather than the scoped (?imx:...),
   and what it makes of x. `*x_on` is left as it was when the letters do
   not name x. Returns the terminator's position, or NULL when the bytes
   are not an option group at all, which includes every malformed one: the
   parser reads those bytes too and reports them, and this pass has only to
   agree with it about the well-formed ones.

   The letters are optional here as they are in parse_inline_flags(), so a
   plain `(?:` is read as a scoped group carrying no options; that is what
   it is, and the pass does the same with it either way, opening a scope
   and leaving x as it stands. */
static const char*
scan_option_group(const char *src, const char *end, mrb_bool *toggle, mrb_bool *x_on)
{
  if (end - src < 3 || src[1] != '?') return NULL;
  const char *q = src + 2;
  mrb_bool negate = FALSE;
  for (; q < end; q++) {
    if (*q == '-') negate = TRUE;
    else if (*q == 'x') *x_on = !negate;
    else if (*q != 'i' && *q != 'm') break;
  }
  if (q >= end || (*q != ')' && *q != ':')) return NULL;
  *toggle = (*q == ')');
  return q;
}

/* The comment pass's scope stack: one bit per open group, holding what
   extended mode was outside it. */
static void
scope_set(uint8_t *scope, mrb_int depth, mrb_bool extended)
{
  uint8_t bit = (uint8_t)(1u << (depth & 7));
  if (extended) scope[depth >> 3] |= bit;
  else scope[depth >> 3] &= (uint8_t)~bit;
}

static mrb_bool
scope_get(const uint8_t *scope, mrb_int depth)
{
  return (scope[depth >> 3] >> (depth & 7)) & 1;
}

/*
 * Rewrite the pattern before the parser sees it: remove the (?#...) comment
 * groups, under any flags, and wherever extended mode is in effect the `#`
 * comments too. That is what CRuby's own pre-pass (re.c) removes before its
 * tokenizer reads the pattern, and it is why the removal is done here rather
 * than by the parser: taking a comment out of the source joins the bytes on
 * either side of it, and CRuby has `\1(?#c)0` and `\1#c`, a newline and `0`
 * both as `\10`. The whitespace of free-spacing mode is another matter. It
 * separates tokens and never joins anything, so the parser skips it between
 * tokens (skip_extended_space()) and reads every token, escapes and group
 * names included, with the whitespace still in place.
 *
 * Extended mode is in effect from the start when the Regexp carries /x, and
 * it is switched inside the pattern by the inline option groups: (?x) and
 * (?-x) for the rest of the enclosing group, (?x:...) and (?-x:...) for
 * their own body. So this pass keeps a stack of one bit per open group,
 * pushed at every '(' it interprets and popped at every ')', which is what
 * lets a toggle end where its group does. That is the same scoping the
 * parser gives x for the whitespace; the `#` comments have to be resolved
 * here because the bytes they cover are gone before the parser reads them.
 * A `#` inside [...] is a member of the class, and so is a (?# written
 * there. Escaped characters (\ followed by anything) are preserved.
 * skip_uninterpreted() decides which bytes those are, and it also says when
 * a numeric escape is to be written at full width, `\x6` as `\x06`, so that
 * a digit a removed comment brings next cannot lengthen it.
 *
 * The buffer comes from the GC arena, which holds it until the caller's frame
 * is gone: the parser reads it from beginning to end, and every raise in
 * between leaves this function nothing to be reached through. The scope
 * stack lives behind the rewritten pattern in the same allocation: a group
 * opener is one byte of source, so len bits is room for every group. The
 * pattern part is twice the source: writing an escape at full width
 * lengthens it by two bytes at most, `\0` to `\000`, and nothing else the
 * pass writes is longer than what it read.
 */
static char*
preprocess_pattern(mrb_state *mrb, const char *src, mrb_int len,
                   mrb_bool extended, mrb_int *out_len)
{
  char *buf = (char*)mrb_temp_alloc(mrb, (size_t)len * 2 + ((size_t)len + 7) / 8);
  uint8_t *scope = (uint8_t*)buf + len * 2;
  mrb_int depth = 0;
  mrb_int o = 0;
  mrb_bool in_class = FALSE;
  const char *end = src + len;

  while (src < end) {
    char ch = *src;
    /* An escape or a character class is copied through untouched: neither
       holds a comment. A numeric escape short of its full width is padded
       with zeros after its letter, `\x6` to `\x06`. */
    int pad;
    const char *skip = skip_uninterpreted(src, end, &in_class, &pad);
    if (skip) {
      if (pad) {
        buf[o++] = *src++;
        buf[o++] = *src++;
        while (pad-- > 0) buf[o++] = '0';
      }
      while (src < skip) buf[o++] = *src++;
      continue;
    }
    if (ch == ')') {
      /* An unmatched ')' has no scope to close; the parser reports it. */
      if (depth > 0) extended = scope_get(scope, --depth);
      buf[o++] = *src++;
      continue;
    }
    if (ch == '(' && end - src >= 3 && src[1] == '?' && src[2] == '#') {
      /* Comment group: ends at the first ')' not preceded by a backslash.
         It does not nest, so (?#a(?#b)) closes at the first ')' and leaves
         the second one to be reported as unmatched, as CRuby does.
         Dropping the group here rather than in compile_atom() is what lets
         it stand where an atom cannot: CRuby compiles "a(?#x)*" as "a*", and
         an atom that emits no instruction cannot be a quantifier's target.
         An unterminated group is copied through instead, so that
         compile_atom() raises on it. */
      const char *q = src + 3;
      while (q < end && *q != ')') {
        if (*q == '\\' && q + 1 < end) q++;
        q++;
      }
      if (q < end) {
        src = q + 1;
        continue;
      }
      buf[o++] = *src++;
      buf[o++] = *src++;
      buf[o++] = *src++;
      continue;
    }
    if (ch == '(') {
      mrb_bool toggle = FALSE, x_on = extended;
      const char *term = scan_option_group(src, end, &toggle, &x_on);
      if (term && toggle) {
        /* (?imx) changes the rest of the enclosing group and opens none of
           its own, so the stack is left alone. The group is copied through
           for the parser, which applies all three letters from the same
           bytes. */
        while (src <= term) buf[o++] = *src++;
        extended = x_on;
        continue;
      }
      /* Every other '(' opens a group whose ')' restores what x is now:
         (?imx:...) after setting it for its body, and a plain, named,
         non-capturing or lookaround group after leaving it as it is. */
      scope_set(scope, depth++, extended);
      if (term) {
        while (src <= term) buf[o++] = *src++;
        extended = x_on;
        continue;
      }
      buf[o++] = *src++;
      continue;
    }
    if (extended && ch == '#') {
      /* Skip to the end of the line, newline included: the comment is one
         removed span, and the newline is not left for the parser to read
         as whitespace where the comment stood inside a token. */
      while (src < end && *src != '\n') src++;
      if (src < end) src++;
      continue;
    }
    buf[o++] = *src++;
  }
  *out_len = o;
  return buf;
}

/*
 * Does the pattern declare a named group anywhere? Answering this before the
 * parser starts is what lets compile_atom() demote a plain (...) that comes
 * before the named group that causes the demotion.
 *
 * A definition has two spellings, so the scan looks for "(?<" and "(?'" both.
 * It excludes (?<= and (?<!, which are lookbehind rather than a definition,
 * and it steps over escapes and character classes with skip_uninterpreted(),
 * so that /\(?/ and /[(?<]/ are not false positives.
 *
 * A truncated "(?<" or "(?'" at the end of the pattern is counted as a named
 * group, which is harmless: the parser reaches the same bytes and raises
 * there.
 */
static mrb_bool
has_named_group(const char *src, mrb_int len)
{
  const char *end = src + len;
  mrb_bool in_class = FALSE;

  while (src < end) {
    char ch = *src;
    int pad;
    const char *skip = skip_uninterpreted(src, end, &in_class, &pad);
    if (skip) {
      src = skip;
      continue;
    }
    if (ch == '(' && end - src >= 3 && src[1] == '?') {
      if (src[2] == '\'') return TRUE;
      if (src[2] == '<') {
        /* src + 3 is at most end here, since the test above leaves three
           bytes to read, so the one-past-the-end pointer it can form is a
           position C allows. */
        if (src + 3 >= end || (src[3] != '=' && src[3] != '!')) return TRUE;
        src += 3;
        continue;
      }
    }
    src++;
  }
  return FALSE;
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
    case RE_ATOMIC: case RE_ATOMIC_END:
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
      if (!class_is_ascii_only(cc)) return FALSE;  /* non-ASCII possible */
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

/* TRUE when a path that need not consume runs from pc to goal, so the
   repetition that goal closes can complete an iteration without consuming.
   A lookaround is zero-width whatever its sub-pattern does, so the walk
   steps over the sub-pattern; a backreference to a group that captured
   empty consumes nothing, so it can be on such a path. seen[] is marked with
   `mark` rather than cleared, so one buffer serves every edge. */
static mrb_bool
epsilon_path(const re_inst *code, uint32_t pc, uint32_t goal,
             uint32_t *seen, uint32_t mark)
{
  while (pc != goal) {
    if (pc > goal || seen[pc] == mark) return FALSE;
    seen[pc] = mark;
    switch (code[pc].op) {
    case RE_SAVE:
    case RE_BOL: case RE_EOL: case RE_BOT: case RE_EOT: case RE_EOTNL:
    case RE_WBOUND: case RE_NWBOUND:
    case RE_ATOMIC: case RE_ATOMIC_END:
    case RE_BACKREF:
      pc++;
      break;
    case RE_JMP:
    case RE_LOOKAHEAD: case RE_NEG_LOOKAHEAD:
    case RE_LOOKBEHIND: case RE_NEG_LOOKBEHIND:
      pc = code[pc].offset;
      break;
    case RE_SPLIT:
    case RE_SPLITNG:
      if (epsilon_path(code, code[pc].offset, goal, seen, mark)) return TRUE;
      pc++;
      break;
    default:
      return FALSE;  /* consumes input */
    }
  }
  return TRUE;
}

/* Find the repetitions whose body can match empty and mark the backward edge
   that closes each one, so that both engines know which loops need the
   empty-iteration handling (add_thread() and bt_match()) and which can stay
   on the cheap path. A repetition laid out as e* is closed by a jump back to
   its SPLIT/SPLITNG head, and that head is marked too: it is where an
   iteration begins, which the backtracker has to record; see bt_iter(). The
   head is a forward edge, and a forward edge's mark means nothing else.
   Returns how deeply the marked loops nest, which bounds the VM's epsilon
   passes and the thread lists sized from them; see RE_MAX_PASS and
   RE_LIST_CAPA. */
static uint8_t
mark_empty_loops(mrb_state *mrb, re_inst *code, uint32_t code_len)
{
  /* One block holds both arrays. Asking twice puts a raising call between the
     first allocation and anything that could free it: mrb_calloc() raises
     when it cannot answer, and this frame is the only owner `delta` has.
     code_len is capped at RE_MAX_CODE_LEN, so the doubled element count stays
     well inside the overflow check mrb_calloc() makes. */
  uint32_t n = code_len + 1;
  int32_t *delta = (int32_t*)mrb_calloc(mrb, 2 * (size_t)n, sizeof(int32_t));
  uint32_t *seen = (uint32_t*)(delta + n);
  uint32_t mark = 0;

  for (uint32_t pc = 0; pc < code_len; pc++) {
    re_inst in = code[pc];
    if (in.op != RE_JMP && in.op != RE_SPLIT && in.op != RE_SPLITNG) continue;
    code[pc].a = 0;                /* this pass owns `a` on the edge opcodes */
    if (in.offset > pc) continue;  /* forward edge: alternation, not a loop */
    if (!epsilon_path(code, in.offset, pc, seen, ++mark)) continue;
    code[pc].a = 1;
    if (in.op == RE_JMP) {
      /* The head was passed earlier in this scan, so its mark stays. */
      mrb_assert(code[in.offset].op == RE_SPLIT || code[in.offset].op == RE_SPLITNG);
      code[in.offset].a = 1;
    }
    delta[in.offset]++;
    delta[pc + 1]--;  /* the closing edge itself still sits inside the loop */
  }

  int32_t depth = 0, max = 0;
  for (uint32_t pc = 0; pc < code_len; pc++) {
    depth += delta[pc];
    if (depth > max) max = depth;
  }
  mrb_free(mrb, delta);
  return max > UINT8_MAX ? UINT8_MAX : (uint8_t)max;
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

void
mrb_re_compile(mrb_state *mrb, mrb_regexp_pattern *pat,
               const char *pattern, mrb_int len, uint32_t flags)
{
  re_compiler c;
  memset(&c, 0, sizeof(c));

  c.mrb = mrb;
  c.pat = pat;
  c.orig = pattern;
  c.orig_end = pattern + len;

  /* Both things the pre-pass removes, a (?#...) group and a `#` comment,
     are spelled with a '#', so a pattern without one is parsed as written.
     A '#' that is escaped or a class member is a false positive here, which
     costs the pass and nothing else: the pass steps over both. */
  if (memchr(pattern, '#', (size_t)len)) {
    mrb_int slen;
    pattern = preprocess_pattern(mrb, pattern, len,
                                 (flags & RE_FLAG_EXTENDED) != 0, &slen);
    len = slen;
  }
  c.src = pattern;
  c.src_end = pattern + len;
  c.p = pattern;
  c.flags = flags;
  c.num_captures = 1;  /* group 0 = whole match */
  /* Scan the same bytes the parser is about to read: preprocess_pattern() has
     already taken out the #comments and the (?#...) groups. */
  c.dont_capture = has_named_group(pattern, len);

  pat->flags = flags;

  /* group 0 start */
  emit(&c, RE_SAVE, 0, 0);

  compile_alt(&c);

  if (c.p < c.src_end) {
    compile_error(&c, "unmatched ')'");
  }

  /* A `\NN` names a group the pattern does not have. The count is taken here
     rather than where the reference stands because a reference may be written
     before the group it names, `\1(a)` being valid in CRuby. */
  if (c.max_backref > c.num_groups) {
    compile_error(&c, "invalid backref number/name");
  }

  /* group 0 end */
  emit(&c, RE_SAVE, 0, 1);
  emit(&c, RE_MATCH, 0, 0);

  /* code_len is the last field written, at the end of this function: the
     Regexp has been holding this pattern since before the compile started,
     and a non-zero code_len is what tells the rest of the gem that what it
     holds can be run (see re_uninitialized_p()). Everything below reads the
     local instead. */
  uint32_t code_len = c.code_len;
  pat->num_captures = c.num_captures;
  pat->num_named = c.num_named;

  /* Copy capture names into an owned arena. Until this point the names point
     into the pattern source, or into the preprocessed copy of it, which the
     GC arena owns and drops once this frame is gone. After this loop the
     regexp owns its names. */
  if (c.num_named > 0) {
    size_t total = 0;
    for (uint16_t i = 0; i < c.num_named; i++) total += pat->named_captures[i].name_len;
    /* total is zero only when every registered name is empty, which the
       parser rejects, so today the arena is always taken. Allocate a byte
       for that case anyway rather than skipping the copy: skipping it leaves
       name borrowing memory the pattern does not own, and nulling name
       instead would hand a NULL to the memcmp() in matchdata_name_to_group(),
       which is declared nonnull even for a zero length. */
    pat->named_arena = (char*)mrb_malloc(mrb, total ? total : 1);
    size_t off = 0;
    for (uint16_t i = 0; i < c.num_named; i++) {
      uint32_t n = pat->named_captures[i].name_len;
      memcpy(pat->named_arena + off, pat->named_captures[i].name, n);
      pat->named_captures[i].name = pat->named_arena + off;
      off += n;
    }
  }
  pat->has_backref = c.has_backref;
  pat->needs_backtrack = c.needs_backtrack;

  /* Extract literal prefix for fast search skip.
     Walk bytecode from the start, skipping SAVE, collecting RE_CHAR. */
  {
    uint8_t pbuf[256];
    int plen = 0;
    for (uint32_t i = 0; i < code_len && plen < 255; i++) {
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
    if (code_len == (uint32_t)(pat->prefix_len + 3) &&
        pat->code[0].op == RE_SAVE &&
        pat->code[code_len - 2].op == RE_SAVE &&
        pat->code[code_len - 1].op == RE_MATCH) {
      pat->is_literal = TRUE;
    }
  }

  /* Compute first-byte bitmap: set of bytes that could start a match.
     Used when prefix is empty (e.g. alternation, character class patterns). */
  {
    uint8_t bm[16];
    memset(bm, 0, sizeof(bm));
    pat->has_first_bytes = compute_first_set(pat->code, code_len, pat->classes, bm);
    if (pat->has_first_bytes) {
      memcpy(pat->first_bytes, bm, 16);
    }
  }

  pat->loop_depth = mark_empty_loops(mrb, pat->code, code_len);

  /* Pre-allocate VM state cache for pike_vm */
  {
    int list_capa = RE_LIST_CAPA(code_len, pat->loop_depth);
    pat->cached_visited = (uint32_t*)mrb_calloc(mrb, code_len + 1, sizeof(uint32_t));
    pat->cached_threads[0] = mrb_malloc(mrb, sizeof(re_thread_cache) * list_capa);
    pat->cached_threads[1] = mrb_malloc(mrb, sizeof(re_thread_cache) * list_capa);
    pat->cached_list_capa = list_capa;
    pat->cache_in_use = FALSE;
  }

  /* Nothing above allocates any more, so the pattern is complete: publish
     the length that says so. */
  pat->code_len = code_len;
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
