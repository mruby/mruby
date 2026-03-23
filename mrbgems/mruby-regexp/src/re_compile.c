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
  if (c->stripped) mrb_free(c->mrb, c->stripped);
  c->stripped = NULL;
  mrb_raisef(c->mrb, mrb_exc_get_id(c->mrb, MRB_SYM(RegexpError)), "%s: /%s/", msg, c->src);
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
   Adjusts all jump offsets >= pos by +1. */
static void
insert_inst(re_compiler *c, uint32_t pos, uint8_t op, uint8_t a, uint16_t offset)
{
  emit(c, RE_JMP, 0, 0);  /* grow array */
  uint32_t len = c->code_len - 1 - pos;
  memmove(&c->code[pos + 1], &c->code[pos], sizeof(re_inst) * len);
  c->code[pos].op = op;
  c->code[pos].a = a;
  c->code[pos].offset = offset;

  /* fix all jump targets that point at or past the insertion point */
  for (uint32_t i = 0; i < c->code_len; i++) {
    if (i == pos) continue;
    switch (c->code[i].op) {
    case RE_JMP: case RE_SPLIT: case RE_SPLITNG:
      if (c->code[i].offset >= pos && c->code[i].offset < 0xffff) {
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

static uint16_t
add_class(re_compiler *c)
{
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
      if (!re_is_word_char(i)) class_set_bit(cc, (uint8_t)i);
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
  }
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
  default: return ch;  /* literal: \., \\, \/, \(, etc. */
  }
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
    int ch;

    if (peek(c) < 0) compile_error(c, "unterminated character class");
    first = FALSE;

    if (peek(c) == '\\') {
      next_char(c);
      int esc = peek(c);
      if (esc == 'd' || esc == 'D' || esc == 'w' || esc == 'W' || esc == 's' || esc == 'S') {
        next_char(c);
        class_add_shorthand(cc, esc);
        continue;
      }
      ch = parse_escape(c);
    }
    else {
      ch = next_char(c);
    }

    /* check for range a-z */
    if (peek(c) == '-' && c->p + 1 < c->src_end && c->p[1] != ']') {
      next_char(c);  /* skip '-' */
      int hi;
      if (peek(c) == '\\') {
        next_char(c);
        hi = parse_escape(c);
      }
      else {
        hi = next_char(c);
      }
      if (ch < 128 && hi < 128) {
        class_set_range(cc, (uint8_t)ch, (uint8_t)hi);
      }
    }
    else {
      if (ch < 128) class_set_bit(cc, (uint8_t)ch);
    }
  }
  next_char(c);  /* skip ']' */

  cc->negated = negated;
  emit(c, negated ? RE_NCLASS : RE_CLASS, (uint8_t)id, 0);
}

/* Parse {n}, {n,}, {n,m} quantifier. Returns min,max via pointers. */
static mrb_bool
parse_quantifier(re_compiler *c, int *min_out, int *max_out)
{
  const char *save = c->p;
  int min = 0, max = -1;

  while (peek(c) >= '0' && peek(c) <= '9') {
    min = min * 10 + (next_char(c) - '0');
  }
  if (peek(c) == ',') {
    next_char(c);
    if (peek(c) >= '0' && peek(c) <= '9') {
      max = 0;
      while (peek(c) >= '0' && peek(c) <= '9') {
        max = max * 10 + (next_char(c) - '0');
      }
    }
    /* else max = -1 (unlimited) */
  }
  else {
    max = min;  /* {n} means exactly n */
  }
  if (peek(c) != '}') {
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
      emit(c, (ch >= 'A' && ch <= 'Z') ? RE_NCLASS : RE_CLASS, (uint8_t)id, 0);
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

  default:
    if (ch < 0 || ch == ')' || ch == '|' || ch == '*' || ch == '+' || ch == '?' || ch == '{') {
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

    /* First, we have one copy already. We need min-1 more mandatory copies. */
    for (int i = 1; i < min; i++) {
      for (uint32_t j = 0; j < atom_size; j++) {
        emit(c, c->code[start + j].op, c->code[start + j].a, c->code[start + j].offset);
      }
    }
    /* Then optional copies */
    if (max < 0) {
      /* {n,} = min copies + * */
      uint32_t loop_start = c->code_len;
      uint32_t split_pos = emit(c, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
      for (uint32_t j = 0; j < atom_size; j++) {
        emit(c, c->code[start + j].op, c->code[start + j].a, c->code[start + j].offset);
      }
      emit(c, RE_JMP, 0, loop_start);
      patch(c, split_pos, c->code_len);
    }
    else {
      for (int i = min; i < max; i++) {
        uint32_t split_pos = emit(c, nongreedy ? RE_SPLITNG : RE_SPLIT, 0, 0);
        for (uint32_t j = 0; j < atom_size; j++) {
          emit(c, c->code[start + j].op, c->code[start + j].a, c->code[start + j].offset);
        }
        patch(c, split_pos, c->code_len);
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

  /* Now set up SPLIT chain: each SPLIT tries next instruction or jumps to alt */
  for (uint32_t i = 0; i < split_count; i++) {
    uint32_t pos = alt_starts[0] - split_count + i;
    c->code[pos].op = RE_SPLIT;
    c->code[pos].a = 0;
    c->code[pos].offset = (uint16_t)alt_starts[i + 1];
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
      bm[code[pc].a >> 3] |= (1 << (code[pc].a & 7));
      return TRUE;
    case RE_CLASS: {
      const re_charclass *cc = &classes[code[pc].a];
      for (int i = 0; i < 16; i++) bm[i] |= cc->bitmap[i];
      if (cc->utf8_any) return FALSE;  /* non-ASCII possible */
      return TRUE;
    }
    case RE_NCLASS: {
      /* negated class: complement of bitmap. Too many bits; not useful. */
      return FALSE;
    }
    case RE_ANY: case RE_ANY_NL:
      return FALSE;  /* any byte possible */
    case RE_MATCH:
      return TRUE;  /* empty match; first_bytes still valid for other branches */
    default:
      return FALSE;
    }
  }
  return TRUE;
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
re_compile(mrb_state *mrb, const char *pattern, mrb_int len, uint32_t flags)
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
  pat->num_named = c.num_named;
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
re_free(mrb_state *mrb, mrb_regexp_pattern *pat)
{
  if (pat) {
    mrb_free(mrb, pat->code);
    mrb_free(mrb, pat->classes);
    mrb_free(mrb, pat->named_captures);
    mrb_free(mrb, pat->prefix);
    mrb_free(mrb, pat->cached_visited);
    mrb_free(mrb, pat->cached_threads[0]);
    mrb_free(mrb, pat->cached_threads[1]);
    mrb_free(mrb, pat);
  }
}
