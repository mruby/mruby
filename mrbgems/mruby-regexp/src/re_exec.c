/*
** re_exec.c - NFA execution engine (Pike VM)
**
** Executes compiled regexp bytecode using Thompson/Pike NFA simulation.
** O(pattern * text) time complexity guarantees ReDoS resistance.
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"
#include <string.h>

/*
 * Skip to the next position where the pattern's literal prefix could match.
 * Uses memchr on the first byte for fast scanning, then verifies the rest.
 * Returns the found position, or NULL if no match is possible.
 */
static const char*
skip_to_prefix(const mrb_regexp_pattern *pat, const char *sp, const char *str_end)
{
  if (pat->prefix_len == 0) return sp;

  uint8_t first = pat->prefix[0];
  int plen = pat->prefix_len;

  while (sp + plen <= str_end) {
    const char *found = (const char*)memchr(sp, first, str_end - sp);
    if (!found || found + plen > str_end) return NULL;
    if (plen == 1 || memcmp(found + 1, pat->prefix + 1, plen - 1) == 0) {
      return found;
    }
    sp = found + 1;
  }
  return NULL;
}

/* Check if a byte is in the first-byte bitmap */
#define FIRST_BYTE_OK(pat, ch) \
  ((ch) >= 128 || ((pat)->first_bytes[(ch) >> 3] & (1 << ((ch) & 7))))

/* Check if character matches a character class */
static mrb_bool
class_match(const re_charclass *cc, uint8_t ch)
{
  if (ch >= 128) return cc->utf8_any;
  return (cc->bitmap[ch >> 3] >> (ch & 7)) & 1;
}

/*
 * Pike VM with optimized thread storage.
 *
 * Key optimizations vs naive approach:
 * - Captures stored in a flat pool, sized to actual ncap (not RE_MAX_CAPTURES)
 * - Generation counter for visited[] eliminates per-step memset
 * - Threads reference captures by pool index, avoiding 260-byte struct copies
 */

typedef struct {
  uint32_t pc;
  int cap_slot;  /* slot index into capture pool */
} re_thread;

typedef struct {
  re_thread *threads;
  int count;
  int capa;
} re_threadlist;

/* All Pike VM state */
typedef struct {
  mrb_state *mrb;
  const mrb_regexp_pattern *pat;
  int ncap;               /* actual capture count (num_captures * 2) */
  int *cap_pool;          /* flat: cap_pool[slot * ncap .. (slot+1) * ncap) */
  int pool_next;          /* next free slot */
  int pool_capa;          /* total slots allocated */
  uint32_t *visited;      /* generation-based */
  uint32_t gen;
  const char *str;
  const char *str_end;
  mrb_bool matched;
  mrb_bool match_only;    /* true: skip capture tracking (match? path) */
  int *result_caps;       /* best match (ncap ints) */
} pike_state;

static int
pool_alloc(pike_state *s)
{
  if (s->pool_next >= s->pool_capa) {
    int new_capa = s->pool_capa * 2;
    s->cap_pool = (int*)mrb_realloc(s->mrb, s->cap_pool,
                                     sizeof(int) * new_capa * s->ncap);
    s->pool_capa = new_capa;
  }
  return s->pool_next++;
}

static int
pool_copy(pike_state *s, int src_slot)
{
  int dst = pool_alloc(s);
  memcpy(&s->cap_pool[dst * s->ncap],
         &s->cap_pool[src_slot * s->ncap],
         sizeof(int) * s->ncap);
  return dst;
}

#define CAP(s, slot) (&(s)->cap_pool[(slot) * (s)->ncap])

/* Add thread following epsilon transitions.
   visited[pc] == gen means already visited this step. */
static void
add_thread(pike_state *s, re_threadlist *list,
           uint32_t pc, int cap_slot, const char *sp)
{
  for (;;) {
    if (pc >= s->pat->code_len) return;
    if (s->visited[pc] == s->gen) return;
    s->visited[pc] = s->gen;

    re_inst inst = s->pat->code[pc];
    switch (inst.op) {
    case RE_JMP:
      pc = inst.offset;
      continue;

    case RE_SPLIT:
      {
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, inst.offset, cp, sp);
      }
      pc++;
      continue;

    case RE_SPLITNG:
      {
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, pc + 1, cp, sp);
      }
      pc = inst.offset;
      continue;

    case RE_SAVE:
      if (!s->match_only) {
        CAP(s, cap_slot)[inst.offset] = (int)(sp - s->str);
      }
      pc++;
      continue;

    case RE_BOL:
      if (sp == s->str || ((s->pat->flags & RE_FLAG_MULTILINE) && sp > s->str && sp[-1] == '\n')) {
        pc++; continue;
      }
      return;

    case RE_EOL:
      if (sp == s->str_end || ((s->pat->flags & RE_FLAG_MULTILINE) && *sp == '\n')) {
        pc++; continue;
      }
      return;

    case RE_BOT:
      if (sp == s->str) { pc++; continue; }
      return;

    case RE_EOT:
      if (sp == s->str_end) { pc++; continue; }
      return;

    case RE_EOTNL:
      if (sp == s->str_end || (sp + 1 == s->str_end && *sp == '\n')) { pc++; continue; }
      return;

    case RE_WBOUND:
      {
        mrb_bool before = (sp > s->str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < s->str_end) && re_is_word_char((uint8_t)*sp);
        if (before != after) { pc++; continue; }
      }
      return;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > s->str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < s->str_end) && re_is_word_char((uint8_t)*sp);
        if (before == after) { pc++; continue; }
      }
      return;

    case RE_MATCH:
      s->matched = TRUE;
      if (s->result_caps) {
        memcpy(s->result_caps, CAP(s, cap_slot), sizeof(int) * s->ncap);
      }
      return;

    default:
      break;
    }
    break;
  }

  if (list->count < list->capa) {
    re_thread *t = &list->threads[list->count++];
    t->pc = pc;
    t->cap_slot = cap_slot;
  }
}

static int
pike_vm(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size)
{
  const char *sp = str + start;
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  int list_capa = (int)pat->code_len * 2 + 16;

  mrb_bool match_only = (captures == NULL || captures_size == 0);

  pike_state s;
  s.mrb = mrb;
  s.pat = pat;
  s.ncap = ncap;
  s.str = str;
  s.str_end = str_end;
  s.matched = FALSE;
  s.match_only = match_only;
  s.gen = 1;
  if (match_only) {
    /* no capture tracking needed; allocate minimal pool (1 dummy slot) */
    s.pool_capa = 1;
    s.pool_next = 0;
    s.cap_pool = (int*)mrb_malloc(mrb, sizeof(int) * ncap);
    s.result_caps = NULL;
  }
  else {
    s.pool_capa = list_capa * 2;
    s.pool_next = 0;
    s.cap_pool = (int*)mrb_malloc(mrb, sizeof(int) * s.pool_capa * ncap);
    s.result_caps = (int*)mrb_malloc(mrb, sizeof(int) * ncap);
    memset(s.result_caps, -1, sizeof(int) * ncap);
  }
  s.visited = (uint32_t*)mrb_calloc(mrb, pat->code_len + 1, sizeof(uint32_t));

  re_threadlist curr, next;
  curr.threads = (re_thread*)mrb_malloc(mrb, sizeof(re_thread) * list_capa);
  curr.count = 0; curr.capa = list_capa;
  next.threads = (re_thread*)mrb_malloc(mrb, sizeof(re_thread) * list_capa);
  next.count = 0; next.capa = list_capa;

  for (; sp <= str_end; sp++) {
    if (!s.matched) {
      /* Skip ahead when no active threads */
      if (curr.count == 0) {
        if (pat->prefix_len > 0) {
          const char *skip = skip_to_prefix(pat, sp, str_end);
          if (!skip) break;
          sp = skip;
        }
        else if (pat->has_first_bytes) {
          while (sp < str_end && !FIRST_BYTE_OK(pat, (uint8_t)*sp)) sp++;
          if (sp > str_end) break;
        }
      }
      int slot = match_only ? 0 : pool_alloc(&s);
      if (!match_only) memset(CAP(&s, slot), -1, sizeof(int) * ncap);
      s.gen++;
      add_thread(&s, &curr, 0, slot, sp);
      if (s.matched && curr.count == 0) break;
    }

    if (sp >= str_end) break;

    if (!match_only) {
      /* Compact: copy live thread captures to the front of the pool. */
      for (int i = 0; i < curr.count; i++) {
        if (curr.threads[i].cap_slot != i) {
          memcpy(CAP(&s, i), CAP(&s, curr.threads[i].cap_slot),
                 sizeof(int) * ncap);
          curr.threads[i].cap_slot = i;
        }
      }
      s.pool_next = curr.count;
    }

    s.gen++;
    next.count = 0;

    int ch = (uint8_t)*sp;
    int advance = re_utf8_charlen(sp, str_end);

    for (int i = 0; i < curr.count; i++) {
      re_thread *th = &curr.threads[i];
      if (th->pc >= pat->code_len) continue;

      re_inst inst = pat->code[th->pc];
      switch (inst.op) {
      case RE_CHAR:
        if (ch == inst.a) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + 1);
        }
        break;

      case RE_ANY:
        if (ch != '\n') {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      case RE_ANY_NL:
        {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      case RE_CLASS:
        if (class_match(&pat->classes[inst.a], (uint8_t)ch)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      case RE_NCLASS:
        if (!class_match(&pat->classes[inst.a], (uint8_t)ch)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      default:
        break;
      }
    }

    /* swap curr and next */
    {
      re_threadlist tmp = curr;
      curr = next;
      next = tmp;
    }

    if (s.matched && curr.count == 0) break;
  }

  int ret = 0;
  if (s.matched) {
    if (captures && s.result_caps) {
      int copy = ncap < captures_size ? ncap : captures_size;
      memcpy(captures, s.result_caps, sizeof(int) * copy);
    }
    ret = ncap > 0 ? ncap : 1;
  }

  mrb_free(mrb, curr.threads);
  mrb_free(mrb, next.threads);
  mrb_free(mrb, s.cap_pool);
  if (s.result_caps) mrb_free(mrb, s.result_caps);
  mrb_free(mrb, s.visited);

  return ret;
}

/*
 * Backtracking engine for patterns with backreferences.
 * Step-limited to prevent ReDoS.
 */
static mrb_bool
bt_match(const mrb_regexp_pattern *pat, const char *str, const char *str_end,
         const char *sp, uint32_t pc, int *captures, int ncap, int *steps)
{
  while (pc < pat->code_len) {
    if (++(*steps) > MRB_REGEXP_STEP_LIMIT) return FALSE;

    re_inst inst = pat->code[pc];
    switch (inst.op) {
    case RE_CHAR:
      if (sp >= str_end || (uint8_t)*sp != inst.a) return FALSE;
      sp++; pc++;
      break;

    case RE_ANY:
      if (sp >= str_end || *sp == '\n') return FALSE;
      sp += re_utf8_charlen(sp, str_end); pc++;
      break;

    case RE_ANY_NL:
      if (sp >= str_end) return FALSE;
      sp += re_utf8_charlen(sp, str_end); pc++;
      break;

    case RE_CLASS:
      if (sp >= str_end || !class_match(&pat->classes[inst.a], (uint8_t)*sp)) return FALSE;
      sp += re_utf8_charlen(sp, str_end); pc++;
      break;

    case RE_NCLASS:
      if (sp >= str_end || class_match(&pat->classes[inst.a], (uint8_t)*sp)) return FALSE;
      sp += re_utf8_charlen(sp, str_end); pc++;
      break;

    case RE_MATCH:
      return TRUE;

    case RE_JMP:
      pc = inst.offset;
      break;

    case RE_SPLIT:
      if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps)) return TRUE;
      pc = inst.offset;
      break;

    case RE_SPLITNG:
      if (bt_match(pat, str, str_end, sp, inst.offset, captures, ncap, steps)) return TRUE;
      pc++;
      break;

    case RE_SAVE:
      {
        int slot = inst.offset;
        if (slot < ncap) {
          int old = captures[slot];
          captures[slot] = (int)(sp - str);
          if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps)) return TRUE;
          captures[slot] = old;
        }
        return FALSE;
      }

    case RE_BOL:
      if (sp != str && !(pat->flags & RE_FLAG_MULTILINE && sp > str && sp[-1] == '\n')) return FALSE;
      pc++;
      break;

    case RE_EOL:
      if (sp != str_end && !(pat->flags & RE_FLAG_MULTILINE && *sp == '\n')) return FALSE;
      pc++;
      break;

    case RE_BOT:
      if (sp != str) return FALSE;
      pc++;
      break;

    case RE_EOT:
      if (sp != str_end) return FALSE;
      pc++;
      break;

    case RE_WBOUND:
      {
        mrb_bool before = (sp > str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && re_is_word_char((uint8_t)*sp);
        if (before == after) return FALSE;
      }
      pc++;
      break;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && re_is_word_char((uint8_t)*sp);
        if (before != after) return FALSE;
      }
      pc++;
      break;

    case RE_BACKREF:
      {
        int group = inst.a;
        int gs = captures[group * 2];
        int ge = captures[group * 2 + 1];
        if (gs < 0 || ge < 0) return FALSE;
        int blen = ge - gs;
        if (sp + blen > str_end) return FALSE;
        if (memcmp(sp, str + gs, blen) != 0) return FALSE;
        sp += blen;
        pc++;
      }
      break;

    case RE_LOOKAHEAD:
      if (!bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps))
        return FALSE;
      pc = inst.offset;
      break;

    case RE_NEG_LOOKAHEAD:
      if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps))
        return FALSE;
      pc = inst.offset;
      break;

    case RE_LOOKBEHIND:
      {
        int lb_len = inst.a;
        if (sp - str < lb_len) return FALSE;  /* not enough text before */
        if (!bt_match(pat, str, str_end, sp - lb_len, pc + 1, captures, ncap, steps))
          return FALSE;
        pc = inst.offset;
      }
      break;

    case RE_NEG_LOOKBEHIND:
      {
        int lb_len = inst.a;
        if (sp - str >= lb_len) {
          if (bt_match(pat, str, str_end, sp - lb_len, pc + 1, captures, ncap, steps))
            return FALSE;
        }
        /* if not enough text before, negative lookbehind succeeds */
        pc = inst.offset;
      }
      break;

    default:
      return FALSE;
    }
  }
  return FALSE;
}

static int
backtrack_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
               const char *str, mrb_int len, mrb_int start,
               int *captures, int captures_size)
{
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  int *caps = (int*)mrb_malloc(mrb, sizeof(int) * ncap);

  for (const char *sp = str + start; sp <= str_end; sp++) {
    /* Skip ahead using literal prefix or first-byte bitmap */
    if (pat->prefix_len > 0) {
      const char *skip = skip_to_prefix(pat, sp, str_end);
      if (!skip) break;
      sp = skip;
    }
    else if (pat->has_first_bytes) {
      while (sp < str_end && !FIRST_BYTE_OK(pat, (uint8_t)*sp)) sp++;
      if (sp > str_end) break;
    }
    memset(caps, -1, sizeof(int) * ncap);
    int steps = 0;

    if (bt_match(pat, str, str_end, sp, 0, caps, ncap, &steps)) {
      if (captures) {
        int copy = ncap < captures_size ? ncap : captures_size;
        memcpy(captures, caps, sizeof(int) * copy);
      }
      mrb_free(mrb, caps);
      return ncap > 0 ? ncap : 1;
    }
  }
  mrb_free(mrb, caps);
  return 0;
}

/* Fast path for pure literal patterns: use memchr+memcmp, no NFA needed */
static int
literal_exec(const mrb_regexp_pattern *pat,
             const char *str, mrb_int len, mrb_int start,
             int *captures, int captures_size)
{
  const char *sp = str + start;
  const char *str_end = str + len;
  int plen = pat->prefix_len;

  while (sp + plen <= str_end) {
    const char *found = (const char*)memchr(sp, pat->prefix[0], str_end - sp);
    if (!found || found + plen > str_end) return 0;
    if (plen == 1 || memcmp(found + 1, pat->prefix + 1, plen - 1) == 0) {
      /* match found */
      if (captures && captures_size >= 2) {
        captures[0] = (int)(found - str);
        captures[1] = (int)(found - str) + plen;
      }
      return 2;  /* group 0 start/end */
    }
    sp = found + 1;
  }
  return 0;
}

/* Public entry point */
int
re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size)
{
  if (pat->is_literal) {
    return literal_exec(pat, str, len, start, captures, captures_size);
  }
  if (pat->has_backref || pat->needs_backtrack) {
    return backtrack_exec(mrb, pat, str, len, start, captures, captures_size);
  }
  return pike_vm(mrb, pat, str, len, start, captures, captures_size);
}
