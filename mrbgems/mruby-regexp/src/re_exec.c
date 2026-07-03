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

/* Check if a codepoint matches a character class. ASCII (cp < 128) hits
   the bitmap; non-ASCII falls back to the inclusive (lo, hi) range list,
   then to the utf8_any catch-all (used by negated shorthand like \D). */
static mrb_bool
class_match(const re_charclass *cc, uint32_t cp)
{
  if (cp < 128) {
    return (cc->bitmap[cp >> 3] >> (cp & 7)) & 1;
  }
  for (uint16_t i = 0; i < cc->num_ranges; i++) {
    if (cp >= cc->ranges[2*i] && cp <= cc->ranges[2*i + 1]) return TRUE;
  }
  return cc->utf8_any;
}

/*
 * Pike VM with optimized thread storage.
 *
 * Key optimizations vs naive approach:
 * - Captures stored in a flat pool, sized to actual ncap (not RE_MAX_CAPTURES)
 * - Generation counter for visited[] eliminates per-step memset
 * - Threads reference captures by pool index, avoiding 260-byte struct copies
 */

typedef re_thread_cache re_thread;

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
  mrb_bool binary;        /* true: subject is byte-indexed ASCII-8BIT */
  mrb_bool cut;           /* a higher-priority thread matched this step:
                             stop adding/processing lower-priority threads */
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
    if (s->cut) return;
    if (pc >= s->pat->code_len) return;
    if (s->visited[pc] == s->gen) return;
    s->visited[pc] = s->gen;

    re_inst inst = s->pat->code[pc];
    switch (inst.op) {
    case RE_JMP:
      pc = inst.offset;
      continue;

    case RE_SPLIT:
      /* Greedy fork: the fall-through (pc+1) outranks the jump target, the
         same priority order the backtracking engine uses. Explore the
         higher-priority branch first so it claims shared pcs (visited[]) and
         reaches a match before the lower one. Snapshot the captures before
         pc+1's closure can mutate the shared slot; the jump branch then runs
         on that snapshot. */
      {
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, pc + 1, cap_slot, sp);
        if (s->cut) return;
        pc = inst.offset;
        cap_slot = cp;
      }
      continue;

    case RE_SPLITNG:
      /* Non-greedy fork: the jump target outranks the fall-through. */
      {
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, inst.offset, cap_slot, sp);
        if (s->cut) return;
        pc = pc + 1;
        cap_slot = cp;
      }
      continue;

    case RE_SAVE:
      if (!s->match_only) {
        CAP(s, cap_slot)[inst.offset] = (int)(sp - s->str);
      }
      pc++;
      continue;

    case RE_BOL:
      /* ^ always matches at a line start (string start or just after a \n);
         Ruby's /m only affects `.`, not the line anchors. \A is RE_BOT. A
         trailing \n does not open a final line, so ^ does not match at the
         very end. */
      if (sp == s->str || (sp != s->str_end && sp[-1] == '\n')) {
        pc++; continue;
      }
      return;

    case RE_EOL:
      /* $ always matches at a line end (string end or just before a \n). */
      if (sp == s->str_end || *sp == '\n') {
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
        mrb_bool before = (sp > s->str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < s->str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before != after) { pc++; continue; }
      }
      return;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > s->str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < s->str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before == after) { pc++; continue; }
      }
      return;

    case RE_MATCH:
      s->matched = TRUE;
      if (s->result_caps) {
        memcpy(s->result_caps, CAP(s, cap_slot), sizeof(int) * s->ncap);
      }
      /* Leftmost-first: this is the highest-priority thread to reach a match
         this step (closures run in priority order), so cut every lower one.
         A surviving higher-priority thread can still match later and override
         this in a subsequent step, which is the correct greedy/longest case. */
      s->cut = TRUE;
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
    t->sp = sp;
  }
}

static int
pike_vm(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size, mrb_bool binary)
{
  const char *sp = str + start;
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  int list_capa = (int)pat->code_len * 2 + 16;

  mrb_bool match_only = (captures == NULL || captures_size == 0);

  /* Use cached VM state if available (avoids malloc per call) */
  mrb_regexp_pattern *mpat = (mrb_regexp_pattern*)pat;  /* for cache_in_use flag */
  mrb_bool use_cache = !mpat->cache_in_use && mpat->cached_visited != NULL;
  if (use_cache) mpat->cache_in_use = TRUE;

  pike_state s;
  s.mrb = mrb;
  s.pat = pat;
  s.ncap = ncap;
  s.str = str;
  s.str_end = str_end;
  s.matched = FALSE;
  s.match_only = match_only;
  s.binary = binary;
  s.cut = FALSE;
  s.gen = 1;
  if (match_only) {
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

  re_threadlist curr, next;
  if (use_cache) {
    s.visited = mpat->cached_visited;
    memset(s.visited, 0, sizeof(uint32_t) * (pat->code_len + 1));
    curr.threads = (re_thread*)mpat->cached_threads[0];
    next.threads = (re_thread*)mpat->cached_threads[1];
    curr.capa = next.capa = mpat->cached_list_capa;
  }
  else {
    s.visited = (uint32_t*)mrb_calloc(mrb, pat->code_len + 1, sizeof(uint32_t));
    curr.threads = (re_thread*)mrb_malloc(mrb, sizeof(re_thread) * list_capa);
    next.threads = (re_thread*)mrb_malloc(mrb, sizeof(re_thread) * list_capa);
    curr.capa = next.capa = list_capa;
  }
  curr.count = next.count = 0;

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
      /* Don't seed a new match attempt at a UTF-8 continuation byte --
         a multi-byte char's interior is not a valid char boundary, and
         starting a thread there mis-decodes the char (e.g. a class-
         match on a stray 0x82 instead of the leader's full codepoint). */
      if (!s.binary && curr.count == 0 && sp < str_end && mrb_re_utf8_continuation_p(sp)) {
        continue;
      }
      int slot = match_only ? 0 : pool_alloc(&s);
      if (!match_only) memset(CAP(&s, slot), -1, sizeof(int) * ncap);
      s.gen++;
      s.cut = FALSE;
      add_thread(&s, &curr, 0, slot, sp);
      if (s.matched && curr.count == 0) break;
    }

    if (sp >= str_end) break;

    if (!match_only && curr.count > 0) {
      /* Renumber each live thread's capture slot to its list index so the
         pool can be reset to curr.count. Stage the copies through freshly
         allocated tail slots first: writing straight to CAP(i) would clobber
         a low slot that a later thread (index j > i) still needs to read
         whenever the slot assignment is a non-identity permutation -- which
         happens once alternation reorders threads relative to their slot
         numbers. Tail slots are disjoint from every source slot, and the
         final block copy to the front is disjoint because pool_next >= count. */
      int base = s.pool_next;
      for (int i = 0; i < curr.count; i++) {
        int dst = pool_alloc(&s);
        memcpy(CAP(&s, dst), CAP(&s, curr.threads[i].cap_slot),
               sizeof(int) * ncap);
        curr.threads[i].cap_slot = i;
      }
      memcpy(&s.cap_pool[0], &s.cap_pool[base * ncap],
             sizeof(int) * ncap * curr.count);
      s.pool_next = curr.count;
    }

    s.gen++;
    s.cut = FALSE;
    next.count = 0;

    int ch = (uint8_t)*sp;
    int advance = mrb_re_charlen(sp, str_end, s.binary);
    /* Decoded codepoint of the current input char. Identical to `ch`
       for ASCII; lazily decoded only when the char is multi-byte. */
    uint32_t curr_cp = (uint32_t)ch;
    if (!s.binary && advance > 1) {
      int dlen = 0;
      curr_cp = mrb_re_decode_char(sp, str_end, &dlen, s.binary);
    }

    for (int i = 0; i < curr.count; i++) {
      re_thread *th = &curr.threads[i];
      if (th->pc >= pat->code_len) continue;
      /* A thread enqueued at sp+advance (RE_CLASS over a multi-byte
         char) waits in the list until the byte-stepped outer sp catches
         up to its own sp. Until then, carry it forward to next
         iteration's curr unchanged. */
      if (th->sp != sp) {
        if (next.count < next.capa) {
          next.threads[next.count++] = *th;
        }
        continue;
      }

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
        if (class_match(&pat->classes[inst.a], curr_cp)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      case RE_NCLASS:
        if (!class_match(&pat->classes[inst.a], curr_cp)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance);
        }
        break;

      default:
        break;
      }

      /* A higher-priority thread reached a match while building `next`; the
         remaining (lower-priority) threads in `curr` are cut for this step. */
      if (s.cut) break;
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

  if (use_cache) {
    mpat->cache_in_use = FALSE;
  }
  else {
    mrb_free(mrb, curr.threads);
    mrb_free(mrb, next.threads);
    mrb_free(mrb, s.visited);
  }
  mrb_free(mrb, s.cap_pool);
  if (s.result_caps) mrb_free(mrb, s.result_caps);

  return ret;
}

/*
 * Backtracking engine for patterns with backreferences.
 * Step-limited to prevent ReDoS.
 */
static mrb_bool
bt_match(const mrb_regexp_pattern *pat, const char *str, const char *str_end,
         const char *sp, uint32_t pc, int *captures, int ncap, int *steps,
         int depth, mrb_bool binary)
{
  if (depth > MRB_REGEXP_RECURSION_LIMIT) return FALSE;
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
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_ANY_NL:
      if (sp >= str_end) return FALSE;
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_CLASS:
      if (sp >= str_end) return FALSE;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        if (!class_match(&pat->classes[inst.a], cp_)) return FALSE;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_NCLASS:
      if (sp >= str_end) return FALSE;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        if (class_match(&pat->classes[inst.a], cp_)) return FALSE;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_MATCH:
      return TRUE;

    case RE_JMP:
      pc = inst.offset;
      break;

    case RE_SPLIT:
      if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps, depth + 1, binary)) return TRUE;
      pc = inst.offset;
      break;

    case RE_SPLITNG:
      if (bt_match(pat, str, str_end, sp, inst.offset, captures, ncap, steps, depth + 1, binary)) return TRUE;
      pc++;
      break;

    case RE_SAVE:
      {
        int slot = inst.offset;
        if (slot < ncap) {
          int old = captures[slot];
          captures[slot] = (int)(sp - str);
          if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps, depth + 1, binary)) return TRUE;
          captures[slot] = old;
        }
        return FALSE;
      }

    case RE_BOL:
      /* ^ always matches at a line start (see the Pike VM case); /m only
         affects `.`. \A is RE_BOT. A trailing \n opens no final line. */
      if (sp != str && (sp == str_end || sp[-1] != '\n')) return FALSE;
      pc++;
      break;

    case RE_EOL:
      /* $ always matches at a line end. */
      if (sp != str_end && *sp != '\n') return FALSE;
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
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before == after) return FALSE;
      }
      pc++;
      break;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before != after) return FALSE;
      }
      pc++;
      break;

    case RE_BACKREF:
      {
        int group = inst.a;
        if (group * 2 + 1 >= ncap) return FALSE;
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
      if (!bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps, depth + 1, binary))
        return FALSE;
      pc = inst.offset;
      break;

    case RE_NEG_LOOKAHEAD:
      if (bt_match(pat, str, str_end, sp, pc + 1, captures, ncap, steps, depth + 1, binary))
        return FALSE;
      pc = inst.offset;
      break;

    case RE_LOOKBEHIND:
      {
        int lb_len = inst.a;
        if (sp - str < lb_len) return FALSE;  /* not enough text before */
        if (!bt_match(pat, str, str_end, sp - lb_len, pc + 1, captures, ncap, steps, depth + 1, binary))
          return FALSE;
        pc = inst.offset;
      }
      break;

    case RE_NEG_LOOKBEHIND:
      {
        int lb_len = inst.a;
        if (sp - str >= lb_len) {
          if (bt_match(pat, str, str_end, sp - lb_len, pc + 1, captures, ncap, steps, depth + 1, binary))
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
               int *captures, int captures_size, mrb_bool binary)
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
    if (!binary && sp < str_end && mrb_re_utf8_continuation_p(sp)) {
      continue;
    }
    memset(caps, -1, sizeof(int) * ncap);
    int steps = 0;

    if (bt_match(pat, str, str_end, sp, 0, caps, ncap, &steps, 0, binary)) {
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
mrb_re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size, mrb_bool binary)
{
  if (pat->is_literal) {
    return literal_exec(pat, str, len, start, captures, captures_size);
  }
  if (pat->has_backref || pat->needs_backtrack) {
    return backtrack_exec(mrb, pat, str, len, start, captures, captures_size, binary);
  }
  return pike_vm(mrb, pat, str, len, start, captures, captures_size, binary);
}
