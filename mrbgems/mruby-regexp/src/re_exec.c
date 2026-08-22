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

/* Check if the current input character matches a character class. ASCII
   (cp < 128) hits the bitmap; non-ASCII falls back to the inclusive (lo, hi)
   range list, then to the types the class holds by POSIX bracket, then to the
   utf8_any catch-all (used by negated shorthand like \D).

   `raw` says the input is a byte rather than a character: a byte-indexed
   subject, or one whose byte at this position starts no whole character. It
   picks which half of the range list to read, since a byte member and a
   codepoint member of the same number are different members and arrive here as
   the same number. A byte has no type, so it is in the class through a
   negated bracket and not through a positive one. utf8_any is the answer for
   either, being about the byte being non-ASCII at all. */
static mrb_bool
class_match(const re_charclass *cc, uint32_t cp, mrb_bool raw)
{
  if (cp < 128) {
    return (cc->bitmap[cp >> 3] >> (cp & 7)) & 1;
  }
  if (raw) cp |= RE_CLASS_BYTE;
  for (uint32_t i = 0; i < cc->num_ranges; i++) {
    if (cp >= cc->ranges[2*i] && cp <= cc->ranges[2*i + 1]) return TRUE;
  }
#ifdef RE_UNICODE_CTYPE
  if (cc->ctype_yes | cc->ctype_no) return mrb_re_class_ctype_match(cc, cp);
#endif
  return cc->utf8_any;
}

/* The fold of one unit of the subject. A byte-indexed subject hands out
   bytes, and a byte above 127 is not the codepoint of the same value: 0xC0 is
   not U+00C0, so folding it to 0xE0 would pair two bytes that spell no letter
   in common. The letters a byte can spell are the ASCII ones, and those fold
   as they do everywhere. */
static inline uint32_t
subject_fold(uint32_t c, mrb_bool binary)
{
  if (binary && c >= 128) return c;
  return mrb_re_case_fold(c);
}

/* Compare two spans ignoring case. Returns how many bytes of `a` were
   consumed, or -1 when they differ. The count is not always the length of
   `b`: with Unicode folding a counterpart can be a different width (U+212A
   folds to 'k'), so the two spans can match while holding different numbers
   of bytes. */
static int
memcmp_ci(const char *a, const char *a_end, const char *b, const char *b_end,
          mrb_bool binary)
{
  const char *a0 = a;
  while (b < b_end) {
    if (a >= a_end) return -1;
    int alen = 0, blen = 0;
    uint32_t ca = mrb_re_decode_char(a, a_end, &alen, binary);
    uint32_t cb = mrb_re_decode_char(b, b_end, &blen, binary);
    if (subject_fold(ca, binary) != subject_fold(cb, binary)) return -1;
    a += alen;
    b += blen;
  }
  return (int)(a - a0);
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
  uint32_t gen;           /* visited key this step's first epsilon pass uses */
  uint32_t key_max;       /* highest key a further pass may reach this step */
  uint32_t pass_span;     /* keys one step reserves: key_max - gen + 1 */
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

/* Hand this step's closure a fresh block of visited keys. A step reserves one
   key per epsilon pass, so the counter climbs faster than the single step it
   used to; on a long enough subject it would wrap, leaving marks from earlier
   steps outranking every fresh key and the closure adding nothing. Clear the
   marks and start the keys over when that comes into reach, which also keeps
   a live key below RE_LOOP_STOP. */
static void
advance_gen(pike_state *s)
{
  if (s->key_max > UINT32_MAX - 2 * s->pass_span) {
    memset(s->visited, 0, sizeof(uint32_t) * (s->pat->code_len + 1));
    s->gen = 0;
    s->key_max = s->pass_span - 1;
  }
  s->gen += s->pass_span;
  s->key_max += s->pass_span;
}

/* TRUE once this step's closure has walked the loop head at pc, which means
   the body just ran without consuming: an empty iteration. */
static mrb_bool
loop_head_seen(pike_state *s, uint32_t pc)
{
  return s->visited[pc] >= s->gen;
}

/* A fork also closes e+: the body is laid out before it, so its jump target
   is the body start and pc+1 is the loop's exit. Returns the key the jump
   target's branch walks under, or RE_LOOP_STOP when the body matched empty
   and the repetition therefore has to stop. An ordinary fork, or one closing
   a loop whose body always consumes (`a` is 0, see mark_empty_loops()), has
   no empty iteration to account for and keeps the current key. advance_gen()
   holds every live key below the sentinel. */
#define RE_LOOP_STOP UINT32_MAX

static uint32_t
re_loop_back(pike_state *s, re_inst inst, uint32_t pc, uint32_t key)
{
  if (inst.offset > pc || !inst.a) return key;
  if (loop_head_seen(s, inst.offset)) return RE_LOOP_STOP;
  return key < s->key_max ? key + 1 : key;
}

/* Add thread following epsilon transitions. `key` is s->gen for the first
   pass over this step's closure and one higher per further pass, and
   visited[pc] holds the key of the pass that last walked pc: a later pass may
   re-walk what an earlier one marked, and no key is ever reused by a later
   step. */
static void
add_thread(pike_state *s, re_threadlist *list,
           uint32_t pc, int cap_slot, const char *sp, uint32_t key)
{
  for (;;) {
    if (s->cut) return;
    if (pc >= s->pat->code_len) return;
    if (s->visited[pc] >= key) return;
    s->visited[pc] = key;

    re_inst inst = s->pat->code[pc];
    switch (inst.op) {
    case RE_JMP:
      /* A backward jump closes a repetition (e*, e{n,}): it returns to the
         RE_SPLIT/RE_SPLITNG head, whose offset is the loop's exit. `a` is set
         only when that body can run empty (see mark_empty_loops()), which is
         the only case with a final empty iteration to account for. */
      if (inst.offset <= pc && inst.a) {
        uint32_t head = inst.offset;
        if (loop_head_seen(s, head)) {
          /* The head was walked at this position, so the iteration that just
             finished consumed nothing. Onigmo stops a repetition on an empty
             iteration and keeps what that iteration captured, so leave the
             loop from here rather than dying on the head's mark: this path
             outranks the exit the head itself queued before the body ran, and
             claims the exit pc first. */
          pc = s->pat->code[head].offset;
          continue;
        }
        /* The head is unmarked, so this closure resumed inside the body and
           the iteration it just finished is a real one. Run the next
           iteration in a fresh pass, past the marks the resumed tail left. */
        if (key < s->key_max) key++;
        pc = head;
        continue;
      }
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
        uint32_t back = re_loop_back(s, inst, pc, key);
        if (back == RE_LOOP_STOP) { pc++; continue; }
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, pc + 1, cap_slot, sp, key);
        if (s->cut) return;
        pc = inst.offset;
        cap_slot = cp;
        key = back;
      }
      continue;

    case RE_SPLITNG:
      /* Non-greedy fork: the jump target outranks the fall-through. */
      {
        uint32_t back = re_loop_back(s, inst, pc, key);
        if (back == RE_LOOP_STOP) { pc++; continue; }
        int cp = s->match_only ? 0 : pool_copy(s, cap_slot);
        add_thread(s, list, inst.offset, cap_slot, sp, back);
        if (s->cut) return;
        pc = pc + 1;
        cap_slot = cp;
      }
      continue;

    case RE_SAVE:
      /* Slot 1 is the end of group 0, so this is where the whole match
         closes. It may not close inside a character, the same rule the
         seeding loop applies to where a match opens. Killing the thread
         rather than the attempt lets a longer branch match instead. */
      if (inst.offset == 1 && !s->binary && sp < s->str_end &&
          mrb_re_char_interior_p(s->str, sp, s->str_end)) {
        return;
      }
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
        const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
        int *captures, int captures_size, mrb_bool binary)
{
  const char *start_cap = str + start_limit;
  const char *sp = str + start;
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  int list_capa = RE_LIST_CAPA(pat->code_len, pat->loop_depth);

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
  s.pass_span = RE_PASS_SPAN(pat->loop_depth);
  s.gen = 0;
  s.key_max = s.pass_span - 1;
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
    /* Past the last position a match may start at, only the threads already
       running can still answer; once they are gone nothing can. */
    if (!s.matched && sp > start_cap && curr.count == 0) break;
    if (!s.matched && sp <= start_cap) {
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
        if (sp > start_cap) break;
      }
      /* Don't seed a new match attempt inside a character. Its interior is
         not a char boundary, and starting a thread there mis-decodes the
         char (e.g. a class match on a stray 0x82 instead of the leader's
         full codepoint). A byte that no lead byte reaches belongs to no
         character and is a boundary of its own.
         Threads seeded earlier are still stepped at this position, so the
         test guards the seeding alone and never skips the iteration. */
      if (s.binary || sp >= str_end ||
          !mrb_re_char_interior_p(str, sp, str_end)) {
        int slot = match_only ? 0 : pool_alloc(&s);
        if (!match_only) memset(CAP(&s, slot), -1, sizeof(int) * ncap);
        advance_gen(&s);
        s.cut = FALSE;
        add_thread(&s, &curr, 0, slot, sp, s.gen);
        if (s.matched && curr.count == 0) break;
      }
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

    advance_gen(&s);
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
    /* A non-ASCII byte that stands alone is a byte, not the character its
       number spells: every byte of a byte-indexed subject, and a byte that
       starts no whole character in a decoded one. */
    mrb_bool curr_raw = (advance == 1 && ch >= 0x80);

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
          add_thread(&s, &next, th->pc + 1, cp, sp + 1, s.gen);
        }
        break;

      case RE_ANY:
        if (ch != '\n') {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_ANY_NL:
        {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_CLASS:
        if (class_match(&pat->classes[inst.a], curr_cp, curr_raw)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_NCLASS:
        if (!class_match(&pat->classes[inst.a], curr_cp, curr_raw)) {
          int cp = match_only ? 0 : pool_copy(&s, th->cap_slot);
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
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
 * Where the lookbehind at pc starts matching from: sp rewound by the byte
 * count in the opcode for a binary subject, and otherwise by the character
 * count in the RE_LB_WIDTH that follows it. The backward walk steps over
 * continuation bytes with mrb_re_char_interior_p(), which keeps it on the
 * boundaries the forward decode uses, broken input included. Returns NULL
 * when the text before sp runs out first.
 */
static const char*
lookbehind_start(const mrb_regexp_pattern *pat, const char *str,
                 const char *str_end, const char *sp, uint32_t pc,
                 mrb_bool binary)
{
  if (binary) {
    int lb_len = pat->code[pc].a;
    return (sp - str < lb_len) ? NULL : sp - lb_len;
  }
  int nchars = pat->code[pc + 1].a;
  while (nchars > 0) {
    if (sp <= str) return NULL;
    sp--;
    while (sp > str && mrb_re_char_interior_p(str, sp, str_end)) sp--;
    nchars--;
  }
  return sp;
}

/* What one bt_match() frame answers. The frame that reaches RE_MATCH answers
   BT_MATCH, and every frame under it hands that up unchanged. A frame whose
   alternatives are exhausted answers BT_FAIL; its alternatives are the choice
   points it pushed on the stack the search shares, which it backtracks into
   itself, and the branches it still recurses into, whose failure it reads
   here. The third answer is the cut of an atomic group: the text after an
   RE_ATOMIC_END has failed, and no alternative inside the group's body may be
   tried for it, so the frames between that end and the RE_ATOMIC that opened
   the group hand BT_CUT of the group's number up unchanged, undoing their
   captures as they go, and the frame that ran that RE_ATOMIC turns it into
   BT_FAIL. A lookaround runs its sub-pattern the same way, the text after it
   going on inside the sub-pattern's frames from the RE_LOOK_END (see there),
   so a cut can pass through one; the opener absorbs its own number like an
   RE_ATOMIC and hands any other up.
   The fourth answer, BT_LIMIT, is a frame giving up at one of the limits, and
   the fifth, BT_NOMEM, is the allocator having refused the state a frame
   needed. Both are the search's answer rather than the frame's: every frame
   hands them up unchanged, as it does a cut, and backtrack_exec() stops at
   them (see there). Neither says anything about the text, so no frame may
   read one as its branch having failed and answer with its other branch,
   which would be answering a smaller question with whatever the alternatives
   inside the limit produce: a shorter, a later or no match, told from the
   real answer by nothing. They stay apart from each other for the same
   reason they stay apart from a failure: what a limit asks of the build is
   that MRB_REGEXP_STACK_LIMIT be turned up, which is the worst thing it
   could do about an allocator that had nothing left to give.

   BT_OK is not one of bt_match()'s answers. It is what bt_push() and the
   other operations on the stacks answer when there is nothing to hand up,
   so that what they answer otherwise is the frame's answer as it stands. */
#define BT_FAIL 0
#define BT_MATCH 1
#define BT_LIMIT 2
#define BT_NOMEM 3
#define BT_OK 4
#define BT_CUT(cut) (-(int)(cut))

/* What taking a choice point does beyond resuming where it points. */
enum re_cp_kind {
  RE_CP_FORK,   /* nothing: a branch the search has not tried yet */
  RE_CP_ITER,   /* the branch begins an iteration of the loop its `group`
                   keys, whose record is written when the branch is taken
                   rather than when it was pushed (see bt_iter_begin()) */
  RE_CP_BARRIER /* not a branch at all: where the group its `group` names was
                 entered. Reaching it while backtracking is that group
                 failing, so it resumes nothing and the search goes on
                 popping; its end drops it and everything above it, which is
                 how an atomic group cuts (see RE_ATOMIC_END). */
};

/* A choice point: an alternative the search has not tried yet, as where the
   input stood (`sp`), which instruction takes the alternative (`pc`) and how
   tall the undo log was when it was pushed (`undo_top`). Backtracking pops
   one, takes back every write logged above its `undo_top` and goes on from
   its `sp` and `pc`. A fork costs one of these rather than the C frame it
   used to recurse into, so the C stack a search spends stops growing with
   the subject: the state that grows is this stack, on the heap.

   MRB_REGEXP_STACK_LIMIT is what bounds it (see there and bt_room()). */
typedef struct {
  const char *sp;
  uint32_t pc;
  uint32_t undo_top;
  uint32_t group;   /* what `kind` names, where it names something */
  uint8_t kind;
} re_cpoint;

/* An undo record: one write the search must be able to take back, as the slot
   written and what stood in it. The records are a stack of their own beside
   the choice points rather than a field in them, because what a cut does to
   each differs: the captures a positive lookaround or an atomic group wrote
   outlive it, so its cut drops the choice points above the barrier and leaves
   the undo log alone, while a negative lookaround, whose captures do not
   outlive it, unwinds both. One mixed stack could not truncate at all: it
   would have to walk the region above the barrier and keep the undo records
   while dropping the choice points. */
typedef struct {
  int *slot;
  int old;
} re_undo;

/* What one backtrack_exec() call shares between its bt_match() frames: the
   pattern, the subject, the capture slots being written, the step count, the
   iteration records and the two stacks the backtracking state stands on. A
   frame's own state is its position, its pc and its depth, plus the heights
   it found the stacks at. */
typedef struct {
  mrb_state *mrb;
  const mrb_regexp_pattern *pat;
  const char *str;
  const char *str_end;
  int *captures;
  int ncap;
  int steps;
  /* Per pc, the offset the frame that pc keys was entered at, or -1 while
     none is running; what a record means is what its pc is. For the edge of
     a repetition whose body can match empty it is where the running
     iteration began: such a repetition has to stop once an iteration ends
     where it began, or it would go round at the same position until a limit
     refused it and answer with whatever the alternatives left inside the
     limit produce. Onigmo stops it with a null check around the body; this
     array is that check's memory. The pc that keys a loop is its marked head
     for e* (the SPLIT/SPLITNG whose offset is the exit; the JMP closing the
     body reads the record) and its marked back edge for e+ (the
     SPLIT/SPLITNG at the end of the body, which both writes and reads it);
     see mark_empty_loops(). For the RE_LOOK_END of a lookaround it is where
     the lookaround was entered, which is where the text after it goes on
     from; see bt_look(). */
  int *entered_at;
  /* Per pc, the pass that wrote entered_at[pc]. A pass is one run of a
     lookaround's sub-pattern, told by the depth of the bt_look() frame
     running it, and 0 is the pattern outside every lookaround; `pass` is the
     one the frames now running are in. A record is read as the running
     iteration's only by the pass that wrote it: the text after a positive
     lookaround runs inside its sub-pattern's frames (see RE_LOOK_END), so a
     repetition around the lookaround re-enters the sub-pattern while the
     records of the loops inside it from the pass before are still live, and
     the first iteration of an e+, which reads its record without having
     written it, would take one of those for its own where the positions
     coincide: `(?=(b|)+)+` on "b" re-enters at 0 while the pass before left
     1 for `(b|)+`, and its first iteration, ending at 1, would stop there
     with "b" captured, where a fresh pass goes round once more and leaves
     "". For the RE_LOOK_END the record is the pass the lookaround was entered
     from, which the text after it runs in. */
  int *entered_in;
  int pass;
  mrb_bool binary;
  /* The choice points not yet tried and the writes not yet taken back, both
     grown lazily on the heap. A search starts with neither allocated and
     pays only for the kind of state it holds: a fork is a choice point, a
     capture or an iteration record is an undo record, and a pattern with
     none of them in it asks the allocator for nothing. */
  re_cpoint *cp;
  uint32_t cp_top;
  uint32_t cp_capa;
  re_undo *undo;
  uint32_t undo_top;
  uint32_t undo_capa;
} bt_state;

/* Whether the loop `key` keys is at the end of an iteration that began at
   sp: the record is this pass's and names sp. */
#define ITER_EMPTY(m, key, sp) \
  ((m)->entered_at[key] == (int)((sp) - (m)->str) && (m)->entered_in[key] == (m)->pass)

/* What is left of the bound on the C stack while the backtracking state moves
   onto the heap. The mechanisms that still recurse (a capture, the record of
   an iteration, an atomic group, a lookaround) each spend a frame per
   iteration the way a fork used to, and MRB_REGEXP_STACK_LIMIT no longer
   counts frames, so the frames keep a bound of their own until the last of
   them is gone. A frame that reaches it gives up as it did before and the
   search names the same limit, there being one knob and not two. */
#define RE_FRAME_LIMIT 1000

/* Whether the search may hold one more entry of backtracking state.
   MRB_REGEXP_STACK_LIMIT counts the two stacks together: a choice point and
   an undo record each stand for a branch or a write the search is still able
   to take back, and bounding their sum is what bounds the state one search
   holds. */
static mrb_bool
bt_room(const bt_state *m)
{
  return m->cp_top + m->undo_top < (uint32_t)MRB_REGEXP_STACK_LIMIT;
}

/* What a full stack grows to. Doubling is what makes a push amortised
   constant; the ceiling is what makes MRB_REGEXP_STACK_LIMIT a bound on the
   memory a search asks for and not only on the entries it holds at once,
   since a stack keeps its capacity for the rest of the search and the two of
   them reach their high-water marks at different times. A stack is grown only
   where bt_room() has just passed, so its height is below the limit and the
   ceiling always leaves room for the entry being pushed. */
static uint32_t
bt_grow_capa(uint32_t capa)
{
  capa = capa ? capa * 2 : 16;
  return capa < (uint32_t)MRB_REGEXP_STACK_LIMIT ? capa : (uint32_t)MRB_REGEXP_STACK_LIMIT;
}

/* Push a choice point. BT_OK is the push having happened, and anything else
   is the search stopping, to be handed up as the frame's answer: BT_LIMIT
   where the stack limit refuses the entry, BT_NOMEM where the allocator
   refuses the memory for it. Growing with mrb_realloc_simple() rather than
   mrb_realloc() is what lets a refusal be an answer at all, a raising
   allocator longjmping past the mrb_free() in backtrack_exec(). */
static int
bt_push(bt_state *m, const char *sp, uint32_t pc, uint8_t kind, uint32_t group)
{
  if (!bt_room(m)) return BT_LIMIT;
  if (m->cp_top == m->cp_capa) {
    uint32_t capa = bt_grow_capa(m->cp_capa);
    re_cpoint *p = (re_cpoint*)mrb_realloc_simple(m->mrb, m->cp, sizeof(re_cpoint) * capa);
    if (!p) return BT_NOMEM;
    m->cp = p;
    m->cp_capa = capa;
  }
  re_cpoint *c = &m->cp[m->cp_top++];
  c->sp = sp;
  c->pc = pc;
  c->undo_top = m->undo_top;
  c->group = group;
  c->kind = kind;
  return BT_OK;
}

/* Write `val` into `slot`, logging what stood there so that backtracking
   past this point puts it back. The answer is bt_push()'s, and means what it
   means there: BT_OK is the write having happened, and BT_LIMIT or BT_NOMEM
   is the search stopping, for the same two reasons and told apart for the
   same one. */
static int
bt_log(bt_state *m, int *slot, int val)
{
  if (!bt_room(m)) return BT_LIMIT;
  if (m->undo_top == m->undo_capa) {
    uint32_t capa = bt_grow_capa(m->undo_capa);
    re_undo *p = (re_undo*)mrb_realloc_simple(m->mrb, m->undo, sizeof(re_undo) * capa);
    if (!p) return BT_NOMEM;
    m->undo = p;
    m->undo_capa = capa;
  }
  re_undo *u = &m->undo[m->undo_top++];
  u->slot = slot;
  u->old = *slot;
  *slot = val;
  return BT_OK;
}

/* Take back every write logged above `top`. */
static void
bt_undo_to(bt_state *m, uint32_t top)
{
  while (m->undo_top > top) {
    re_undo *u = &m->undo[--m->undo_top];
    *u->slot = u->old;
  }
}

/* Drop the backtracking state above the heights a frame found: the choice
   points it pushed and the writes it logged, none of which its caller may
   backtrack into. */
static void
bt_truncate(bt_state *m, uint32_t cp_top, uint32_t undo_top)
{
  m->cp_top = cp_top;
  bt_undo_to(m, undo_top);
}

/* Where the group `group` was entered, as an index into the choice point
   stack, or FALSE when it was entered in a frame below `floor`: a group
   whose body holds a lookaround, whose end runs inside the frames the
   lookaround makes, and which the cut still reaches instead (see
   bt_cut_absorb()). Barriers nest, so the walk down from the top is over
   what the group's end is about to drop in any case. */
static mrb_bool
bt_barrier_find(const bt_state *m, uint32_t group, uint32_t floor, uint32_t *idx)
{
  uint32_t i = m->cp_top;
  while (i > floor) {
    i--;
    if (m->cp[i].kind == RE_CP_BARRIER && m->cp[i].group == group) {
      *idx = i;
      return TRUE;
    }
  }
  return FALSE;
}

/* Whether a nested call's answer is the cut of a group this frame opened,
   which is that group failing here: everything the body left goes, the writes
   it logged with it, which is what popping its barrier does. Any other answer
   is not this frame's to read. */
static mrb_bool
bt_cut_absorb(bt_state *m, int r, uint32_t floor)
{
  uint32_t idx;
  if (r >= 0 || !bt_barrier_find(m, (uint32_t)(-r), floor, &idx)) return FALSE;
  m->cp_top = idx;
  bt_undo_to(m, m->cp[idx].undo_top);
  return TRUE;
}

static int bt_match(bt_state *m, const char *sp, uint32_t pc, int depth);

/* Record that an iteration of the loop `key` keys begins at sp, so that the
   edge closing the body can tell an iteration that matched empty. The two
   records go on the undo log, so that backtracking out of an iteration puts
   back the record of the one it lands in; the branch that begins an
   iteration is written this way rather than in place when it is taken, so
   that there is one place that writes them. */
static int
bt_iter_begin(bt_state *m, uint32_t key, const char *sp)
{
  int r = bt_log(m, &m->entered_at[key], (int)(sp - m->str));
  return r != BT_OK ? r : bt_log(m, &m->entered_in[key], m->pass);
}

/* Run the sub-pattern of the lookaround whose opener is at pc: it begins at
   `body` and matches from `from`, which is sp for a lookahead and the
   rewound start for a lookbehind. The record of sp, and of the pass the
   lookaround is entered from, lasts as long as the frame, as an iteration's
   does in bt_iter_begin(); the RE_LOOK_END closing the sub-pattern reads it (see
   there). The sub-pattern runs as a pass of its own, this frame's depth,
   which no pass still live has, so the records of the loops inside it that
   another run of the same sub-pattern may have left live are not taken for
   this run's (see bt_state).

   The answer is the sub-pattern's, and the cut of this lookaround's number
   is what the sub-pattern matching comes back as: for a negative lookaround
   from the RE_LOOK_END itself, and the answer is BT_MATCH, with every
   capture the sub-pattern wrote undone on the way up; for a positive one
   from the text after the lookaround failing, which the RE_LOOK_END ran
   inside the sub-pattern's frames, so the sub-pattern matched once and that
   was its only match: BT_FAIL, the captures undone the same way. BT_MATCH
   from a positive one is the whole pattern having matched through that
   text. Everything else, a failure, a limit or another group's cut, goes up
   as it is. */
static int
bt_look(bt_state *m, const char *sp, const char *from, uint32_t pc,
        uint32_t body, int depth)
{
  uint32_t end = m->pat->code[pc].offset - 1;
  re_inst end_inst = m->pat->code[end];
  int old_at = m->entered_at[end], old_in = m->entered_in[end];
  int old_pass = m->pass;
  m->entered_at[end] = (int)(sp - m->str);
  m->entered_in[end] = old_pass;
  m->pass = depth;
  int r = bt_match(m, from, body, depth);
  m->pass = old_pass;
  m->entered_at[end] = old_at;
  m->entered_in[end] = old_in;
  if (r != BT_CUT(end_inst.offset)) return r;
  return end_inst.a ? BT_MATCH : BT_FAIL;
}

/*
 * Backtracking engine for patterns with backreferences.
 *
 * A fork pushes a choice point and goes on with its first branch; a failure
 * pops one and goes on with the alternative it holds, taking back the writes
 * logged since it was pushed. The frame therefore loops where it used to
 * recurse, and what it may pop is what it pushed: the heights the stacks
 * stood at on entry are the floor, since below them is the state of the
 * frame that called this one, whose iteration records that frame restores
 * itself. Every answer but BT_MATCH leaves the stacks as the frame found
 * them.
 *
 * Step-limited to prevent ReDoS.
 */
static int
bt_match(bt_state *m, const char *sp, uint32_t pc, int depth)
{
  const mrb_regexp_pattern *pat = m->pat;
  const char *str = m->str;
  const char *str_end = m->str_end;
  int *captures = m->captures;
  int ncap = m->ncap;
  mrb_bool binary = m->binary;
  const uint32_t cp_floor = m->cp_top;
  const uint32_t undo_floor = m->undo_top;
  int r;
  /* Outside the loop so that no `goto fail` crosses its initialization: a
     C++ build refuses a jump that does. */
  re_inst inst;

  if (depth > RE_FRAME_LIMIT) return BT_LIMIT;
  for (;;) {
    if (pc >= pat->code_len) goto fail;
    if (++m->steps > MRB_REGEXP_STEP_LIMIT) { r = BT_LIMIT; goto done; }

    inst = pat->code[pc];
    switch (inst.op) {
    case RE_CHAR:
      if (sp >= str_end || (uint8_t)*sp != inst.a) goto fail;
      sp++; pc++;
      break;

    case RE_ANY:
      if (sp >= str_end || *sp == '\n') goto fail;
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_ANY_NL:
      if (sp >= str_end) goto fail;
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_CLASS:
      if (sp >= str_end) goto fail;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        mrb_bool raw = (dlen == 1 && (uint8_t)*sp >= 0x80);
        if (!class_match(&pat->classes[inst.a], cp_, raw)) goto fail;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_NCLASS:
      if (sp >= str_end) goto fail;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        mrb_bool raw = (dlen == 1 && (uint8_t)*sp >= 0x80);
        if (class_match(&pat->classes[inst.a], cp_, raw)) goto fail;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_MATCH:
      return BT_MATCH;

    case RE_JMP:
      /* A backward jump closes e* and returns to its head. When the head is
         marked, the body can match empty and entered_at[head] holds where
         the iteration that just ended began (see bt_iter_begin()): an
         iteration that ended where it began matched empty, and the
         repetition stops here, taking the head's exit and keeping what the
         iteration captured, as Onigmo's null check does. */
      if (inst.a && ITER_EMPTY(m, inst.offset, sp)) {
        pc = pat->code[inst.offset].offset;
        break;
      }
      pc = inst.offset;
      break;

    case RE_SPLIT:
      /* Greedy fork: pc+1 first, then the jump target. A marked one is an
         edge of a repetition whose body can match empty. Forward, it heads
         e*, and pc+1 begins an iteration. Backward, it closes e+?, and its
         target begins the next iteration, unless the one that just ended was
         empty: then there is only the exit, as at a marked RE_JMP. */
      if (inst.a) {
        if (inst.offset > pc) {
          if ((r = bt_push(m, sp, inst.offset, RE_CP_FORK, 0)) != BT_OK ||
              (r = bt_iter_begin(m, pc, sp)) != BT_OK) goto done;
          pc++;
          break;
        }
        if (ITER_EMPTY(m, pc, sp)) { pc++; break; }
        if ((r = bt_push(m, sp, inst.offset, RE_CP_ITER, pc)) != BT_OK) goto done;
        pc++;
        break;
      }
      if ((r = bt_push(m, sp, inst.offset, RE_CP_FORK, 0)) != BT_OK) goto done;
      pc++;
      break;

    case RE_SPLITNG:
      /* Non-greedy fork: the jump target first, then pc+1. Marked, forward
         it heads e*? and backward it closes e+; the iteration-starting branch
         is the other one from RE_SPLIT's, and the empty-iteration stop is the
         same. */
      if (inst.a) {
        if (inst.offset > pc) {
          if ((r = bt_push(m, sp, pc + 1, RE_CP_ITER, pc)) != BT_OK) goto done;
          pc = inst.offset;
          break;
        }
        if (ITER_EMPTY(m, pc, sp)) { pc++; break; }
        if ((r = bt_push(m, sp, pc + 1, RE_CP_FORK, 0)) != BT_OK ||
            (r = bt_iter_begin(m, pc, sp)) != BT_OK) goto done;
        pc = inst.offset;
        break;
      }
      if ((r = bt_push(m, sp, pc + 1, RE_CP_FORK, 0)) != BT_OK) goto done;
      pc = inst.offset;
      break;

    case RE_SAVE:
      {
        int slot = inst.offset;
        /* End of group 0: the whole match may not close inside a character
           (see the Pike VM case). Failing here backtracks into the other
           branches, so a longer one can still match. */
        if (slot == 1 && !binary && sp < str_end &&
            mrb_re_char_interior_p(str, sp, str_end)) {
          goto fail;
        }
        if (slot >= ncap) goto fail;
        /* The write is logged rather than recursed over: backtracking past
           it puts the slot back, which is undone for a cut as for a failure
           (the group a cut fails may be the one this slot was written
           inside), and a match keeps it, the log unwinding for neither. */
        if ((r = bt_log(m, &captures[slot], (int)(sp - str))) != BT_OK) goto done;
        /* An even slot opens its group, and the pair it heads is a span
           only while the group is closed: clear the end slot with the
           start, so that RE_BACKREF reads a group a repetition has just
           re-entered the way it reads one never entered, instead of
           pairing this iteration's start with the end of the one before:
           an empty span where the two coincide, and a negative one where
           the start is past it, which no closed group holds. CRuby
           reads an open group as unmatched the same way (Onigmo's
           STACK_PUSH_MEM_START invalidates the end with the start). The
           end slot exists whenever the start does, ncap being even. The
           clear is logged too, so backtracking puts back the end the
           iteration before it left. */
        if ((slot & 1) == 0 && (r = bt_log(m, &captures[slot + 1], -1)) != BT_OK) goto done;
        pc++;
      }
      break;

    case RE_BOL:
      /* ^ always matches at a line start (see the Pike VM case); /m only
         affects `.`. \A is RE_BOT. A trailing \n opens no final line. */
      if (sp != str && (sp == str_end || sp[-1] != '\n')) goto fail;
      pc++;
      break;

    case RE_EOL:
      /* $ always matches at a line end. */
      if (sp != str_end && *sp != '\n') goto fail;
      pc++;
      break;

    case RE_BOT:
      if (sp != str) goto fail;
      pc++;
      break;

    case RE_EOT:
      if (sp != str_end) goto fail;
      pc++;
      break;

    case RE_EOTNL:
      if (sp != str_end && !(sp + 1 == str_end && *sp == '\n')) goto fail;
      pc++;
      break;

    case RE_WBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before == after) goto fail;
      }
      pc++;
      break;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before != after) goto fail;
      }
      pc++;
      break;

    case RE_BACKREF:
      {
        int group = inst.a;
        if (group * 2 + 1 >= ncap) goto fail;
        int gs = captures[group * 2];
        int ge = captures[group * 2 + 1];
        if (gs < 0 || ge < 0) goto fail;
        int blen = ge - gs;
        if (inst.offset) {
          /* A folded comparison can consume a different number of bytes than
             the captured text holds, so the span is measured, not assumed. */
          int used = memcmp_ci(sp, str_end, str + gs, str + ge, binary);
          if (used < 0) goto fail;
          sp += used;
        }
        else {
          if (sp + blen > str_end) goto fail;
          if (memcmp(sp, str + gs, blen) != 0) goto fail;
          sp += blen;
        }
        pc++;
      }
      break;

    case RE_LOOKAHEAD:
      /* A positive lookaround never goes on in this frame: its RE_LOOK_END
         has run the text after it, so the sub-pattern's answer is the
         frame's, whichever it is. */
      {
        int rr = bt_look(m, sp, sp, pc, pc + 1, depth + 1);
        if (rr == BT_FAIL || bt_cut_absorb(m, rr, cp_floor)) goto fail;
        r = rr;
        goto done;
      }

    case RE_NEG_LOOKAHEAD:
      {
        /* The sub-pattern matching is the assertion failing; the sub-pattern
           running out of alternatives is the assertion holding, and the text
           after it goes on here. A limit goes up as it is. */
        int rr = bt_look(m, sp, sp, pc, pc + 1, depth + 1);
        if (rr == BT_MATCH || bt_cut_absorb(m, rr, cp_floor)) goto fail;
        if (rr != BT_FAIL) { r = rr; goto done; }
        pc = inst.offset;
      }
      break;

    case RE_LOOKBEHIND:
      {
        const char *back = lookbehind_start(pat, str, str_end, sp, pc, binary);
        if (!back) goto fail;  /* not enough text before */
        int rr = bt_look(m, sp, back, pc, pc + 2, depth + 1);
        if (rr == BT_FAIL || bt_cut_absorb(m, rr, cp_floor)) goto fail;
        r = rr;
        goto done;
      }

    case RE_NEG_LOOKBEHIND:
      {
        const char *back = lookbehind_start(pat, str, str_end, sp, pc, binary);
        if (back) {
          int rr = bt_look(m, sp, back, pc, pc + 2, depth + 1);
          if (rr == BT_MATCH || bt_cut_absorb(m, rr, cp_floor)) goto fail;
          if (rr != BT_FAIL) { r = rr; goto done; }
        }
        /* if not enough text before, negative lookbehind succeeds */
        pc = inst.offset;
      }
      break;

    case RE_LOOK_END:
      {
        /* The sub-pattern has matched. For a negative lookaround that is
           the whole of its answer, and it goes up as a cut so that the
           frames of the sub-pattern undo their captures and try no other
           branch on the way; bt_look() reads it back as the match it is.
           For a positive one the text after the lookaround goes on from
           where the lookaround was entered, inside the sub-pattern's frames
           as the text after an atomic group does (RE_ATOMIC_END): a failure
           there is a cut, undone for and not backtracked into, since the
           sub-pattern matching once is its only match. A limit goes up as
           it is. The text after runs in the pass the lookaround was entered
           from, which is what its loops' records are keyed by; the pass of
           this sub-pattern comes back for the frames above on the way up. */
        if (inst.a) { r = BT_CUT(inst.offset); goto done; }
        int pass = m->pass;
        m->pass = m->entered_in[pc];
        int rr = bt_match(m, str + m->entered_at[pc], pc + 1, depth + 1);
        m->pass = pass;
        if (bt_cut_absorb(m, rr, cp_floor)) goto fail;
        r = (rr == BT_FAIL) ? BT_CUT(inst.offset) : rr;
        goto done;
      }

    case RE_ATOMIC:
      /* The group is entered: a barrier stands where it began, so that a
         failure inside the body that reaches it is the group failing, and
         the body goes on in this frame. */
      if ((r = bt_push(m, sp, pc + 1, RE_CP_BARRIER, inst.offset)) != BT_OK) goto done;
      pc++;
      break;

    case RE_ATOMIC_END:
      {
        /* The body has matched once, and that is the only way it matches:
           the alternatives it left are dropped, barrier and all, so that a
           failure after the group is not answered from inside it. What the
           body captured stays; the undo log is left as it is, and a
           failure that goes back past where the group began takes it back
           along with everything else logged since. */
        uint32_t idx;
        if (bt_barrier_find(m, inst.offset, cp_floor, &idx)) {
          m->cp_top = idx;
          pc++;
          break;
        }
        /* The group was entered in a frame below this one, whose choice
           points this one may not touch. The text after it runs inside this
           frame instead, as it did before the barrier: a failure there is
           the cut, which reaches the barrier through the frames between
           (see bt_cut_absorb()), and a limit is not the text failing and
           goes up as it is. */
        int rr = bt_match(m, sp, pc + 1, depth + 1);
        r = (rr == BT_FAIL) ? BT_CUT(inst.offset) : rr;
        goto done;
      }

    default:
      goto fail;
    }
    continue;

  fail:
    /* This branch is spent. The next alternative is the choice point on top,
       with every write since it was pushed taken back; below the floor is the
       caller's state and the caller's alternatives, so there this frame is
       spent too. */
    for (;;) {
      if (m->cp_top == cp_floor) { r = BT_FAIL; goto done; }
      re_cpoint c = m->cp[--m->cp_top];
      bt_undo_to(m, c.undo_top);
      /* A barrier is where a group was entered, not a branch: reaching it
         is that group failing, and what is left to try is below it. */
      if (c.kind == RE_CP_BARRIER) continue;
      sp = c.sp;
      pc = c.pc;
      if (c.kind == RE_CP_ITER && (r = bt_iter_begin(m, c.group, sp)) != BT_OK) {
        goto done;
      }
      break;
    }
  }

done:
  if (r != BT_MATCH) bt_truncate(m, cp_floor, undo_floor);
  return r;
}

static int
backtrack_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
               const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
               int *captures, int captures_size, mrb_bool binary)
{
  const char *start_cap = str + start_limit;
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  /* One block: the capture slots, then an entry record per pc and the pass
     that wrote it. The arrays are filled once here and put back to what they
     hold by the undo log unwinding between start positions (see there): an
     iteration's record is written on the log now, which a match does not
     unwind, so a record left by the attempt before would otherwise be read
     as this attempt's and stop a repetition that had not gone round yet. */
  int *caps = (int*)mrb_malloc(mrb, sizeof(int) * (ncap + 2 * pat->code_len));
  int ret = 0;
  bt_state m;
  m.mrb = mrb;
  m.pat = pat;
  m.str = str;
  m.str_end = str_end;
  m.captures = caps;
  m.ncap = ncap;
  m.entered_at = caps + ncap;
  m.entered_in = m.entered_at + pat->code_len;
  m.pass = 0;
  m.binary = binary;
  m.cp = NULL;
  m.cp_top = m.cp_capa = 0;
  m.undo = NULL;
  m.undo_top = m.undo_capa = 0;
  memset(m.entered_at, -1, sizeof(int) * pat->code_len);
  memset(m.entered_in, 0, sizeof(int) * pat->code_len);

  for (const char *sp = str + start; sp <= str_end && sp <= start_cap; sp++) {
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
    if (sp > start_cap) break;
    if (!binary && sp < str_end && mrb_re_char_interior_p(str, sp, str_end)) {
      continue;
    }
    memset(caps, -1, sizeof(int) * ncap);
    m.steps = 0;
    /* The state of the attempt before, which failed and left nothing to
       resume, is not this attempt's: the stacks start empty, and the writes
       still logged are taken back so that the records the arrays hold are
       what a fresh search finds. */
    m.cp_top = 0;
    bt_undo_to(&m, 0);

    int r = bt_match(&m, sp, 0, 0);
    if (r == BT_MATCH) {
      if (captures) {
        int copy = ncap < captures_size ? ncap : captures_size;
        memcpy(captures, caps, sizeof(int) * copy);
      }
      ret = ncap > 0 ? ncap : 1;
      break;
    }
    if (r == BT_NOMEM) {
      /* The allocator had nothing left for the state this search needed. The
         search ends here as it does at a limit, and it is told apart from
         one all the way out to the caller, since what it asks for is not a
         knob turned up: raising MRB_REGEXP_STACK_LIMIT in answer to this
         would only let the next search ask for more. */
      ret = RE_NOMEM;
      break;
    }
    if (r == BT_LIMIT) {
      /* The search ends here, not this start position's attempt: the
         positions after it answer where the first match is only once this
         one has none, which is what the limit left unanswered. Which limit
         is read off the step count: the state checks run before a frame
         counts its step, so the count is over the step limit exactly when
         the step check gave up. */
      ret = m.steps > MRB_REGEXP_STEP_LIMIT ? RE_OVER_STEP_LIMIT : RE_OVER_STACK_LIMIT;
      break;
    }
  }
  mrb_free(mrb, m.undo);
  mrb_free(mrb, m.cp);
  mrb_free(mrb, caps);
  return ret;
}

/* Fast path for pure literal patterns: use memchr+memcmp, no NFA needed */
static int
literal_exec(const mrb_regexp_pattern *pat,
             const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
             int *captures, int captures_size, mrb_bool binary)
{
  const char *start_cap = str + start_limit;
  const char *sp = str + start;
  const char *str_end = str + len;
  int plen = pat->prefix_len;

  while (sp + plen <= str_end && sp <= start_cap) {
    const char *found = (const char*)memchr(sp, pat->prefix[0], str_end - sp);
    if (!found || found + plen > str_end || found > start_cap) return 0;
    if (!binary && mrb_re_char_interior_p(str, found, str_end)) {
      sp = found + 1;  /* not a char boundary, same rule as the other engines */
      continue;
    }
    if (plen == 1 || memcmp(found + 1, pat->prefix + 1, plen - 1) == 0) {
      if (!binary && found + plen < str_end &&
          mrb_re_char_interior_p(str, found + plen, str_end)) {
        sp = found + 1;  /* ends inside a character, same rule as the end of
                            group 0 in the other engines */
        continue;
      }
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

/* The search the three engines make, with the last position a match may
   start at named.  The bound is on where a match may begin and not on how
   far the subject is read: a match that begins at `start_limit` runs to
   wherever it ends, which is why this is a separate argument rather than a
   shorter `len`.  Shortening the subject would answer a different question
   -- `$` and `\z` would assert at the cut, and a match reaching past it
   would be lost. */
static int
exec_range(mrb_state *mrb, const mrb_regexp_pattern *pat,
           const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
           int *captures, int captures_size, mrb_bool binary)
{
  if (pat->is_literal) {
    return literal_exec(pat, str, len, start, start_limit, captures, captures_size, binary);
  }
  if (pat->has_backref || pat->needs_backtrack) {
    return backtrack_exec(mrb, pat, str, len, start, start_limit, captures, captures_size, binary);
  }
  return pike_vm(mrb, pat, str, len, start, start_limit, captures, captures_size, binary);
}

/* Public entry point */
int
mrb_re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size, mrb_bool binary)
{
  /* The end of the subject is the last position anything can start at, so
     the forward search is the unbounded case of the above. */
  return exec_range(mrb, pat, str, len, start, len, captures, captures_size, binary);
}

/* How far the backward probe below may read before it stops being the cheap
   question. A search bounded to a window still reads from where the window
   starts to the end of the subject, so the bound is on that span and not on
   the window's own width: a narrow window at the end of a long subject is
   cheap, the same window asked about a position far from the end is the
   whole search over again. */
#define RE_RSEARCH_PROBE_SPAN 256

/* The last match that starts at or before `limit`, which is what `rindex`,
   `byterindex` and `rpartition` ask for.

   A forward walk answers this by stepping every match from the front and
   keeping the last, which costs a search per match: on a subject a pattern
   matches everywhere that is a search per position, and where one search is
   itself linear in the subject -- a greedy `/a+b?/` and the like -- the walk
   is quadratic in it.

   The last match is usually near the end, so ask about the end first: widen
   a window there until a match starts inside it. A window that catches one
   costs the window rather than the subject, and the walk that follows has
   only the window to cross. Widening stops at the span above rather than at
   the front, so a subject with no match near the end falls through to the
   single forward search this cost before, instead of paying for the
   widening as well. */
int
mrb_re_rexec(mrb_state *mrb, const mrb_regexp_pattern *pat,
             const char *str, mrb_int len, mrb_int limit,
             int *captures, int captures_size, mrb_bool binary)
{
  if (limit > len) limit = len;
  if (limit < 0) return 0;

  int last[RE_MAX_CAPTURES * 2];
  int last_n = 0;

  for (mrb_int k = 1; ; k *= 2) {
    mrb_int lo = limit - k + 1;
    if (lo < 0) lo = 0;
    if (len - lo > RE_RSEARCH_PROBE_SPAN) break;
    memset(captures, -1, sizeof(int) * captures_size);
    last_n = exec_range(mrb, pat, str, len, lo, limit, captures, captures_size, binary);
    /* A match or an execution error ends the probe. A limit, or an
       allocation the engine was refused, is the answer to the whole question
       and not to this window's: a window that gave up is not one with no
       match in it, and widening would only ask the same search again a size
       up. */
    if (last_n) break;
    /* The window had grown to the whole range, so there is no match to find
       and the search below would only ask again. */
    if (lo == 0) return 0;
  }

  if (last_n == 0) {
    memset(captures, -1, sizeof(int) * captures_size);
    last_n = exec_range(mrb, pat, str, len, 0, limit, captures, captures_size, binary);
    if (last_n == 0) return 0;
  }
  if (last_n < 0) return last_n;

  /* Whichever range answered gave its leftmost match; the last one is found
     by walking forward from it. Each step resumes one byte past the match
     start and not at the match end, which is what keeps overlapping matches
     in view: `"aaa"` against `/aa/` answers 1, where resuming at the end
     would answer 0. A byte inside a character is not a position a match can
     start at, and the engine steps over one rather than seed an attempt
     there, so `+ 1` reaches the next character by itself. */
  memcpy(last, captures, sizeof(int) * captures_size);
  mrb_int pos = captures[0] + 1;
  while (pos <= limit) {
    memset(captures, -1, sizeof(int) * captures_size);
    int n = exec_range(mrb, pat, str, len, pos, limit, captures, captures_size, binary);
    if (n == 0) break;
    if (n < 0) return n;
    last_n = n;
    memcpy(last, captures, sizeof(int) * captures_size);
    pos = captures[0] + 1;
  }
  memcpy(captures, last, sizeof(int) * captures_size);
  return last_n;
}
