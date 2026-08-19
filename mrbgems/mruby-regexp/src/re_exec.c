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
   range list, then to the utf8_any catch-all (used by negated shorthand like
   \D).

   `raw` says the input is a byte rather than a character: a byte-indexed
   subject, or one whose byte at this position starts no whole character. It
   picks which half of the range list to read, since a byte member and a
   codepoint member of the same number are different members and arrive here as
   the same number. utf8_any is the answer for either, being about the byte
   being non-ASCII at all. */
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
   alternatives are exhausted answers BT_FAIL, and its caller tries the next
   alternative of its own. The third answer is the cut of an atomic group:
   the text after an RE_ATOMIC_END has failed, and no alternative inside the
   group's body may be tried for it, so the frames between that end and the
   RE_ATOMIC that opened the group hand BT_CUT of the group's number up
   unchanged, undoing their captures as they go, and the frame that ran that
   RE_ATOMIC turns it into BT_FAIL. A cut never reaches a lookaround from
   inside its sub-pattern: the RE_ATOMIC that absorbs it is in there too.
   The fourth answer, BT_LIMIT, is a frame giving up at the recursion or step
   limit. A frame that gets it hands it up; a SPLIT takes it as that branch
   failing and answers with its other branch, as it would with a failure.
   What no frame does is turn it into a cut or into a lookaround's answer,
   since a limit says nothing about the text: the frame giving up may be
   inside an atomic group's body, where a cut would keep the group's exit
   from being taken, or inside a negative lookaround, where reading the limit
   as "no match" would make the assertion hold. */
#define BT_FAIL 0
#define BT_MATCH 1
#define BT_LIMIT 2
#define BT_CUT(cut) (-(int)(cut))

/* What one backtrack_exec() call shares between its bt_match() frames: the
   pattern, the subject, the capture slots being written, the step count and
   the iteration records. A frame's own state is its position, its pc and its
   depth. */
typedef struct {
  const mrb_regexp_pattern *pat;
  const char *str;
  const char *str_end;
  int *captures;
  int ncap;
  int steps;
  /* Per pc, the offset the running iteration of the loop that pc keys began
     at, or -1 while none is running. A repetition whose body can match empty
     has to stop once an iteration ends where it began, or it would go round
     at the same position until a limit refused it and answer with whatever
     the alternatives left inside the limit produce. Onigmo stops it with a
     null check around the body; this array is that check's memory. The pc
     that keys a loop is its marked head for e* (the SPLIT/SPLITNG whose
     offset is the exit; the JMP closing the body reads the record) and its
     marked back edge for e+ (the SPLIT/SPLITNG at the end of the body, which
     both writes and reads it); see mark_empty_loops(). */
  int *iter_at;
  mrb_bool binary;
} bt_state;

static int bt_match(bt_state *m, const char *sp, uint32_t pc, int depth);

/* Run the frame at pc as the start of an iteration of the loop `key` keys,
   recording where it begins so that the edge closing the body can tell an
   empty iteration. The record lasts exactly as long as the frame: the frame
   that ran the edge into the body is the one that undoes it, so backtracking
   out of an iteration finds the record of the one it lands in, and the
   branch that begins an iteration is run this way rather than in place even
   when it is the frame's last, so that there is one place to undo it. */
static int
bt_iter(bt_state *m, const char *sp, uint32_t pc, uint32_t key, int depth)
{
  int old = m->iter_at[key];
  m->iter_at[key] = (int)(sp - m->str);
  int r = bt_match(m, sp, pc, depth);
  m->iter_at[key] = old;
  return r;
}

/*
 * Backtracking engine for patterns with backreferences.
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

  if (depth > MRB_REGEXP_RECURSION_LIMIT) return BT_LIMIT;
  while (pc < pat->code_len) {
    if (++m->steps > MRB_REGEXP_STEP_LIMIT) return BT_LIMIT;

    re_inst inst = pat->code[pc];
    switch (inst.op) {
    case RE_CHAR:
      if (sp >= str_end || (uint8_t)*sp != inst.a) return BT_FAIL;
      sp++; pc++;
      break;

    case RE_ANY:
      if (sp >= str_end || *sp == '\n') return BT_FAIL;
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_ANY_NL:
      if (sp >= str_end) return BT_FAIL;
      sp += mrb_re_charlen(sp, str_end, binary); pc++;
      break;

    case RE_CLASS:
      if (sp >= str_end) return BT_FAIL;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        mrb_bool raw = (dlen == 1 && (uint8_t)*sp >= 0x80);
        if (!class_match(&pat->classes[inst.a], cp_, raw)) return BT_FAIL;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_NCLASS:
      if (sp >= str_end) return BT_FAIL;
      {
        int dlen = 0;
        uint32_t cp_ = mrb_re_decode_char(sp, str_end, &dlen, binary);
        mrb_bool raw = (dlen == 1 && (uint8_t)*sp >= 0x80);
        if (class_match(&pat->classes[inst.a], cp_, raw)) return BT_FAIL;
        sp += mrb_re_charlen(sp, str_end, binary);
      }
      pc++;
      break;

    case RE_MATCH:
      return BT_MATCH;

    case RE_JMP:
      /* A backward jump closes e* and returns to its head. When the head is
         marked, the body can match empty and iter_at[head] holds where the
         iteration that just ended began (see bt_iter()): an iteration that
         ended where it began matched empty, and the repetition stops here,
         taking the head's exit and keeping what the iteration captured, as
         Onigmo's null check does. */
      if (inst.a && m->iter_at[inst.offset] == (int)(sp - str)) {
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
          int r = bt_iter(m, sp, pc + 1, pc, depth + 1);
          if (r != BT_FAIL && r != BT_LIMIT) return r;
          pc = inst.offset;
          break;
        }
        if (m->iter_at[pc] == (int)(sp - str)) { pc++; break; }
        int r = bt_match(m, sp, pc + 1, depth + 1);
        if (r != BT_FAIL && r != BT_LIMIT) return r;
        return bt_iter(m, sp, inst.offset, pc, depth + 1);
      }
      {
        int r = bt_match(m, sp, pc + 1, depth + 1);
        if (r != BT_FAIL && r != BT_LIMIT) return r;
      }
      pc = inst.offset;
      break;

    case RE_SPLITNG:
      /* Non-greedy fork: the jump target first, then pc+1. Marked, forward
         it heads e*? and backward it closes e+; the iteration-starting branch
         is the other one from RE_SPLIT's, and the empty-iteration stop is the
         same. */
      if (inst.a) {
        if (inst.offset > pc) {
          int r = bt_match(m, sp, inst.offset, depth + 1);
          if (r != BT_FAIL && r != BT_LIMIT) return r;
          return bt_iter(m, sp, pc + 1, pc, depth + 1);
        }
        if (m->iter_at[pc] == (int)(sp - str)) { pc++; break; }
        int r = bt_iter(m, sp, inst.offset, pc, depth + 1);
        if (r != BT_FAIL && r != BT_LIMIT) return r;
        pc++;
        break;
      }
      {
        int r = bt_match(m, sp, inst.offset, depth + 1);
        if (r != BT_FAIL && r != BT_LIMIT) return r;
      }
      pc++;
      break;

    case RE_SAVE:
      {
        int slot = inst.offset;
        /* End of group 0: the whole match may not close inside a character
           (see the Pike VM case). Failing here backtracks into the other
           branches, so a longer one can still match. */
        if (slot == 1 && !binary && sp < str_end &&
            mrb_re_char_interior_p(str, sp, str_end)) {
          return BT_FAIL;
        }
        if (slot < ncap) {
          int old = captures[slot];
          captures[slot] = (int)(sp - str);
          int r = bt_match(m, sp, pc + 1, depth + 1);
          if (r == BT_MATCH) return r;
          /* undone for a cut as for a failure: the group the cut fails may
             be the one this slot was written inside */
          captures[slot] = old;
          return r;
        }
        return BT_FAIL;
      }

    case RE_BOL:
      /* ^ always matches at a line start (see the Pike VM case); /m only
         affects `.`. \A is RE_BOT. A trailing \n opens no final line. */
      if (sp != str && (sp == str_end || sp[-1] != '\n')) return BT_FAIL;
      pc++;
      break;

    case RE_EOL:
      /* $ always matches at a line end. */
      if (sp != str_end && *sp != '\n') return BT_FAIL;
      pc++;
      break;

    case RE_BOT:
      if (sp != str) return BT_FAIL;
      pc++;
      break;

    case RE_EOT:
      if (sp != str_end) return BT_FAIL;
      pc++;
      break;

    case RE_EOTNL:
      if (sp != str_end && !(sp + 1 == str_end && *sp == '\n')) return FALSE;
      pc++;
      break;

    case RE_WBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before == after) return BT_FAIL;
      }
      pc++;
      break;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && mrb_re_is_word_char((uint8_t)*sp);
        if (before != after) return BT_FAIL;
      }
      pc++;
      break;

    case RE_BACKREF:
      {
        int group = inst.a;
        if (group * 2 + 1 >= ncap) return BT_FAIL;
        int gs = captures[group * 2];
        int ge = captures[group * 2 + 1];
        if (gs < 0 || ge < 0) return BT_FAIL;
        int blen = ge - gs;
        if (inst.offset) {
          /* A folded comparison can consume a different number of bytes than
             the captured text holds, so the span is measured, not assumed. */
          int used = memcmp_ci(sp, str_end, str + gs, str + ge, binary);
          if (used < 0) return BT_FAIL;
          sp += used;
        }
        else {
          if (sp + blen > str_end) return BT_FAIL;
          if (memcmp(sp, str + gs, blen) != 0) return BT_FAIL;
          sp += blen;
        }
        pc++;
      }
      break;

    case RE_LOOKAHEAD:
      {
        /* A sub-pattern answers BT_MATCH, BT_FAIL or BT_LIMIT, never a cut.
           The two failures go up as they are; the four lookarounds only
           differ in what a match means. */
        int r = bt_match(m, sp, pc + 1, depth + 1);
        if (r != BT_MATCH) return r;
        pc = inst.offset;
      }
      break;

    case RE_NEG_LOOKAHEAD:
      {
        int r = bt_match(m, sp, pc + 1, depth + 1);
        if (r == BT_MATCH) return BT_FAIL;
        if (r == BT_LIMIT) return r;
        pc = inst.offset;
      }
      break;

    case RE_LOOKBEHIND:
      {
        const char *back = lookbehind_start(pat, str, str_end, sp, pc, binary);
        if (!back) return BT_FAIL;  /* not enough text before */
        int r = bt_match(m, back, pc + 2, depth + 1);
        if (r != BT_MATCH) return r;
        pc = inst.offset;
      }
      break;

    case RE_NEG_LOOKBEHIND:
      {
        const char *back = lookbehind_start(pat, str, str_end, sp, pc, binary);
        if (back) {
          int r = bt_match(m, back, pc + 2, depth + 1);
          if (r == BT_MATCH) return BT_FAIL;
          if (r == BT_LIMIT) return r;
        }
        /* if not enough text before, negative lookbehind succeeds */
        pc = inst.offset;
      }
      break;

    case RE_ATOMIC:
      {
        /* The body runs on through its RE_ATOMIC_END to the end of the
           pattern inside this call, so there is nothing to continue with
           here: the answer is passed up, except that a cut aimed at this
           group is this group failing, which the caller backtracks over the
           way it would any other failed atom. */
        int r = bt_match(m, sp, pc + 1, depth + 1);
        return (r == BT_CUT(inst.offset)) ? BT_FAIL : r;
      }

    case RE_ATOMIC_END:
      {
        /* The body has matched once, and that is the only way it matches:
           when what follows fails, the failure is a cut, so that the SPLITs
           inside the body do not get to try their other branches. A limit
           is not the text failing and goes up as it is. */
        int r = bt_match(m, sp, pc + 1, depth + 1);
        return (r == BT_FAIL) ? BT_CUT(inst.offset) : r;
      }

    default:
      return BT_FAIL;
    }
  }
  return BT_FAIL;
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

  /* One block: the capture slots, then an iteration record per pc. Every
     record a search writes it undoes before returning (see bt_iter()), so
     the array is filled once for all start positions. */
  int *caps = (int*)mrb_malloc(mrb, sizeof(int) * (ncap + pat->code_len));
  bt_state m;
  m.pat = pat;
  m.str = str;
  m.str_end = str_end;
  m.captures = caps;
  m.ncap = ncap;
  m.iter_at = caps + ncap;
  m.binary = binary;
  memset(m.iter_at, -1, sizeof(int) * pat->code_len);

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

    if (bt_match(&m, sp, 0, 0) == BT_MATCH) {
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
    last_n = n;
    memcpy(last, captures, sizeof(int) * captures_size);
    pos = captures[0] + 1;
  }
  memcpy(captures, last, sizeof(int) * captures_size);
  return last_n;
}
