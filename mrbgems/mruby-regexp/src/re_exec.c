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

/* Whether the rest of a prefix of two bytes or more stands at `p`, whose
   first byte is already known to. The last byte is tested before the ones
   between it and the first, so that a position the scan below is about to
   leave is left on one comparison. */
static inline mrb_bool
prefix_rest_at(const uint8_t *prefix, mrb_int plen, const char *p)
{
  return (uint8_t)p[plen - 1] == prefix[plen - 1] &&
         (plen == 2 || memcmp(p + 1, prefix + 1, (size_t)(plen - 2)) == 0);
}

/*
 * The first position at or after `sp` where a prefix of two bytes or more
 * stands, or NULL when the subject holds none before `str_end`.
 *
 * A scan on the first byte alone proposes a position per occurrence of it, and
 * a subject made mostly of that byte -- /aaaaab/ over a run of `a` -- then
 * costs a rejected comparison per position. The prefix's last byte is scanned
 * for too, at the offset it stands at: where it is not there, the next place
 * it is names the next position worth proposing, and getting there is one
 * scan rather than the occurrences of the first byte one at a time.
 *
 * The second scan runs only where the first byte was found and the last was
 * not, so a subject that holds the first byte nowhere costs what it costs with
 * one scan. Every round leaves `sp` past where it entered, so the two get
 * through the subject once between them whichever of the bytes is the one that
 * keeps being found.
 *
 * It earns its place by skipping positions the first scan would have proposed
 * one at a time, and a subject can be built where it skips none: both bytes
 * all through it, never at the distance the prefix puts them. It is dropped
 * where that shows, leaving the walk the one scan it had before, so no subject
 * pays for it twice over.
 */
static const char*
find_prefix_ends(const uint8_t *prefix, mrb_int plen, const char *sp, const char *str_end)
{
  if (str_end - sp < plen) return NULL;
  /* The last position a match can start at, so the first scan never proposes
     one the subject is too short to hold. */
  const char *limit = str_end - plen;
  mrb_int last = plen - 1;
  mrb_bool scan_last = TRUE;

  while (sp <= limit) {
    const char *base = sp;
    const char *p0 = (const char*)memchr(sp, prefix[0], (size_t)(limit - sp) + 1);
    if (!p0) return NULL;
    if (prefix_rest_at(prefix, plen, p0)) return p0;
    if (!scan_last || (uint8_t)p0[last] == prefix[last]) {
      /* Nothing to scan for: either the last byte is here too and only the
         bytes between the ends disagree, which says nothing about where the
         next position is, or the scan has been dropped. */
      sp = p0 + 1;
      continue;
    }
    /* Where the last byte stands next names the position a match holding it
       would start at, which is past the one the first byte just named. */
    const char *p1 = (const char*)memchr(p0 + last + 1, prefix[last],
                                         (size_t)(str_end - (p0 + last + 1)));
    if (!p1) return NULL;
    const char *cand = p1 - last;
    /* Reaching no further than the first scan had just reached is the second
       one proposing the positions it is supposed to be skipping. */
    if (cand - p0 <= p0 - base) scan_last = FALSE;
    sp = cand;
  }
  return NULL;
}

/* The first position at or after `sp` where a prefix of `plen` bytes stands,
   or NULL when the subject holds none. The first scan, and the comparison at
   the position it names, are here rather than behind the walk above: they are
   the whole of the question for a prefix of one byte, and the whole of it
   again wherever the position the scan names is the answer, which is what a
   pattern that matches often asks over and over. The walk is entered only
   where that answer was no. */
static inline const char*
find_prefix(const uint8_t *prefix, mrb_int plen, const char *sp, const char *str_end)
{
  const char *p = (const char*)memchr(sp, prefix[0], (size_t)(str_end - sp));
  if (!p || plen == 1) return p;
  /* A first occurrence of the first byte too near the end settles the whole
     subject, since every later one stands nearer still. */
  if (str_end - p < plen) return NULL;
  if (prefix_rest_at(prefix, plen, p)) return p;
  return find_prefix_ends(prefix, plen, p, str_end);
}

/*
 * Skip to the next position where the pattern's literal prefix could match.
 * Returns the found position, or NULL if no match is possible.
 */
static const char*
skip_to_prefix(const mrb_regexp_pattern *pat, const char *sp, const char *str_end)
{
  if (pat->prefix_len == 0) return sp;
  return find_prefix(pat->prefix, pat->prefix_len, sp, str_end);
}

/* Check if a byte is in the first-byte bitmap */
#define FIRST_BYTE_OK(pat, ch) \
  ((ch) >= 128 || ((pat)->first_bytes[(ch) >> 3] & (1 << ((ch) & 7))))

/*
 * Skip to the next position a match could start at, per the first-byte set.
 * Returns NULL when no such position remains: the set never holds a byte the
 * pattern can match empty at (first_set_walk() answers FALSE there), so a
 * subject with none of it left holds no match either.
 *
 * A set of up to three bytes is scanned with one bounded memchr per member,
 * each next call bounded by the nearest find so far, so every byte is read at
 * most first_byte_count times and by memchr rather than one test at a time. A
 * wider set walks the bitmap as before. The bitmap walk also stops at any
 * byte above 127, which the memchr scan runs past: the set being usable at
 * all means no match starts on a non-ASCII byte (see first_set_walk()), so
 * those stops were never candidates, only where the walk gave up.
 */
static const char*
skip_to_first_byte(const mrb_regexp_pattern *pat, const char *sp, const char *str_end)
{
  int n = pat->first_byte_count;
  if (n == 0) {
    while (sp < str_end && !FIRST_BYTE_OK(pat, (uint8_t)*sp)) sp++;
    return sp < str_end ? sp : NULL;
  }
  if (sp >= str_end) return NULL;
  /* The caller asks again after every failed attempt, and on a dense subject
     the answer is usually the position it is already at: answer that with the
     member tests alone, keeping the call per byte no dearer than the bitmap
     test it replaces. */
  uint8_t b = (uint8_t)*sp;
  for (int i = 0; i < n; i++) {
    if (b == pat->first_byte[i]) return sp;
  }
  const char *found = NULL;
  size_t span = (size_t)(str_end - sp);
  for (int i = 0; i < n; i++) {
    const char *p = (const char*)memchr(sp, pat->first_byte[i], span);
    if (p) {
      found = p;
      span = (size_t)(p - sp);
    }
  }
  return found;
}

/*
 * Skip to the next position a line-anchored match could start at: the string
 * start, or just after a \n. Everything between fails RE_BOL on its first
 * step, so those positions are gone as candidates, found by memchr rather
 * than proposed one at a time. The tests here are RE_BOL's own: the very end
 * is no line start (a trailing \n opens no final line), except when the
 * string is empty and the end is the start. Returns NULL when no candidate
 * remains.
 */
static const char*
skip_to_line_start(const char *str, const char *sp, const char *str_end)
{
  if (sp == str || (sp != str_end && sp[-1] == '\n')) return sp;
  const char *nl = (const char*)memchr(sp, '\n', (size_t)(str_end - sp));
  if (!nl || nl + 1 == str_end) return NULL;
  return nl + 1;
}

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
  mrb_bool nomem;         /* the allocator refused the search a buffer it
                             needed: it stops and answers RE_NOMEM */
  int *result_caps;       /* best match (ncap ints) */
} pike_state;

/* Hand out a capture slot, growing the pool where it has none left. TRUE is
   a slot having been handed out, and FALSE is the allocator having refused
   the growth, which is the search stopping: there is no slot to answer with,
   and an index that is not one would be read as a slot all the same. The
   refusal is recorded on the state as well, since the walk that asked for it
   is several frames deep and hands nothing back (see add_thread()).

   Growing with mrb_realloc_simple() rather than mrb_realloc() is what lets
   the refusal be an answer at all, for the reason pike_vm() gives: a raising
   allocator here jumps past the epilogue that frees the pool and releases
   the cache, and by this point the search holds all of both. */
static mrb_bool
pool_alloc(pike_state *s, int *slot)
{
  if (s->pool_next >= s->pool_capa) {
    int new_capa = s->pool_capa * 2;
    int *p = (int*)mrb_realloc_simple(s->mrb, s->cap_pool,
                                      sizeof(int) * new_capa * s->ncap);
    if (!p) {
      s->nomem = TRUE;
      return FALSE;
    }
    s->cap_pool = p;
    s->pool_capa = new_capa;
  }
  *slot = s->pool_next++;
  return TRUE;
}

/* A fresh slot holding what `src_slot` holds. Answers as pool_alloc() does,
   and for the same reason. */
static mrb_bool
pool_copy(pike_state *s, int src_slot, int *slot)
{
  int dst;
  if (!pool_alloc(s, &dst)) return FALSE;
  memcpy(&s->cap_pool[dst * s->ncap],
         &s->cap_pool[src_slot * s->ncap],
         sizeof(int) * s->ncap);
  *slot = dst;
  return TRUE;
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
    /* Both of these end the walk without an answer to hand back: a cut is
       this step having been settled by a higher-priority thread, and a
       refused allocation is the search stopping (see pool_alloc()). */
    if (s->cut || s->nomem) return;
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
        int cp = 0;
        if (!s->match_only && !pool_copy(s, cap_slot, &cp)) return;
        add_thread(s, list, pc + 1, cap_slot, sp, key);
        if (s->cut || s->nomem) return;
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
        int cp = 0;
        if (!s->match_only && !pool_copy(s, cap_slot, &cp)) return;
        add_thread(s, list, inst.offset, cap_slot, sp, back);
        if (s->cut || s->nomem) return;
        pc = pc + 1;
        cap_slot = cp;
      }
      continue;

    case RE_SAVE:
      /* No test that the position is a character boundary: it is one. A byte
         that spells no character is RE_BYTE and matches only where the subject
         byte stands alone, so no atom stops between two bytes of a character
         and no position a group is recorded at is inside one. The rule used to
         be tested here, on the end of group 0 and then on every slot. */
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
        mrb_bool before = (sp > s->str) && mrb_re_word_before(s->str, sp, s->str_end, s->binary);
        mrb_bool after = (sp < s->str_end) && mrb_re_word_at(sp, s->str_end, s->binary);
        if (before != after) { pc++; continue; }
      }
      return;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > s->str) && mrb_re_word_before(s->str, sp, s->str_end, s->binary);
        mrb_bool after = (sp < s->str_end) && mrb_re_word_at(sp, s->str_end, s->binary);
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
  /* \A: no position past the string start can begin a match, which is what
     start_cap already bounds, so the anchor costs the scan loop nothing. */
  if (pat->anchor == RE_ANCHOR_BOT) start_cap = str;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  int list_capa = RE_LIST_CAPA(pat->code_len, pat->loop_depth);

  mrb_bool match_only = (captures == NULL || captures_size == 0);

  /* Use cached VM state if available (avoids malloc per call). Whether this
     search may have it is decided here, since it decides what there is left
     to allocate; the claim itself is made below, once nothing is left to
     ask for. */
  mrb_regexp_pattern *mpat = (mrb_regexp_pattern*)pat;  /* for cache_in_use flag */
  mrb_bool use_cache = !mpat->cache_in_use && mpat->cached_visited != NULL;

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
  s.nomem = FALSE;
  s.pass_span = RE_PASS_SPAN(pat->loop_depth);
  s.gen = 0;
  s.key_max = s.pass_span - 1;

  /* Everything this search holds is given back by one epilogue below, which
     an allocator that raises would jump past: what had been taken by then
     would be leaked, and the cache claim made after it would stand for the
     rest of the pattern's life, since nothing but that epilogue clears it.
     The allocator that answers NULL is what lets a refusal be the search's
     own answer (RE_NOMEM) and leave by the same exit as every other one.
     What was not taken is left NULL, which the epilogue frees as readily. */
  size_t vsize = sizeof(uint32_t) * ((size_t)pat->code_len + 1);
  s.pool_next = 0;
  s.result_caps = NULL;
  if (match_only) {
    s.pool_capa = 1;
    s.cap_pool = (int*)mrb_malloc_simple(mrb, sizeof(int) * ncap);
    if (!s.cap_pool) s.nomem = TRUE;
  }
  else {
    s.pool_capa = list_capa * 2;
    s.cap_pool = (int*)mrb_malloc_simple(mrb, sizeof(int) * s.pool_capa * ncap);
    s.result_caps = (int*)mrb_malloc_simple(mrb, sizeof(int) * ncap);
    if (!s.cap_pool || !s.result_caps) s.nomem = TRUE;
    else memset(s.result_caps, -1, sizeof(int) * ncap);
  }

  re_threadlist curr, next;
  s.visited = NULL;
  curr.threads = next.threads = NULL;
  curr.capa = next.capa = 0;
  curr.count = next.count = 0;
  if (s.nomem) {
    /* The search is over before it starts; there is nothing left to take. */
  }
  else if (use_cache) {
    s.visited = mpat->cached_visited;
    memset(s.visited, 0, vsize);
    curr.threads = (re_thread*)mpat->cached_threads[0];
    next.threads = (re_thread*)mpat->cached_threads[1];
    curr.capa = next.capa = mpat->cached_list_capa;
  }
  else {
    s.visited = (uint32_t*)mrb_malloc_simple(mrb, vsize);
    curr.threads = (re_thread*)mrb_malloc_simple(mrb, sizeof(re_thread) * list_capa);
    next.threads = (re_thread*)mrb_malloc_simple(mrb, sizeof(re_thread) * list_capa);
    if (!s.visited || !curr.threads || !next.threads) {
      s.nomem = TRUE;
    }
    else {
      memset(s.visited, 0, vsize);
      curr.capa = next.capa = list_capa;
    }
  }

  /* Claim the cache last. The claim is on the pattern, which outlives the
     search, so it may not be made where something between it and the
     epilogue could leave without reaching the epilogue, which is what every
     allocation above could do until it stopped raising. A search that
     was refused the memory to run never claims it and never used it, so it
     is not the one to release it either. */
  if (s.nomem) use_cache = FALSE;
  else if (use_cache) mpat->cache_in_use = TRUE;

  for (; !s.nomem && sp <= str_end; sp++) {
    /* Past the last position a match may start at, only the threads already
       running can still answer; once they are gone nothing can. */
    if (!s.matched && sp > start_cap && curr.count == 0) break;
    if (!s.matched && sp <= start_cap) {
      /* Skip ahead when no active threads */
      if (curr.count == 0) {
        /* ^: every branch asserts a line start first, so only those are
           candidates. \A was folded into start_cap on entry; such a pattern
           reaches here at the start alone, where the line-start answer is
           the position unchanged. */
        if (pat->anchor != RE_ANCHOR_NONE) {
          const char *skip = skip_to_line_start(str, sp, str_end);
          if (!skip) break;
          sp = skip;
        }
        if (pat->prefix_len > 0) {
          const char *skip = skip_to_prefix(pat, sp, str_end);
          if (!skip) break;
          sp = skip;
        }
        else if (pat->has_first_bytes) {
          const char *skip = skip_to_first_byte(pat, sp, str_end);
          if (!skip) break;
          sp = skip;
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
        int slot = 0;
        if (!match_only) {
          if (!pool_alloc(&s, &slot)) break;
          memset(CAP(&s, slot), -1, sizeof(int) * ncap);
        }
        advance_gen(&s);
        s.cut = FALSE;
        add_thread(&s, &curr, 0, slot, sp, s.gen);
        if (s.nomem) break;
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
        int dst;
        if (!pool_alloc(&s, &dst)) break;
        memcpy(CAP(&s, dst), CAP(&s, curr.threads[i].cap_slot),
               sizeof(int) * ncap);
        curr.threads[i].cap_slot = i;
      }
      /* Leave before the block copy rather than after it: the staging slots
         it reads from are the ones the loop above was refused, and `base` is
         past the end of the pool without them. */
      if (s.nomem) break;
      memcpy(&s.cap_pool[0], &s.cap_pool[base * ncap],
             sizeof(int) * ncap * curr.count);
    }
    /* Every live slot is a thread's, and a match keeps what it found in
       result_caps rather than in the pool (see RE_MATCH in add_thread), so a
       step with no threads left holds no slot either and the pool goes back
       to the front. Reclaiming only where there was something to renumber
       would leave the slot each dead attempt took: a run of positions where
       nothing survives climbs the pool by one a position, and a search over
       a long enough subject asks the allocator for memory in proportion to
       the subject rather than to the pattern. */
    if (!match_only) s.pool_next = curr.count;

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
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + 1, s.gen);
        }
        break;

      case RE_BYTE:
        /* A byte that spells no character matches only where the subject byte
           spells none either: inside a character the byte belongs to that
           character (see RE_BYTE). `curr_raw` is that question already asked
           for the class path, and a byte-indexed subject answers it for every
           byte, which is where this is RE_CHAR. */
        if (ch == inst.a && curr_raw) {
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + 1, s.gen);
        }
        break;

      case RE_ANY:
        if (ch != '\n') {
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_ANY_NL:
        {
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_CLASS:
        if (class_match(&pat->classes[inst.a], curr_cp, curr_raw)) {
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      case RE_NCLASS:
        if (!class_match(&pat->classes[inst.a], curr_cp, curr_raw)) {
          int cp = 0;
          if (!match_only && !pool_copy(&s, th->cap_slot, &cp)) break;
          add_thread(&s, &next, th->pc + 1, cp, sp + advance, s.gen);
        }
        break;

      default:
        break;
      }

      /* A higher-priority thread reached a match while building `next`; the
         remaining (lower-priority) threads in `curr` are cut for this step.
         A refused allocation stops the whole search rather than this step,
         and the loop above reads it as its own end. */
      if (s.cut || s.nomem) break;
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
  if (s.nomem) {
    /* The allocator had nothing left for a buffer this search needed. It is
       told apart from a limit all the way out to the caller for the reason
       backtrack_exec() gives: what it asks for is not a knob turned up. */
    ret = RE_NOMEM;
  }
  else if (s.matched) {
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
 * Where a lookbehind branch starts matching from: sp rewound by the byte
 * count the RE_LB_WIDTH at its head carries for a binary subject, and
 * otherwise by the character count beside it. The backward walk steps over
 * continuation bytes with mrb_re_char_interior_p(), which keeps it on the
 * boundaries the forward decode uses, broken input included. Returns NULL
 * when the text before sp runs out first.
 */
static const char*
lookbehind_start(const char *str, const char *str_end, const char *sp,
                 uint16_t width, mrb_bool binary)
{
  if (binary) {
    int lb_len = RE_LB_BYTES(width);
    return (sp - str < lb_len) ? NULL : sp - lb_len;
  }
  int nchars = RE_LB_CHARS(width);
  while (nchars > 0) {
    if (sp <= str) return NULL;
    sp--;
    while (sp > str && mrb_re_char_interior_p(str, sp, str_end)) sp--;
    nchars--;
  }
  return sp;
}

/* What one bt_match() call answers. BT_MATCH is the search having reached
   RE_MATCH, BT_FAIL is every alternative it held having been tried, BT_LIMIT
   is the search giving up at one of the limits before it had either, and
   BT_NOMEM is the allocator having refused the state it needed. Neither of
   the last two says anything about the text, so neither is read as a branch
   having failed and answered from the alternatives left: that would be
   answering a smaller question with whatever they produce: a shorter match, a
   later one or none, told from the real answer by nothing. backtrack_exec()
   stops the whole search at either (see there), and tells the two apart
   there, since what a limit asks of the build is that MRB_REGEXP_STACK_LIMIT
   be turned up, which is the worst thing it could do about an allocator that
   had nothing left to give.

   BT_OK is not one of bt_match()'s answers. It is what bt_push(), bt_log()
   and bt_iter_begin() answer when there is nothing to hand up, so that what
   they answer otherwise is bt_match()'s answer as it stands. */
#define BT_FAIL 0
#define BT_MATCH 1
#define BT_LIMIT 2
#define BT_NOMEM 3
#define BT_OK 4

/* What taking a choice point does beyond resuming where it points. */
enum re_cp_kind {
  RE_CP_FORK,   /* nothing: a branch the search has not tried yet */
  RE_CP_ITER,   /* the branch begins an iteration of the loop its `group`
                   keys, whose record is written when the branch is taken
                   rather than when it was pushed (see bt_iter_begin()) */
  RE_CP_BARRIER,/* not a branch at all: where the group its `group` names was
                   entered. Reaching it while backtracking is that group
                   failing, so it resumes nothing and the search goes on
                   popping; its end drops it and everything above it, which is
                   how an atomic group and a positive lookaround cut (see
                   RE_ATOMIC_END and RE_LOOK_END). */
  RE_CP_NEG,    /* the barrier of a negative lookaround, which is both: its
                   sub-pattern running out of alternatives is the assertion
                   holding, so reaching it resumes the text after the
                   lookaround, at the sp and pc it holds. */
  RE_CP_CALL,   /* not a branch: a call frame, pushed where RE_CALL entered a
                   group's body. `pc` is where the body's RE_RETURN goes on
                   and `sp` is where the input stood, which is where the
                   invocation's capture opens; `group` is the group, for the
                   assert alone. Living on this stack is what makes a call
                   backtrack-safe with no bookkeeping: a failure that goes
                   back past the call pops the frame with everything above
                   it, and MRB_REGEXP_STACK_LIMIT bounds the call depth by
                   bounding the frames. */
  RE_CP_ABSENT, /* not a branch: the state one absent repeater's scan runs
                   on. `sp` is where the absent began. `pc` is how far it may
                   still reach as an offset into the subject: the whole of
                   what stands after it until a match of the body says
                   otherwise, and below `sp` once the body has matched empty
                   where the absent began, which is the absent matching
                   nothing anywhere. `group` is the end of the subject the
                   text around it runs against, put back when the scan ends.
                   RE_ABSENT pops it at the head of each round and pushes it
                   back for the next, so the scan's state is backtracking
                   state like any other and a failure that goes back past the
                   absent drops it. Reaching it while backtracking resumes
                   nothing: there is no round left to try. */
  RE_CP_ABSENT_ITER, /* the round after the one now running, pushed directly
                        above the state it belongs to: the body failing at
                        this position is the scan going on at the next.
                        RE_ABSENT_END takes it by hand once the body has
                        matched, which is what drops the alternatives the
                        body left rather than trying them. */
  RE_CP_ABSENT_BACK, /* the ends an absent repeater has not answered with
                        yet: `sp` is the next one and `group` is where the
                        absent began, which is the last. Taking one pushes
                        the one below it, so a scan of any length leaves one
                        choice point behind rather than one per position. */
  RE_CP_RET     /* not a branch either: the mark RE_RETURN leaves in place of
                   popping the frame it answered, which backtracking may
                   still need. The next RE_RETURN's downward scan counts
                   these against the frames it passes, so each return pairs
                   with the innermost frame no return has answered yet, as
                   Onigmo's STACK_RETURN counts its STK_RETURN marks. */
};

/* A choice point: an alternative the search has not tried yet, as where the
   input stood (`sp`), which instruction takes the alternative (`pc`) and how
   tall the undo log was when it was pushed (`undo_top`). Backtracking pops
   one, takes back every write logged above its `undo_top` and goes on from
   its `sp` and `pc`. A fork costs one of these rather than the C frame the
   engine used to recurse into, so the C stack a search spends is the one
   bt_match() call it makes, whatever the subject: the state that grows is
   this stack, on the heap.

   MRB_REGEXP_STACK_LIMIT is what bounds it (see there and bt_room()). */
typedef struct {
  const char *sp;
  uint32_t pc;
  uint32_t undo_top;
  uint32_t group;   /* what `kind` names, where it names something */
  int pass;         /* the pass it was pushed in, which is the pass the
                       branch it holds runs in; for a lookaround's barrier
                       that is the pass the lookaround was entered from, and
                       the sub-pattern above it runs in one of its own */
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

/* Everything one backtrack_exec() call carries: the pattern, the subject,
   the capture slots being written, the step count, the iteration records and
   the two stacks the backtracking state stands on. What the search itself
   holds between instructions is only its position and its pc. */
typedef struct {
  mrb_state *mrb;
  const mrb_regexp_pattern *pat;
  const char *str;
  const char *str_end;
  int *captures;
  int ncap;
  int steps;
  /* Per pc, the offset the loop that pc keys was entered at, or -1 while
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
     see mark_empty_loops(). */
  int *entered_at;
  /* Per pc, the pass that wrote entered_at[pc]. A pass is one run of a
     lookaround's sub-pattern, numbered from `pass_seq` so that no two runs
     of one start position's attempt share one, and 0 is the pattern outside
     every lookaround; `pass` is the one the search is in. A record is read
     as the running iteration's only by the pass that wrote it: what a
     positive lookaround captured outlives it, and so do the records of the
     loops inside it, the undo log not unwinding at its end, so a repetition
     around the lookaround re-enters the sub-pattern with the records of the
     run before still live. The first iteration of an e+, which reads its
     record without having written it, would take one of those for its own
     where the positions coincide: `(?=(b|)+)+` on "b" re-enters at 0 while
     the run before left 1 for `(b|)+`, and its first iteration, ending at
     1, would stop there with "b" captured, where a fresh pass goes round
     once more and leaves "". */
  int *entered_in;
  int pass;
  int pass_seq;
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
  c->pass = m->pass;
  c->kind = kind;
  return BT_OK;
}

/* Write `val` into `slot`, logging what stood there so that backtracking
   past this point puts it back. The answer is bt_push()'s, and means what it
   means there: BT_OK is the write having gone through, and BT_LIMIT or
   BT_NOMEM is the search stopping, for the same two reasons and told apart
   for the same one.

   A write of the value already there leaves nothing to put back, so it is not
   logged. What MRB_REGEXP_STACK_LIMIT counts is then the state a search would
   have to restore rather than the calls it made, and a search at the limit
   goes on through such a write rather than stopping at one that would have
   restored nothing. */
static int
bt_log(bt_state *m, int *slot, int val)
{
  if (*slot == val) return BT_OK;
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

/* Where the group `group` was entered, as an index into the choice point
   stack. Barriers nest and a group's end runs while its own is the innermost
   still standing, so the walk down from the top is over what that end is
   about to drop in any case. FALSE is a group's end reached without its
   opener, which a compiled pattern does not hold; the search reads it as a
   failure rather than trusting an index. */
static mrb_bool
bt_barrier_find(const bt_state *m, uint32_t group, uint32_t *idx)
{
  uint32_t i = m->cp_top;
  while (i > 0) {
    i--;
    if ((m->cp[i].kind == RE_CP_BARRIER || m->cp[i].kind == RE_CP_NEG) &&
        m->cp[i].group == group) {
      *idx = i;
      return TRUE;
    }
  }
  return FALSE;
}

/* Record that an iteration of the loop `key` keys begins at sp, so that the
   edge closing the body can tell an iteration that matched empty. The two
   records go on the undo log, so that backtracking out of an iteration puts
   back the record of the one it lands in; the branch that begins an
   iteration is written this way rather than in place when it is taken, so
   that there is one place that writes them. They go through bt_log() like
   any other write, so an iteration that begins where the record already
   says, in the pass it already names, spends nothing on saying so again. */
static int
bt_iter_begin(bt_state *m, uint32_t key, const char *sp)
{
  int r = bt_log(m, &m->entered_at[key], (int)(sp - m->str));
  return r != BT_OK ? r : bt_log(m, &m->entered_in[key], m->pass);
}

/*
 * Backtracking engine for patterns with backreferences.
 *
 * A fork pushes a choice point and goes on with its first branch; a failure
 * pops one and goes on with the alternative it holds, taking back the writes
 * logged since it was pushed. The whole of a search is this one loop, so what
 * it costs the C stack is one call however long the subject is and however
 * deep the pattern nests; what grows instead are the two stacks on the heap,
 * which MRB_REGEXP_STACK_LIMIT bounds.
 *
 * Step-limited to prevent ReDoS.
 */
static int
bt_match(bt_state *m, const char *sp, uint32_t pc)
{
  const mrb_regexp_pattern *pat = m->pat;
  const char *str = m->str;
  /* Where the subject ends, which is where it really ends except while the
     body of an absent repeater runs: there it is the furthest the absent may
     still reach, since a run of text the absent could never take is one the
     body has no business reading. The whole of the search reads it, an
     assertion as much as a literal, so `\z` inside such a body holds where
     the scan stops rather than where the string does, which is CRuby's
     answer: Onigmo keeps the reach in the same `end` the rest of its
     executor measures against. RE_ABSENT puts it back at the head of every
     round. */
  const char *str_end = m->str_end;
  int *captures = m->captures;
  int ncap = m->ncap;
  mrb_bool binary = m->binary;
  /* What an operation on the stacks answers, which is this call's answer
     wherever it is not BT_OK. Outside the loop so that no `goto fail`
     crosses an initialization: a C++ build refuses a jump that does. */
  int r;
  re_inst inst;

  for (;;) {
    if (pc >= pat->code_len) goto fail;
    if (++m->steps > MRB_REGEXP_STEP_LIMIT) return BT_LIMIT;

    inst = pat->code[pc];
    switch (inst.op) {
    case RE_CHAR:
      if (sp >= str_end || (uint8_t)*sp != inst.a) goto fail;
      sp++; pc++;
      break;

    case RE_BYTE:
      /* see the Pike VM case */
      if (sp >= str_end || (uint8_t)*sp != inst.a) goto fail;
      if (!binary && mrb_re_charlen(sp, str_end, FALSE) != 1) goto fail;
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
              (r = bt_iter_begin(m, pc, sp)) != BT_OK) return r;
          pc++;
          break;
        }
        if (ITER_EMPTY(m, pc, sp)) { pc++; break; }
        if ((r = bt_push(m, sp, inst.offset, RE_CP_ITER, pc)) != BT_OK) return r;
        pc++;
        break;
      }
      if ((r = bt_push(m, sp, inst.offset, RE_CP_FORK, 0)) != BT_OK) return r;
      pc++;
      break;

    case RE_SPLITNG:
      /* Non-greedy fork: the jump target first, then pc+1. Marked, forward
         it heads e*? and backward it closes e+; the iteration-starting branch
         is the other one from RE_SPLIT's, and the empty-iteration stop is the
         same. */
      if (inst.a) {
        if (inst.offset > pc) {
          if ((r = bt_push(m, sp, pc + 1, RE_CP_ITER, pc)) != BT_OK) return r;
          pc = inst.offset;
          break;
        }
        if (ITER_EMPTY(m, pc, sp)) { pc++; break; }
        if ((r = bt_push(m, sp, pc + 1, RE_CP_FORK, 0)) != BT_OK ||
            (r = bt_iter_begin(m, pc, sp)) != BT_OK) return r;
        pc = inst.offset;
        break;
      }
      if ((r = bt_push(m, sp, pc + 1, RE_CP_FORK, 0)) != BT_OK) return r;
      pc = inst.offset;
      break;

    case RE_SAVE:
      {
        int slot = inst.offset;
        /* No boundary test: see the Pike VM case. */
        if (slot >= ncap) goto fail;
        /* The write is logged rather than recursed over: backtracking past
           it puts the slot back, which is what undoes what a branch captured
           before it was abandoned. What a match keeps, and what an atomic
           group or a positive lookaround keeps once it has matched, is kept
           by the log not unwinding at all there. */
        if ((r = bt_log(m, &captures[slot], (int)(sp - str))) != BT_OK) return r;
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
           iteration before it left; where there is no such end, the group
           being one this attempt has not closed yet, the clear writes the
           -1 that already stands there and bt_log() records nothing. */
        if ((slot & 1) == 0 && (r = bt_log(m, &captures[slot + 1], -1)) != BT_OK) return r;
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
        mrb_bool before = (sp > str) && mrb_re_word_before(str, sp, str_end, binary);
        mrb_bool after = (sp < str_end) && mrb_re_word_at(sp, str_end, binary);
        if (before == after) goto fail;
      }
      pc++;
      break;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && mrb_re_word_before(str, sp, str_end, binary);
        mrb_bool after = (sp < str_end) && mrb_re_word_at(sp, str_end, binary);
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
    case RE_NEG_LOOKAHEAD:
    case RE_LOOKBEHIND:
    case RE_NEG_LOOKBEHIND:
      {
        /* The lookaround is entered: a barrier stands where it began, and
           the sub-pattern goes on from here. What the barrier holds is where
           the text after the lookaround goes on from and the pass to go on
           in; for a negative one that is what taking the barrier resumes,
           its sub-pattern running out of alternatives being the assertion
           holding. The sub-pattern runs as a pass of its own, one no run
           before it had, so the records of the loops inside it that an
           earlier run may have left live are not taken for this run's (see
           bt_state).

           A lookbehind is entered at the same position as a lookahead: the
           rewind belongs to the branch, not to the opener, so that the
           branches of a body whose widths differ each look back their own
           way and are tried in the order the alternation gives them. The
           RE_LB_WIDTH at the head of the branch does it (see there). */
        mrb_bool negated = (inst.op == RE_NEG_LOOKAHEAD || inst.op == RE_NEG_LOOKBEHIND);
        if ((r = bt_push(m, sp, inst.offset, negated ? RE_CP_NEG : RE_CP_BARRIER,
                         pat->code[inst.offset - 1].offset)) != BT_OK) {
          return r;
        }
        m->pass = ++m->pass_seq;
        pc++;
        /* A lookbehind whose body takes one width begins with that rewind,
           and taking it here rather than through another turn of the loop is
           what leaves such a lookbehind, every one this engine compiled
           before a branch could have a width of its own, costing what it
           did. A body whose branches differ begins with the fork that picks
           between them, and each branch rewinds when it is reached.

           The opcode alone tells the two apart for a lookahead as well: a
           rewind stands at the head of a lookbehind branch and nowhere
           else, so a lookahead's body never begins with one. */
        if (pat->code[pc].op != RE_LB_WIDTH) break;
        inst = pat->code[pc];
      }
      /* fall through */

    case RE_LB_WIDTH:
      {
        /* The branch this heads looks back by the width it carries. Too
           little text before is this branch failing and nothing more: what
           the search tries next is the branch after it, and with none left
           the barrier below answers, a positive lookbehind not holding and
           a negative one holding with no sub-pattern having run. */
        const char *from = lookbehind_start(str, str_end, sp, inst.offset, binary);
        if (!from) goto fail;
        sp = from;
        pc++;
      }
      break;

    case RE_LOOK_END:
      {
        /* The sub-pattern has matched, and that is the whole of what the
           lookaround asks of it: no alternative inside it may be tried for
           the text after, so its choice points go, barrier and all, as an
           atomic group's do. A positive one goes on with the text after from
           where it was entered, and what the sub-pattern captured stays --
           the undo log is left alone, and a failure that goes back past
           where the lookaround began takes it back with everything else
           logged since. A negative one is the assertion failing: the log
           unwinds to where the lookaround began, so what the sub-pattern
           captured goes with it, and the search backtracks. Either way the
           pass the lookaround was entered from comes back with the
           barrier. */
        uint32_t idx;
        if (!bt_barrier_find(m, inst.offset, &idx)) goto fail;
        re_cpoint c = m->cp[idx];
        /* Where the sub-pattern is one whose branches rewind by different
           widths, it has to have landed back where the lookaround was
           entered: `(?<=c|ab)` rewound the two characters `ab` asks for can
           still match `c` and stop a character short, which is a match of
           the text before the wrong position. The test comes before the cut
           below, so that a branch that lands short leaves the search the
           alternatives it has not tried, which are the branches after it,
           each with its own rewind. Every other lookaround lands where it
           must by construction and carries no such bit; see RE_LOOK_LANDING.
           No boundary test either: a sub-pattern reaches the same
           positions as the rest of the search, so an assertion that used to
           hold on half a character has no half to hold on. */
        if ((inst.a & RE_LOOK_LANDING) && sp != c.sp) goto fail;
        m->cp_top = idx;
        m->pass = c.pass;
        if (inst.a & RE_LOOK_NEGATED) {
          bt_undo_to(m, c.undo_top);
          goto fail;
        }
        sp = c.sp;
        pc++;
      }
      break;

    case RE_ATOMIC:
      /* The group is entered: a barrier stands where it began, so that a
         failure inside the body that reaches it is the group failing, and
         the body goes on from here. */
      if ((r = bt_push(m, sp, pc + 1, RE_CP_BARRIER, inst.offset)) != BT_OK) return r;
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
        if (!bt_barrier_find(m, inst.offset, &idx)) goto fail;
        m->cp_top = idx;
        pc++;
      }
      break;

    case RE_CALL:
      {
        /* The group's body is entered: a frame holds where its RE_RETURN
           goes on and where the input stands, and the group's end slot is
           cleared so the group reads as unmatched while the invocation is
           open -- the same invalidation entering a group inline makes at
           RE_SAVE, and what makes `(?<a>x|\k<a>y)\g<a>` read \k<a> as
           unmatched inside the second invocation rather than as the first
           invocation's text, which is CRuby's answer. The clear is logged,
           so backtracking out of the call puts back what an earlier
           invocation captured. */
        int slot = inst.a * 2 + 1;
        if (slot < ncap && (r = bt_log(m, &captures[slot], -1)) != BT_OK) return r;
        if ((r = bt_push(m, sp, pc + 1, RE_CP_CALL, inst.a)) != BT_OK) return r;
        pc = inst.offset;
      }
      break;

    case RE_RETURN:
      {
        /* The invocation completed: find its frame -- the innermost one no
           return has answered yet, the returns already made counting
           against the frames they answered -- write the group's capture
           pair from it, leave the mark, and go on where the frame says.
           The pair is written whole here rather than half at entry: the
           invocation that completes last is the one the capture names, as
           in CRuby, where `(?<a>x)\g<a>` leaves the call's text and the
           recursion this feature exists for leaves the outermost span. */
        uint32_t i = m->cp_top;
        uint32_t level = 0;
        uint32_t idx = 0;
        mrb_bool found = FALSE;
        while (i > 0) {
          i--;
          if (m->cp[i].kind == RE_CP_RET) level++;
          else if (m->cp[i].kind == RE_CP_CALL) {
            if (level == 0) { idx = i; found = TRUE; break; }
            level--;
          }
        }
        if (!found) goto fail;  /* a compiled pattern always has the frame */
        mrb_assert(m->cp[idx].group == inst.a);
        /* Group 0 is the whole match, which may not close inside a
           character; the same rule RE_SAVE applies to slot 1. */
        if (inst.a == 0 && !binary && sp < str_end &&
            mrb_re_char_interior_p(str, sp, str_end)) {
          goto fail;
        }
        {
          int slot = inst.a * 2;
          if (slot + 1 >= ncap) goto fail;
          if ((r = bt_log(m, &captures[slot], (int)(m->cp[idx].sp - str))) != BT_OK) return r;
          if ((r = bt_log(m, &captures[slot + 1], (int)(sp - str))) != BT_OK) return r;
        }
        {
          uint32_t ret = m->cp[idx].pc;
          if ((r = bt_push(m, sp, 0, RE_CP_RET, inst.a)) != BT_OK) return r;
          pc = ret;
        }
      }
      break;

    case RE_ABSENT_START:
      /* The absent repeater is entered. Its scan runs on the state pushed
         here: it began at sp, it may reach as far as the subject reaches
         around it, and that same end is what the scan puts back when it is
         done. The subject an absent inside the body of another one runs
         against is the one that one has narrowed. */
      if ((r = bt_push(m, sp, (uint32_t)(str_end - str), RE_CP_ABSENT,
                       (uint32_t)(str_end - str))) != BT_OK) {
        return r;
      }
      pc++;
      break;

    case RE_ABSENT:
      {
        /* One round of the scan, at the position the round before left.
           Every position from where the absent began to the end it may still
           reach is an end the absent can take, so the round either takes the
           furthest of them, leaving the shorter ones to backtracking under
           one choice point for all of them, or runs the body once here and
           goes on at the next position.

           The state is popped and pushed back rather than read in place: the
           round runs the body above it, and a body that fails has to land on
           the choice point that begins the next round and not inside a state
           the next round is about to read. */
        if (m->cp_top == 0 || m->cp[m->cp_top - 1].kind != RE_CP_ABSENT) goto fail;
        re_cpoint st = m->cp[--m->cp_top];
        m->pass = st.pass;
        str_end = str + st.group;
        int begun = (int)(st.sp - str);
        int reach = (int32_t)st.pc;
        /* The body matched empty where the absent began, so every run of
           text from here holds a match of it, the empty one included. */
        if (reach < begun) goto fail;
        if ((int)(sp - str) >= reach) {
          if (reach > begun) {
            const char *prev = lookbehind_start(str, str_end, str + reach,
                                                RE_LB_PACK(1, 1), binary);
            if (prev && (r = bt_push(m, prev, inst.offset, RE_CP_ABSENT_BACK,
                                     (uint32_t)begun)) != BT_OK) {
              return r;
            }
          }
          sp = str + reach;
          pc = inst.offset;
          break;
        }
        {
          const char *next = sp + mrb_re_charlen(sp, str_end, binary);
          if ((r = bt_push(m, st.sp, st.pc, RE_CP_ABSENT, st.group)) != BT_OK) return r;
          if ((r = bt_push(m, next, pc, RE_CP_ABSENT_ITER, 0)) != BT_OK) return r;
        }
        /* The body runs against a subject that ends where the absent may
           still reach, and as a pass of its own, as a lookaround's
           sub-pattern does, so that the records of the loops inside it are
           read only by the round that wrote them. */
        str_end = str + reach;
        m->pass = ++m->pass_seq;
        pc++;
      }
      break;

    case RE_ABSENT_END:
      {
        /* The body has matched, which says how far the absent may still
           reach: not past the text the body just read, so it stops at the
           last character of that text, or, where the body matched empty, at
           the position the body ran at, which the empty match stands after
           rather than inside. An empty match where the absent began leaves
           it nothing at all, recorded as a reach below that position and
           answered at the head of the next round.

           The alternatives the body left are dropped: the scan asks whether
           the body matches here, and one match is the whole of the answer.
           So is what it captured. The body is a test the scan runs and no
           part of the match, so a group inside one is left as the match
           found it, whether the run of the body matched or failed. */
        uint32_t idx = m->cp_top;
        while (idx > 0 && m->cp[idx - 1].kind != RE_CP_ABSENT_ITER) idx--;
        if (idx == 0) goto fail;
        idx--;
        re_cpoint it = m->cp[idx];
        if (idx == 0 || m->cp[idx - 1].kind != RE_CP_ABSENT) goto fail;
        re_cpoint *st = &m->cp[idx - 1];
        int reach;
        if (sp < it.sp) {
          reach = (sp == st->sp) ? -1 : (int)(sp - str);
        }
        else {
          const char *prev = lookbehind_start(str, str_end, sp, RE_LB_PACK(1, 1), binary);
          reach = prev ? (int)(prev - str) : -1;
        }
        if (reach < (int32_t)st->pc) st->pc = (uint32_t)reach;
        m->cp_top = idx;
        bt_undo_to(m, it.undo_top);
        m->pass = it.pass;
        sp = it.sp;
        pc = it.pc;
      }
      break;

    default:
      goto fail;
    }
    continue;

  fail:
    /* This branch is spent. The next alternative is the choice point on top,
       with every write since it was pushed taken back; with none left there
       is nothing more to try from this start position. */
    for (;;) {
      if (m->cp_top == 0) return BT_FAIL;
      re_cpoint c = m->cp[--m->cp_top];
      bt_undo_to(m, c.undo_top);
      /* The pass a branch was pushed in is the pass it runs in, and for a
         lookaround's barrier it is the pass the lookaround was entered
         from, which is where the search is once the barrier is taken. */
      m->pass = c.pass;
      /* A barrier is where a group was entered, not a branch: reaching it is
         that group failing, and what is left to try is below it. A negative
         lookaround's is the exception: its sub-pattern having no match
         left is the assertion holding, and what it resumes is the text after
         the lookaround. A call frame and a return's mark are not branches
         either: backtracking past a call is just the call unwinding, and
         what is left to try is below them. */
      if (c.kind == RE_CP_BARRIER || c.kind == RE_CP_CALL || c.kind == RE_CP_RET) continue;
      /* An absent repeater's state is not a branch either: reaching it is
         the scan having no round left, and the subject the text around it
         runs against comes back with it. */
      if (c.kind == RE_CP_ABSENT) { str_end = str + c.group; continue; }
      /* The ends of an absent repeater are answered longest first, and the
         one below the end being taken is what is left to try after it. */
      if (c.kind == RE_CP_ABSENT_BACK && c.sp > str + c.group) {
        const char *prev = lookbehind_start(str, str_end, c.sp, RE_LB_PACK(1, 1), binary);
        if (prev && (r = bt_push(m, prev, c.pc, RE_CP_ABSENT_BACK, c.group)) != BT_OK) {
          return r;
        }
      }
      sp = c.sp;
      pc = c.pc;
      if (c.kind == RE_CP_ITER && (r = bt_iter_begin(m, c.group, sp)) != BT_OK) return r;
      break;
    }
  }
}

static int
backtrack_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
               const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
               int *captures, int captures_size, mrb_bool binary)
{
  const char *start_cap = str + start_limit;
  const char *str_end = str + len;
  /* \A bounds the start positions the way pike_vm's clamp does. */
  if (pat->anchor == RE_ANCHOR_BOT) start_cap = str;
  int ncap = pat->num_captures * 2;
  if (ncap == 0) ncap = 2;

  /* One block: the capture slots, then an entry record per pc and the pass
     that wrote it. The arrays are filled once here and put back to what they
     hold by the undo log unwinding between start positions (see there): an
     iteration's record is written on the log now, which a match does not
     unwind, so a record left by the attempt before would otherwise be read
     as this attempt's and stop a repetition that had not gone round yet.

     Taken with mrb_malloc_simple() rather than mrb_malloc(), so that a
     refusal is answered where the growth reallocs below already answer one.
     Nothing is held here yet, so raising from this one line would strand
     nothing; what it would cost is the engine's own answer. Every other
     refusal inside a search reaches the caller as RE_NOMEM, which
     re_check_exec_error() turns into the raise, and a search that raised
     from here instead would be one the caller never saw stop. */
  int *caps = (int*)mrb_malloc_simple(mrb, sizeof(int) * (ncap + 2 * pat->code_len));
  if (!caps) return RE_NOMEM;
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
  m.pass_seq = 0;
  m.binary = binary;
  m.cp = NULL;
  m.cp_top = m.cp_capa = 0;
  m.undo = NULL;
  m.undo_top = m.undo_capa = 0;
  memset(m.entered_at, -1, sizeof(int) * pat->code_len);
  memset(m.entered_in, 0, sizeof(int) * pat->code_len);

  for (const char *sp = str + start; sp <= str_end && sp <= start_cap; sp++) {
    /* Skip ahead using the anchor, the literal prefix or the first-byte
       bitmap; the same composition as pike_vm's. */
    if (pat->anchor != RE_ANCHOR_NONE) {
      const char *skip = skip_to_line_start(str, sp, str_end);
      if (!skip) break;
      sp = skip;
    }
    if (pat->prefix_len > 0) {
      const char *skip = skip_to_prefix(pat, sp, str_end);
      if (!skip) break;
      sp = skip;
    }
    else if (pat->has_first_bytes) {
      const char *skip = skip_to_first_byte(pat, sp, str_end);
      if (!skip) break;
      sp = skip;
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
       what a fresh search finds. The pass numbering starts over with them.
       Every record a pass wrote is on that log, so once it has unwound
       there is nothing left for a number to be read against and the numbers
       need only be unique within the attempt; a counter that ran on across
       the start positions would climb with the length of the subject
       instead, until a long enough search overflowed it. */
    m.cp_top = 0;
    m.pass = 0;
    m.pass_seq = 0;
    bt_undo_to(&m, 0);

    int r = bt_match(&m, sp, 0);
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
         is read off the step count: nothing but the step check moves it, so
         it is over the step limit exactly when that check is what gave
         up. */
      ret = m.steps > MRB_REGEXP_STEP_LIMIT ? RE_OVER_STEP_LIMIT : RE_OVER_STACK_LIMIT;
      break;
    }
  }
  mrb_free(mrb, m.undo);
  mrb_free(mrb, m.cp);
  mrb_free(mrb, caps);
  return ret;
}

/* Fast path for pure literal patterns: the whole pattern is the prefix, so
   the search is the prefix skip itself and there is no NFA to run. */
static int
literal_exec(const mrb_regexp_pattern *pat,
             const char *str, mrb_int len, mrb_int start, mrb_int start_limit,
             int *captures, int captures_size, mrb_bool binary)
{
  const char *start_cap = str + start_limit;
  const char *sp = str + start;
  const char *str_end = str + len;
  mrb_int plen = pat->prefix_len;

  while (sp + plen <= str_end && sp <= start_cap) {
    const char *found = find_prefix(pat->prefix, plen, sp, str_end);
    if (!found || found > start_cap) return 0;
    if (!binary && mrb_re_char_interior_p(str, found, str_end)) {
      sp = found + 1;  /* not a char boundary, same rule as the other engines */
      continue;
    }
    /* No test that the end is a character boundary: a byte that spells no
       character is RE_BYTE, which this path never holds (the prefix is
       RE_CHAR only), so the literal is whole characters and a lead byte
       that matched fixed the length of the one it starts. */
    if (captures && captures_size >= 2) {
      captures[0] = (int)(found - str);
      captures[1] = (int)(found - str) + plen;
    }
    return 2;  /* group 0 start/end */
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
