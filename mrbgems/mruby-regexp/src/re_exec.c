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

/* NFA thread: a position in the bytecode + captured positions */
typedef struct {
  uint32_t pc;
  int captures[RE_MAX_CAPTURES * 2];
} re_thread;

/* Thread list for NFA simulation */
typedef struct {
  re_thread *threads;
  int count;
  int capa;
} re_threadlist;

/* Match result: updated when RE_MATCH is reached during epsilon traversal */
typedef struct {
  mrb_bool matched;
  int captures[RE_MAX_CAPTURES * 2];
} re_match_result;

static void
threadlist_init(mrb_state *mrb, re_threadlist *l, int capa)
{
  l->threads = (re_thread*)mrb_malloc(mrb, sizeof(re_thread) * capa);
  l->count = 0;
  l->capa = capa;
}

static void
threadlist_free(mrb_state *mrb, re_threadlist *l)
{
  mrb_free(mrb, l->threads);
}

/* Add a thread, following epsilon transitions (JMP, SPLIT, SAVE, assertions).
   visited[] prevents adding duplicate threads at the same pc.
   When RE_MATCH is reached, records in result and does NOT add to thread list. */
static void
add_thread(const mrb_regexp_pattern *pat, re_threadlist *list,
           re_thread t, const char *str, const char *sp, const char *str_end,
           uint8_t *visited, re_match_result *result)
{
  for (;;) {
    if (t.pc >= pat->code_len) return;
    if (visited[t.pc]) return;
    visited[t.pc] = 1;

    re_inst inst = pat->code[t.pc];
    switch (inst.op) {
    case RE_JMP:
      t.pc = inst.offset;
      continue;

    case RE_SPLIT:
      /* greedy: try pc+1 first, then jump target */
      {
        re_thread t2 = t;
        t2.pc = inst.offset;
        add_thread(pat, list, t2, str, sp, str_end, visited, result);
      }
      t.pc++;
      continue;

    case RE_SPLITNG:
      /* non-greedy: try jump target first, then pc+1 */
      {
        re_thread t2 = t;
        t2.pc = t.pc + 1;
        add_thread(pat, list, t2, str, sp, str_end, visited, result);
      }
      t.pc = inst.offset;
      continue;

    case RE_SAVE:
      t.captures[inst.offset] = (int)(sp - str);
      t.pc++;
      continue;

    case RE_BOL:
      if (sp == str || ((pat->flags & RE_FLAG_MULTILINE) && sp > str && sp[-1] == '\n')) {
        t.pc++; continue;
      }
      return;

    case RE_EOL:
      if (sp == str_end || ((pat->flags & RE_FLAG_MULTILINE) && *sp == '\n')) {
        t.pc++; continue;
      }
      return;

    case RE_BOT:
      if (sp == str) { t.pc++; continue; }
      return;

    case RE_EOT:
      if (sp == str_end) { t.pc++; continue; }
      return;

    case RE_EOTNL:
      if (sp == str_end || (sp + 1 == str_end && *sp == '\n')) { t.pc++; continue; }
      return;

    case RE_WBOUND:
      {
        mrb_bool before = (sp > str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && re_is_word_char((uint8_t)*sp);
        if (before != after) { t.pc++; continue; }
      }
      return;

    case RE_NWBOUND:
      {
        mrb_bool before = (sp > str) && re_is_word_char((uint8_t)sp[-1]);
        mrb_bool after = (sp < str_end) && re_is_word_char((uint8_t)*sp);
        if (before == after) { t.pc++; continue; }
      }
      return;

    case RE_MATCH:
      /* match found during epsilon traversal.
         always update: greedy quantifiers may find longer matches later
         at the same starting position. thread priority ensures correctness. */
      if (result) {
        result->matched = TRUE;
        memcpy(result->captures, t.captures, sizeof(t.captures));
      }
      return;  /* don't add to thread list */

    default:
      /* consuming instruction: add to thread list */
      break;
    }
    break;
  }

  /* add to thread list */
  if (list->count < list->capa) {
    list->threads[list->count++] = t;
  }
}

/* Check if character matches a character class */
static mrb_bool
class_match(const re_charclass *cc, uint8_t ch)
{
  if (ch >= 128) return cc->utf8_any;
  return (cc->bitmap[ch >> 3] >> (ch & 7)) & 1;
}

/* Pike VM: NFA simulation with submatch tracking */
int
re_exec(mrb_state *mrb, const mrb_regexp_pattern *pat,
        const char *str, mrb_int len, mrb_int start,
        int *captures, int captures_size)
{
  const char *sp = str + start;
  const char *str_end = str + len;
  int ncap = pat->num_captures * 2;

  int list_capa = (int)pat->code_len * 2 + 16;
  re_threadlist curr, next;
  threadlist_init(mrb, &curr, list_capa);
  threadlist_init(mrb, &next, list_capa);

  uint8_t *visited = (uint8_t*)mrb_calloc(mrb, 1, pat->code_len + 1);

  re_match_result result;
  result.matched = FALSE;
  memset(result.captures, -1, sizeof(result.captures));

  for (; sp <= str_end; sp++) {
    /* Add a new initial thread at current position (unanchored search) */
    if (!result.matched) {
      re_thread t0;
      memset(t0.captures, -1, sizeof(t0.captures));
      t0.pc = 0;
      memset(visited, 0, pat->code_len + 1);
      add_thread(pat, &curr, t0, str, sp, str_end, visited, &result);
      /* if match found during epsilon traversal (empty pattern), done */
      if (result.matched && curr.count == 0) break;
    }

    if (sp >= str_end) break;

    /* Process all current threads against current character */
    memset(visited, 0, pat->code_len + 1);
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
          th->pc++;
          add_thread(pat, &next, *th, str, sp + 1, str_end, visited, &result);
        }
        break;

      case RE_ANY:
        if (ch != '\n') {
          th->pc++;
          add_thread(pat, &next, *th, str, sp + advance, str_end, visited, &result);
        }
        break;

      case RE_ANY_NL:
        th->pc++;
        add_thread(pat, &next, *th, str, sp + advance, str_end, visited, &result);
        break;

      case RE_CLASS:
        if (class_match(&pat->classes[inst.a], (uint8_t)ch)) {
          th->pc++;
          add_thread(pat, &next, *th, str, sp + advance, str_end, visited, &result);
        }
        break;

      case RE_NCLASS:
        if (!class_match(&pat->classes[inst.a], (uint8_t)ch)) {
          th->pc++;
          add_thread(pat, &next, *th, str, sp + advance, str_end, visited, &result);
        }
        break;

      case RE_BACKREF:
        /* TODO: backtracking for backreferences */
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

    /* if matched and no more threads, we're done */
    if (result.matched && curr.count == 0) break;
  }

  threadlist_free(mrb, &curr);
  threadlist_free(mrb, &next);
  mrb_free(mrb, visited);

  if (result.matched && captures) {
    int copy = ncap < captures_size ? ncap : captures_size;
    memcpy(captures, result.captures, sizeof(int) * copy);
  }
  return result.matched ? (ncap > 0 ? ncap : 1) : 0;
}
