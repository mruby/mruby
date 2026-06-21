/*
** mirb_completion.c - Tab completion support for mirb
**
** See Copyright Notice in mruby.h
*/

#include "mirb_completion.h"
#include "mirb_highlight.h"
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/value.h>
#include <mruby/variable.h>

#include <stdlib.h>
#include <string.h>

/* Windows compatibility */
#ifdef _MSC_VER
#define strdup _strdup
#endif

/* strndup is not available on Windows (MSVC and MinGW) */
#ifdef _WIN32
static char*
strndup(const char *s, size_t n)
{
  size_t len = strlen(s);
  if (len > n) len = n;
  char *p = (char*)malloc(len + 1);
  if (p) {
    memcpy(p, s, len);
    p[len] = '\0';
  }
  return p;
}
#endif

#ifdef MRB_USE_READLINE
#ifndef MRB_USE_LINENOISE
#include MRB_READLINE_HEADER
#endif
#endif

#ifdef MRB_USE_LINENOISE
#include <linenoise.h>
#endif

/* ============================================================
 * Core Completion Engine
 * ============================================================ */

void
mirb_completion_init(mirb_completion_ctx *ctx, mrb_state *mrb, mrb_ccontext *cxt)
{
  memset(ctx, 0, sizeof(*ctx));
  ctx->mrb = mrb;
  ctx->cxt = cxt;
}

void
mirb_completion_free(mirb_completion_ctx *ctx)
{
  int i;

  /* Free match prefix */
  if (ctx->match_prefix) {
    free(ctx->match_prefix);
    ctx->match_prefix = NULL;
  }

  /* Free completions */
  if (ctx->completions) {
    for (i = 0; i < ctx->completion_count; i++) {
      free(ctx->completions[i]);
    }
    free(ctx->completions);
    ctx->completions = NULL;
  }

  ctx->completion_count = 0;
  ctx->completion_alloc = 0;
  ctx->current_index = 0;
}

/* ============================================================
 * Context Analysis
 * ============================================================ */

mirb_completion_type
mirb_detect_completion_type(const char *line, int cursor_pos)
{
  int i;
  int in_string = 0;  /* 0 = not in string, '"' or '\'' = in that string type */

  /* First pass: determine if we're inside a string by scanning from start */
  for (i = 0; i < cursor_pos; i++) {
    if (in_string) {
      if (line[i] == '\\' && i + 1 < cursor_pos) {
        i++;  /* Skip escaped character */
      }
      else if (line[i] == in_string) {
        in_string = 0;  /* End of string */
      }
    }
    else {
      if (line[i] == '"' || line[i] == '\'') {
        in_string = line[i];  /* Start of string */
      }
    }
  }

  /* If we're inside a string, check for file completion context */
  if (in_string) {
    if (mirb_in_file_context(line, cursor_pos)) {
      return COMPLETION_FILE;
    }
    return COMPLETION_KEYWORD;  /* No completion inside strings */
  }

  /* Scan backwards from cursor to find context */
  for (i = cursor_pos - 1; i >= 0; i--) {
    if (line[i] == '.') {
      /* After dot = method completion */
      return COMPLETION_METHOD;
    }
    if (line[i] == '$') {
      /* Global variable */
      return COMPLETION_GLOBAL_VAR;
    }
    if (line[i] == '"' || line[i] == '\'') {
      /* This is a closing quote (we know we're not in a string) */
      /* Continue scanning to find if there's a dot before the string */
      continue;
    }
    if (ISSPACE(line[i]) || line[i] == '(' || line[i] == ',' ||
        line[i] == '[' || line[i] == '{' || line[i] == ';') {
      /* Start of new expression */
      break;
    }
  }

  /* Default: complete everything at top level */
  return COMPLETION_KEYWORD;  /* Includes keywords, locals, constants */
}

mrb_bool
mirb_in_file_context(const char *line, int quote_pos)
{
  int i;

  /* Look backwards for require or load keyword */
  for (i = quote_pos - 1; i >= 0; i--) {
    if (ISSPACE(line[i])) continue;

    /* Check for 'require' or 'load' */
    if (i >= 6 && strncmp(&line[i-6], "require", 7) == 0) return TRUE;
    if (i >= 3 && strncmp(&line[i-3], "load", 4) == 0) return TRUE;

    break;
  }
  return FALSE;
}

/* Extract receiver expression before the dot */
char *
mirb_extract_receiver(const char *line, int cursor_pos, int *recv_end)
{
  int depth = 0;  /* Parentheses/bracket depth */
  int i, start = -1;
  char *receiver;
  int len;

  /* Find the dot before cursor */
  for (i = cursor_pos - 1; i >= 0; i--) {
    if (line[i] == '.' && depth == 0) {
      *recv_end = i;
      break;
    }
    /* Track nesting depth for complex expressions */
    if (line[i] == ')' || line[i] == ']' || line[i] == '}') depth++;
    if (line[i] == '(' || line[i] == '[' || line[i] == '{') depth--;
  }

  if (i < 0) return NULL;  /* No dot found */

  /* Now find start of receiver expression */
  depth = 0;
  for (start = i - 1; start >= 0; start--) {
    char c = line[start];

    if (c == ')' || c == ']' || c == '}') depth++;
    if (c == '(' || c == '[' || c == '{') depth--;

    if (depth < 0) {
      start++;
      break;
    }

    /* Break on operators/keywords at depth 0 */
    if (depth == 0 && (ISSPACE(c) || c == '=' || c == ',' || c == ';')) {
      start++;
      break;
    }
  }

  if (start < 0) start = 0;

  /* Allocate and copy receiver */
  len = i - start;
  receiver = (char*)malloc(len + 1);
  if (!receiver) return NULL;

  memcpy(receiver, line + start, len);
  receiver[len] = '\0';

  return receiver;
}

/* ============================================================
 * Receiver Evaluation
 * ============================================================ */

/* Check if receiver expression is simple (just a name, no method calls) */
static mrb_bool
is_simple_receiver(const char *expr)
{
  int i;
  int in_string = 0;

  /* Empty is not simple */
  if (!expr || expr[0] == '\0') return FALSE;

  /* Check if it's a safe expression to evaluate */
  for (i = 0; expr[i]; i++) {
    char c = expr[i];

    if (in_string) {
      /* Inside string - allow anything except check for end */
      if (c == '\\' && expr[i+1]) {
        i++;  /* Skip escaped character */
      }
      else if (c == in_string) {
        in_string = 0;  /* End of string */
      }
    }
    else {
      /* Outside string */
      if (c == '"' || c == '\'') {
        in_string = c;  /* Start of string */
      }
      else if (c == '(' || c == ')') {
        /* Disallow method calls - could have side effects */
        return FALSE;
      }
      else if (!(ISALNUM(c) || c == '_' || c == ':' || c == '[' || c == ']' ||
                 c == '{' || c == '}' || c == ',' || c == ' ' || c == '\t' ||
                 c == '-' || c == '+' || c == '.' || c == '@')) {
        /* Disallow unknown characters */
        return FALSE;
      }
    }
  }

  /* Unclosed string is not valid */
  if (in_string) return FALSE;

  return TRUE;
}

mrb_value
mirb_eval_receiver(mrb_state *mrb, const char *receiver_expr, mrb_ccontext *cxt)
{
  struct mrb_parser_state *parser;
  struct RProc *proc;
  mrb_value result;
  int ai = mrb_gc_arena_save(mrb);

  /* Parse the receiver expression WITH compiler context to access local variables */
  parser = mrb_parse_string(mrb, receiver_expr, cxt);
  if (!parser || parser->nerr > 0) {
    if (parser) mrb_parser_free(parser);
    return mrb_nil_value();
  }

  /* Generate and execute */
  proc = mrb_generate_code(mrb, parser);
  mrb_parser_free(parser);

  if (!proc) {
    return mrb_nil_value();
  }

  result = mrb_vm_run(mrb, proc, mrb_top_self(mrb), 0);

  /* Clear exception if any */
  if (mrb->exc) {
    mrb->exc = NULL;
    result = mrb_nil_value();
  }

  mrb_gc_arena_restore(mrb, ai);
  return result;
}

/* ============================================================
 * Method Completion
 * ============================================================ */

/* Callback for mrb_mt_foreach */
struct method_collector {
  mirb_completion_ctx *ctx;
  int count;
};

static int
collect_method_callback(mrb_state *mrb, mrb_sym sym, mrb_method_t method, void *data)
{
  struct method_collector *mc = (struct method_collector*)data;
  const char *name = mrb_sym_name(mrb, sym);

  (void)method;  /* Unused */

  /* Skip internal methods (start with __) */
  if (name[0] == '_' && name[1] == '_') {
    return 0;  /* Continue iteration */
  }

  /* Add if matches prefix */
  mirb_add_completion(mc->ctx, name);
  mc->count++;

  return 0;  /* Continue */
}

void
mirb_complete_methods(mirb_completion_ctx *ctx, mrb_value receiver)
{
  struct RClass *klass = mrb_class(ctx->mrb, receiver);
  struct method_collector mc = { ctx, 0 };

  /* Walk up class hierarchy */
  while (klass) {
    mrb_mt_foreach(ctx->mrb, klass, collect_method_callback, &mc);
    klass = klass->super;
  }
}

/* ============================================================
 * Keyword and Variable Completion
 * ============================================================ */

void
mirb_complete_keywords(mirb_completion_ctx *ctx)
{
  int i;
  for (i = 0; mirb_keywords[i] != NULL; i++) {
    mirb_add_completion(ctx, mirb_keywords[i]);
  }
}

void
mirb_complete_local_vars(mirb_completion_ctx *ctx)
{
  int i;

  /* Local variables from compiler context */
  if (ctx->cxt && ctx->cxt->syms) {
    for (i = 0; i < (int)ctx->cxt->slen; i++) {
      const char *name = mrb_sym_name(ctx->mrb, ctx->cxt->syms[i]);
      if (name && name[0] != '_') {  /* Skip underscore-only */
        mirb_add_completion(ctx, name);
      }
    }
  }
}

void
mirb_complete_global_vars(mirb_completion_ctx *ctx)
{
  mrb_value gvars;
  mrb_int len, i;
  int ai = mrb_gc_arena_save(ctx->mrb);

  /* Use Ruby to get global variables */
  gvars = mrb_funcall_argv(ctx->mrb, mrb_obj_value(ctx->mrb->kernel_module),
                            mrb_intern_lit(ctx->mrb, "global_variables"),
                            0, NULL);

  if (ctx->mrb->exc) {
    ctx->mrb->exc = NULL;
    mrb_gc_arena_restore(ctx->mrb, ai);
    return;
  }

  if (mrb_array_p(gvars)) {
    len = RARRAY_LEN(gvars);

    for (i = 0; i < len; i++) {
      mrb_value sym = mrb_ary_entry(gvars, i);
      mrb_sym s = mrb_symbol(sym);
      const char *name = mrb_sym_name(ctx->mrb, s);
      if (name) {
        mirb_add_completion(ctx, name);
      }
    }
  }

  mrb_gc_arena_restore(ctx->mrb, ai);
}

void
mirb_complete_constants(mirb_completion_ctx *ctx, struct RClass *scope)
{
  mrb_value consts;
  mrb_int len, i;
  int ai = mrb_gc_arena_save(ctx->mrb);

  /* Use Ruby to get constants */
  consts = mrb_funcall_argv(ctx->mrb,
                             mrb_obj_value(scope ? scope : ctx->mrb->object_class),
                             mrb_intern_lit(ctx->mrb, "constants"),
                             0, NULL);

  if (ctx->mrb->exc) {
    ctx->mrb->exc = NULL;
    mrb_gc_arena_restore(ctx->mrb, ai);
    return;
  }

  if (mrb_array_p(consts)) {
    len = RARRAY_LEN(consts);

    for (i = 0; i < len; i++) {
      mrb_value sym = mrb_ary_entry(consts, i);
      mrb_sym s = mrb_symbol(sym);
      const char *name = mrb_sym_name(ctx->mrb, s);
      if (name) {
        mirb_add_completion(ctx, name);
      }
    }
  }

  mrb_gc_arena_restore(ctx->mrb, ai);
}

void
mirb_complete_files(mirb_completion_ctx *ctx, const char *partial_path)
{
  /* File completion implementation would go here */
  /* For now, just a stub */
  (void)ctx;
  (void)partial_path;
}

/* ============================================================
 * Completion Management
 * ============================================================ */

void
mirb_add_completion(mirb_completion_ctx *ctx, const char *text)
{
  char **new_completions;
  int new_alloc;

  /* Check if matches prefix */
  if (ctx->prefix_len > 0) {
    if (strncmp(text, ctx->match_prefix, ctx->prefix_len) != 0) {
      return;  /* Doesn't match */
    }
  }

  /* Grow array if needed */
  if (ctx->completion_count >= ctx->completion_alloc) {
    new_alloc = ctx->completion_alloc == 0 ? 16 : ctx->completion_alloc * 2;
    new_completions = (char**)realloc(ctx->completions,
                                      new_alloc * sizeof(char*));
    if (!new_completions) return;  /* Out of memory */

    ctx->completions = new_completions;
    ctx->completion_alloc = new_alloc;
  }

  /* Add completion */
  ctx->completions[ctx->completion_count] = strdup(text);
  if (ctx->completions[ctx->completion_count]) {
    ctx->completion_count++;
  }
}

void
mirb_generate_completions(mirb_completion_ctx *ctx, const char *line, int cursor_pos)
{
  mirb_completion_type type;
  int i, recv_end;
  char *receiver_expr;
  mrb_value receiver;

  /* Store context */
  ctx->line_buf = line;
  ctx->cursor_pos = cursor_pos;

  /* Extract prefix to match */
  for (i = cursor_pos - 1; i >= 0; i--) {
    char c = line[i];
    if (!ISALNUM(c) && c != '_' && c != '?' && c != '!' && c != '$' && c != '@') {
      break;
    }
  }
  i++;  /* Move to start of identifier */

  if (ctx->match_prefix) {
    free(ctx->match_prefix);
  }
  ctx->match_prefix = strndup(line + i, cursor_pos - i);
  ctx->prefix_len = cursor_pos - i;

  /* Detect completion type */
  type = mirb_detect_completion_type(line, cursor_pos);

  /* Generate completions based on type */
  switch (type) {
    case COMPLETION_METHOD:
      receiver_expr = mirb_extract_receiver(line, cursor_pos, &recv_end);
      if (receiver_expr) {
        /* Only evaluate simple receivers to avoid corrupting VM state.
         * Complex expressions like "obj.method()" are skipped for now.
         * This prevents local variables from being cleared during tab completion. */
        if (is_simple_receiver(receiver_expr)) {
          receiver = mirb_eval_receiver(ctx->mrb, receiver_expr, ctx->cxt);
          if (!mrb_nil_p(receiver)) {
            mirb_complete_methods(ctx, receiver);
          }
        }
        free(receiver_expr);
      }
      break;

    case COMPLETION_GLOBAL_VAR:
      mirb_complete_global_vars(ctx);
      break;

    case COMPLETION_FILE:
      mirb_complete_files(ctx, ctx->match_prefix);
      break;

    case COMPLETION_LOCAL_VAR:
    case COMPLETION_CONSTANT:
    case COMPLETION_KEYWORD:
    default:
      /* Complete everything */
      mirb_complete_keywords(ctx);
      mirb_complete_local_vars(ctx);
      mirb_complete_constants(ctx, NULL);
      break;
  }
}

/* ============================================================
 * Shared Completion Context
 * ============================================================ */

static mirb_completion_ctx *g_ctx = NULL;

static mrb_bool
init_completion_ctx(mrb_state *mrb, mrb_ccontext *cxt)
{
  if (g_ctx) return TRUE;
  g_ctx = (mirb_completion_ctx*)malloc(sizeof(mirb_completion_ctx));
  if (!g_ctx) return FALSE;
  mirb_completion_init(g_ctx, mrb, cxt);
  return TRUE;
}

void
mirb_cleanup_completion(void)
{
  if (g_ctx) {
    mirb_completion_free(g_ctx);
    free(g_ctx);
    g_ctx = NULL;
  }
}

/* ============================================================
 * Readline/Libedit Adapter
 * ============================================================ */

#ifdef MRB_USE_READLINE
#ifndef MRB_USE_LINENOISE

static char *
mirb_readline_generator(const char *text, int state)
{
  (void)text;  /* text is already in match_prefix */

  /* state == 0: first call, generate completions */
  if (state == 0) {
    mirb_completion_free(g_ctx);

    /* Generate completions based on full line */
    mirb_generate_completions(g_ctx, rl_line_buffer, rl_point);

    g_ctx->current_index = 0;
  }

  /* Return next completion or NULL when done */
  if (g_ctx->current_index < g_ctx->completion_count) {
    char *completion = g_ctx->completions[g_ctx->current_index];
    g_ctx->current_index++;

    /* readline will free this, so duplicate */
    return strdup(completion);
  }

  return NULL;
}

static char **
mirb_readline_completion(const char *text, int start, int end)
{
  (void)start;
  (void)end;

  /* Prevent default filename completion */
  rl_attempted_completion_over = 1;

  /* Use our generator */
  return rl_completion_matches(text, mirb_readline_generator);
}

void
mirb_setup_readline_completion(mrb_state *mrb, mrb_ccontext *cxt)
{
  if (!init_completion_ctx(mrb, cxt)) return;

  /* Set completion function */
  rl_attempted_completion_function = mirb_readline_completion;

  /* Configure readline behavior - include . so "obj.method" are separate words */
  rl_basic_word_break_characters = " \t\n\"\\'`@$><=;|&{(.";
  rl_completer_word_break_characters = " \t\n\"\\'`@$><=;|&{(.";
}

#endif
#endif

/* ============================================================
 * Linenoise Adapter
 * ============================================================ */

#ifdef MRB_USE_LINENOISE

static void
mirb_linenoise_completion(const char *buf, linenoiseCompletions *lc)
{
  int cursor_pos = (int)strlen(buf);  /* linenoise completes at end */
  int i, prefix_start;
  char completion_line[1024];

  /* Clear previous completions */
  mirb_completion_free(g_ctx);

  /* Generate completions */
  mirb_generate_completions(g_ctx, buf, cursor_pos);

  /* Add each completion to linenoise */
  for (i = 0; i < g_ctx->completion_count; i++) {
    /* Need to build full line with completion */
    prefix_start = cursor_pos - g_ctx->prefix_len;

    /* Copy line up to prefix */
    if (prefix_start > 0) {
      memcpy(completion_line, buf, prefix_start);
    }

    /* Add completion */
    strcpy(completion_line + prefix_start, g_ctx->completions[i]);

    linenoiseAddCompletion(lc, completion_line);
  }
}

void
mirb_setup_linenoise_completion(mrb_state *mrb, mrb_ccontext *cxt)
{
  if (!init_completion_ctx(mrb, cxt)) return;

  /* Set completion callback */
  linenoiseSetCompletionCallback(mirb_linenoise_completion);
}

#endif

/* ============================================================
 * Custom Editor Adapter
 * ============================================================ */

void
mirb_setup_editor_completion(mrb_state *mrb, mrb_ccontext *cxt)
{
  init_completion_ctx(mrb, cxt);
}

int
mirb_get_completions(const char *line, int cursor_pos,
                     char ***completions_out, int *prefix_len_out)
{
  int i;

  if (!g_ctx) {
    *completions_out = NULL;
    *prefix_len_out = 0;
    return 0;
  }

  /* Clear previous completions */
  mirb_completion_free(g_ctx);

  /* Generate completions */
  mirb_generate_completions(g_ctx, line, cursor_pos);

  /* Return results */
  *prefix_len_out = g_ctx->prefix_len;

  if (g_ctx->completion_count == 0) {
    *completions_out = NULL;
    return 0;
  }

  /* Copy completions (caller will free) */
  *completions_out = (char**)malloc(g_ctx->completion_count * sizeof(char*));
  if (!*completions_out) return 0;

  for (i = 0; i < g_ctx->completion_count; i++) {
    (*completions_out)[i] = strdup(g_ctx->completions[i]);
  }

  return g_ctx->completion_count;
}

void
mirb_free_completions(char **completions, int count)
{
  int i;
  if (completions) {
    for (i = 0; i < count; i++) {
      free(completions[i]);
    }
    free(completions);
  }
}
