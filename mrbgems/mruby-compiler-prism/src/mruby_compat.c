#include "../include/mrc_common.h"

#if defined(MRC_TARGET_MRUBY)

#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/dump.h>
#include <mruby/error.h>
#include <mruby/internal.h>
#include <mruby/proc.h>
#include <string.h>

#include "../include/mrc_ccontext.h"
#include "../include/mrc_compile.h"
#include "../include/mrc_diagnostic.h"
#include "../include/mrc_irep.h"
#include "../include/mrc_parser_util.h"
#include "../include/mrc_pool.h"

#define MRC_COMPAT_DUMP_OK 0
#define MRC_COMPAT_DUMP_DEBUG_INFO 1

int mrc_dump_irep(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, uint8_t **bin, size_t *bin_size);

typedef mrb_bool mrb_parser_foreach_top_variable_func(mrb_state *mrb, mrb_sym sym, void *user);

static void
copy_context_to_mrc(mrc_ccontext *dst, const mrb_ccontext *src)
{
  if (!src) return;

  dst->slen = src->slen;
  dst->lineno = src->lineno;
  dst->target_class = src->target_class;
  dst->capture_errors = FALSE;
  dst->dump_result = src->dump_result;
  dst->no_exec = src->no_exec;
  dst->keep_lv = src->keep_lv;
  dst->no_optimize = src->no_optimize;
  dst->no_ext_ops = src->no_ext_ops;
  dst->upper = src->upper;
  if (src->filename) {
    mrc_ccontext_filename(dst, src->filename);
  }
  if (src->syms && src->slen > 0) {
    pm_options_t *options = (pm_options_t*)mrc_calloc(dst, 1, sizeof(pm_options_t));
    pm_options_scope_t *scope;

    pm_string_constant_init(&options->encoding, "UTF-8", 5);
    pm_options_scopes_init(options, 1);
    scope = &options->scopes[0];
    pm_options_scope_init(scope, (size_t)src->slen);
    for (int i = 0; i < src->slen; i++) {
      const char *name = mrb_sym_name(dst->mrb, src->syms[i]);
      if (name) {
        size_t len = strlen(name);
        char *copy = (char*)mrc_malloc(dst, len);
        memcpy(copy, name, len);
        pm_string_constant_init(&scope->locals[i], copy, len);
      }
    }
    dst->options = options;
  }
}

static void
update_context_locals_from_irep(mrb_state *mrb, mrb_ccontext *c, mrc_ccontext *mc, const mrc_irep *irep)
{
  mrb_sym *syms;
  uint16_t i;
  int count = 0;

  if (!c || !irep || !irep->lv) return;
  for (i = 0; i + 1 < irep->nlocals; i++) {
    mrc_int len = 0;
    const char *name = mrc_sym_name_len(mc, irep->lv[i], &len);
    if (name && len > 0) count++;
  }
  if (count == 0) return;

  syms = (mrb_sym*)mrb_realloc(mrb, c->syms, sizeof(mrb_sym) * count);
  c->syms = syms;
  c->slen = count;
  count = 0;
  for (i = 0; i + 1 < irep->nlocals; i++) {
    mrc_int len = 0;
    const char *name = mrc_sym_name_len(mc, irep->lv[i], &len);
    if (name && len > 0) {
      c->syms[count++] = mrb_intern(mrb, name, (size_t)len);
    }
  }
}

static void
free_parser_messages(mrb_state *mrb, struct mrb_parser_message *messages, size_t count)
{
  size_t i;

  for (i = 0; i < count; i++) {
    if (messages[i].message) {
      mrb_free(mrb, messages[i].message);
      messages[i].message = NULL;
    }
  }
}

static void
copy_parser_message(mrb_state *mrb, struct mrb_parser_message *dst, const mrc_diagnostic_list *src)
{
  size_t len;

  dst->lineno = src->line > UINT16_MAX ? UINT16_MAX : (uint16_t)src->line;
  dst->column = (int)src->column;
  len = strlen(src->message);
  dst->message = (char*)mrb_malloc(mrb, len + 1);
  memcpy(dst->message, src->message, len + 1);
}

static void
copy_diagnostics_to_parser(mrb_state *mrb, struct mrb_parser_state *p, const mrc_ccontext *c)
{
  const mrc_diagnostic_list *d;

  for (d = c->diagnostic_list; d; d = d->next) {
    if (d->code == MRC_PARSER_WARNING || d->code == MRC_GENERATOR_WARNING) {
      if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
        copy_parser_message(mrb, &p->warn_buffer[p->nwarn], d);
      }
      p->nwarn++;
    }
    else {
      if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
        copy_parser_message(mrb, &p->error_buffer[p->nerr], d);
      }
      p->nerr++;
    }
  }
  if (p->nerr == 0 && p->tree == NULL) {
    p->error_buffer[0].lineno = 0;
    p->error_buffer[0].column = 0;
    p->error_buffer[0].message = (char*)mrb_malloc(mrb, sizeof("compile error"));
    memcpy(p->error_buffer[0].message, "compile error", sizeof("compile error"));
    p->nerr = 1;
  }
}

static struct mrb_parser_state*
parser_alloc(mrb_state *mrb, mrb_ccontext *c)
{
  struct mrb_parser_state *p;

  p = (struct mrb_parser_state*)mrb_calloc(mrb, 1, sizeof(struct mrb_parser_state));
  p->mrb = mrb;
  p->cxt = c;
  p->capture_errors = c ? c->capture_errors : FALSE;
  p->no_ext_ops = c ? c->no_ext_ops : FALSE;
  p->no_return_value = c ? c->no_return_value : FALSE;
  p->upper = c ? c->upper : NULL;
  if (c && c->filename) {
    p->filename_sym = mrb_intern_cstr(mrb, c->filename);
  }
  return p;
}

static struct mrb_parser_state*
parse_source(mrb_state *mrb, const char *s, size_t len, mrb_ccontext *c)
{
  struct mrb_parser_state *p;
  mrc_ccontext *mc;
  uint8_t *source;
  const uint8_t *parse_source;
  mrc_irep *irep;

  p = parser_alloc(mrb, c);
  source = (uint8_t*)mrb_malloc(mrb, len + 1);
  memcpy(source, s, len);
  source[len] = '\0';
  p->s = (const char*)source;
  p->send = (const char*)source + len;

  mc = mrc_ccontext_new(mrb);
  copy_context_to_mrc(mc, c);
  p->ylval = mc;

  parse_source = source;
  irep = mrc_load_string_cxt(mc, &parse_source, len);
  update_context_locals_from_irep(mrb, c, mc, irep);
  p->tree = (mrb_ast_node*)irep;
  copy_diagnostics_to_parser(mrb, p, mc);
  if (c) {
    c->parser_nerr = p->nerr;
  }
#ifndef MRB_NO_STDIO
  /* Unless the caller captures errors (e.g. eval), report parse errors to
     stderr like the bison parser does, so a syntax error is not a silent
     failure. Only parser errors are emitted here; codegen errors are
     already printed by the generator (see codegen_error()). */
  if (!c || !c->capture_errors) {
    const char *fn = (c && c->filename) ? c->filename : "(string)";
    const mrc_diagnostic_list *d;
    for (d = mc->diagnostic_list; d; d = d->next) {
      if (d->code == MRC_PARSER_ERROR && d->message) {
        fprintf(stderr, "%s:%d:%d: %s\n", fn, (int)d->line, (int)d->column,
                d->message);
      }
    }
  }
#endif
  return p;
}

#ifndef MRB_NO_STDIO
static char*
read_file_content(mrb_state *mrb, FILE *f, size_t *lenp)
{
  size_t capa = 4096;
  size_t len = 0;
  char *buf = (char*)mrb_malloc(mrb, capa + 1);

  while (!feof(f)) {
    size_t n;
    if (len == capa) {
      capa *= 2;
      buf = (char*)mrb_realloc(mrb, buf, capa + 1);
    }
    n = fread(buf + len, 1, capa - len, f);
    len += n;
    if (ferror(f)) {
      mrb_free(mrb, buf);
      return NULL;
    }
  }
  buf[len] = '\0';
  *lenp = len;
  return buf;
}
#endif

MRB_API mrb_ccontext*
mrb_ccontext_new(mrb_state *mrb)
{
  return (mrb_ccontext*)mrb_calloc(mrb, 1, sizeof(mrb_ccontext));
}

MRB_API void
mrb_ccontext_free(mrb_state *mrb, mrb_ccontext *cxt)
{
  if (!cxt) return;
  if (cxt->syms) mrb_free(mrb, cxt->syms);
  if (cxt->filename) mrb_free(mrb, cxt->filename);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrb_ccontext_filename(mrb_state *mrb, mrb_ccontext *c, const char *s)
{
  size_t len;
  char *filename;

  if (!c) return NULL;
  if (!s) return c->filename;
  len = strlen(s);
  filename = (char*)mrb_malloc(mrb, len + 1);
  memcpy(filename, s, len + 1);
  if (c->filename) mrb_free(mrb, c->filename);
  c->filename = filename;
  return c->filename;
}

MRB_API void
mrb_ccontext_partial_hook(mrb_ccontext *c, int (*partial_hook)(struct mrb_parser_state*), void *data)
{
  if (!c) return;
  c->partial_hook = partial_hook;
  c->partial_data = data;
}

MRB_API void
mrb_ccontext_cleanup_local_variables(mrb_ccontext *c)
{
  if (!c) return;
  /* Forget the locals accumulated so far so they do not leak into the next
     compilation unit (e.g. a script run after a -r required file). The syms
     buffer itself is kept; it is reused by the next compile's realloc or
     freed by mrb_ccontext_free(). copy_context_to_mrc() only propagates
     locals when slen > 0, so clearing slen is enough to stop the leak. */
  c->slen = 0;
  c->keep_lv = FALSE;
}

MRB_API struct mrb_parser_state*
mrb_parser_new(mrb_state *mrb)
{
  return parser_alloc(mrb, NULL);
}

MRB_API void
mrb_parser_free(struct mrb_parser_state *p)
{
  mrb_state *mrb;
  mrc_ccontext *mc;

  if (!p) return;
  mrb = p->mrb;
  mc = (mrc_ccontext*)p->ylval;
  if (mc && p->tree) {
    mrc_irep_free(mc, (mrc_irep*)p->tree);
  }
  if (mc) {
    mrc_ccontext_free(mc);
  }
  free_parser_messages(mrb, p->error_buffer, sizeof(p->error_buffer) / sizeof(p->error_buffer[0]));
  free_parser_messages(mrb, p->warn_buffer, sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0]));
  mrb_free(mrb, p);
}

MRB_API void
mrb_parser_parse(struct mrb_parser_state *p, mrb_ccontext *c)
{
  struct mrb_parser_state *parsed;
  size_t len;

  if (!p || !p->s || p->tree || p->nerr) return;
  len = (size_t)(p->send - p->s);
  parsed = parse_source(p->mrb, p->s, len, c);
  p->tree = parsed->tree;
  p->ylval = parsed->ylval;
  p->nerr = parsed->nerr;
  p->nwarn = parsed->nwarn;
  memcpy(p->error_buffer, parsed->error_buffer, sizeof(p->error_buffer));
  memcpy(p->warn_buffer, parsed->warn_buffer, sizeof(p->warn_buffer));
  parsed->tree = NULL;
  parsed->ylval = NULL;
  memset(parsed->error_buffer, 0, sizeof(parsed->error_buffer));
  memset(parsed->warn_buffer, 0, sizeof(parsed->warn_buffer));
  mrb_parser_free(parsed);
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, char const *filename)
{
  if (!p || !filename) return;
  p->filename_sym = mrb_intern_cstr(p->mrb, filename);
  if (p->cxt) {
    mrb_ccontext_filename(p->mrb, p->cxt, filename);
  }
}

MRB_API mrb_sym
mrb_parser_get_filename(struct mrb_parser_state *p, uint16_t idx)
{
  (void)idx;
  return p ? p->filename_sym : 0;
}

#ifndef MRB_NO_STDIO
MRB_API struct mrb_parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrb_ccontext *c)
{
  char *buf;
  size_t len = 0;
  struct mrb_parser_state *p;

  if (!f) return NULL;
  buf = read_file_content(mrb, f, &len);
  if (!buf) return NULL;
  p = parse_source(mrb, buf, len, c);
  mrb_free(mrb, buf);
  return p;
}
#endif

MRB_API struct mrb_parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrb_ccontext *c)
{
  if (!s) return NULL;
  return parse_source(mrb, s, len, c);
}

MRB_API struct mrb_parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrb_ccontext *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API struct RProc*
mrb_generate_code(mrb_state *mrb, struct mrb_parser_state *p)
{
  mrc_ccontext *mc;
  mrc_irep *irep;
  mrb_irep *mir;
  struct RProc *proc;
  uint8_t *bin = NULL;
  size_t bin_size = 0;
  /* Always carry debug info across the dump/reload that turns the mrc_irep
     into an mrb_irep: without it runtime backtraces lose the file name and
     line number, and mruby reports those even when compiled without -g. */
  uint8_t flags = MRC_COMPAT_DUMP_DEBUG_INFO;

  if (!p || !p->tree || p->nerr) return NULL;
  mc = (mrc_ccontext*)p->ylval;
  irep = (mrc_irep*)p->tree;
  if (mrc_dump_irep(mc, irep, flags, &bin, &bin_size) != MRC_COMPAT_DUMP_OK) {
    return NULL;
  }
  mir = mrb_read_irep_buf(mrb, bin, bin_size);
  mrc_free(mc, bin);
  if (!mir) {
    return NULL;
  }
  proc = mrb_proc_new(mrb, mir);
  mrb_irep_decref(mrb, mir);
  proc->c = NULL;
  proc->upper = p->upper;
  mrc_irep_free(mc, irep);
  p->tree = NULL;
  return proc;
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrb_ccontext *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (c) c->parser_nerr = p->nerr;
    if (mrb->exc == NULL) {
      const char *message = "syntax error";
      if (p->error_buffer[0].message) {
        message = p->error_buffer[0].message;
      }
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, message, strlen(message)));
    }
    mrb_parser_free(p);
    return mrb_undef_value();
  }

  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (!proc) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    }
    return mrb_undef_value();
  }

  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) target = c->target_class;
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  MRB_PROC_SET_TARGET_CLASS(proc, target);
  if (mrb->c->ci) {
    mrb_vm_ci_target_class_set(mrb->c->ci, target);
  }
  return mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
}

#ifndef MRB_NO_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrb_ccontext *c)
{
  return mrb_load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}

MRB_API mrb_value
mrb_load_detect_file_cxt(mrb_state *mrb, FILE *fp, mrb_ccontext *c)
{
  char *buf;
  size_t len = 0;
  mrb_value result;

  if (!fp) return mrb_nil_value();
  buf = read_file_content(mrb, fp, &len);
  if (!buf) return mrb_nil_value();
  if (len >= 4 && memcmp(buf, "RITE", 4) == 0) {
    result = mrb_load_irep_buf_cxt(mrb, buf, len, c);
  }
  else {
    result = mrb_load_nstring_cxt(mrb, buf, len, c);
  }
  mrb_free(mrb, buf);
  return result;
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrb_ccontext *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrb_ccontext *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

void
mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user)
{
  mrc_ccontext *mc;
  mrc_irep *irep;
  uint16_t i;

  if (!p || !p->tree || !func) return;
  mc = (mrc_ccontext*)p->ylval;
  irep = (mrc_irep*)p->tree;
  if (!mc || !irep || !irep->lv) return;
  for (i = 0; i + 1 < irep->nlocals; i++) {
    mrc_int len = 0;
    const char *name = mrc_sym_name_len(mc, irep->lv[i], &len);
    if (name && len > 0) {
      mrb_sym sym = mrb_intern(mrb, name, (size_t)len);
      if (!func(mrb, sym, user)) return;
    }
  }
}

#endif
