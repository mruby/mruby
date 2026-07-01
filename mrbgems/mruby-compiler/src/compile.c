#include "../include/mrc_parser_util.h"
#include "../include/mrc_irep.h"
#include "../include/mrc_ccontext.h"
#include "../include/mrc_codegen.h"
#include "../include/mrc_dump.h"
#include "../include/mrc_codedump.h"
#include "../include/mrc_opcode.h"
#include "../include/mrc_presym.h"
#include "../include/mrc_diagnostic.h"

#if defined(MRC_TARGET_MRUBY)
#include "../include/mrc_proc.h"
#endif

static mrc_irep *
mrc_load_exec(mrc_ccontext *c, mrc_node *ast)
{
  mrc_irep *irep;
  /* parse error */
  if (0 < c->p->error_list.size) {
    pm_diagnostic_t *e = (pm_diagnostic_t *)c->p->error_list.head;
    while (e) {
      mrc_diagnostic_list_append(c, e->location.start, e->message, MRC_PARSER_ERROR);
      e = (pm_diagnostic_t *)e->node.next;
    }
    return NULL;
  }
  /* parse warning */
  if (0 < c->p->warning_list.size) {
    pm_diagnostic_t *w = (pm_diagnostic_t *)c->p->warning_list.head;
    while (w) {
      mrc_diagnostic_list_append(c, w->location.start, w->message, MRC_PARSER_WARNING);
      w = (pm_diagnostic_t *)w->node.next;
    }
  }
#if defined(MRC_DUMP_PRETTY) && !defined(MRC_NO_STDIO)
  if (c->dump_result) {
    pm_buffer_t buffer = { 0 };
    pm_prettyprint(&buffer, c->p, ast);
    fprintf(stderr, "%s\n", buffer.value);
    pm_buffer_free(&buffer);
  }
#endif
  irep = mrc_generate_code(c, ast);
  if (c->capture_errors) {
    return NULL;
  }
  if (c->dump_result) {
    mrc_codedump_all(c, irep);
  }

  return irep;
}

static void
partial_hook(void *data, pm_parser_t *p, pm_token_t *token)
{
  mrc_ccontext *c = (mrc_ccontext *)data;
  if (c->current_filename_index + 1 == c->filename_table_length) {
    return;
  }
  uint32_t token_pos = (uint32_t)(token->start - p->start);
  if (token_pos < c->filename_table[c->current_filename_index].start) {
    return;
  }
  if (c->filename_table[c->current_filename_index + 1].start <= token_pos) {
    c->current_filename_index++;
    pm_string_t filename_string;
    pm_string_constant_init(
        &filename_string,
        c->filename_table[c->current_filename_index].filename,
        strlen(c->filename_table[c->current_filename_index].filename));
    p->filepath = filename_string;
  }
}

#if defined(MRC_TARGET_MRUBY)
#define MRC_PROC_CFUNC_FL 128
#define MRC_PROC_CFUNC_P(p) (((p)->flags & MRC_PROC_CFUNC_FL) != 0)

static mrc_bool
mrc_mruby_lvspace_proc_p(const struct RProc *proc)
{
  const struct mrc_irep *irep;

  if (proc == NULL || MRC_PROC_CFUNC_P(proc) || proc->upper == NULL) {
    return FALSE;
  }
  irep = proc->body.irep;
  return irep && irep->lv == NULL && irep->nlocals == 1;
}

static size_t
mrc_mruby_irep_local_count(mrb_state *mrb, const struct mrc_irep *irep)
{
  size_t count = 0;

  if (irep && irep->lv) {
    size_t lv_count = irep->nlocals > 0 ? irep->nlocals - 1 : 0;
    for (size_t i = 0; i < lv_count; i++) {
      if (mrb_sym_name(mrb, irep->lv[i])) {
        count++;
      }
    }
  }
  return count;
}

static void
mrc_mruby_options_scope_local_init(mrc_ccontext *cc, pm_string_t *local, mrc_sym sym)
{
  const char *name = mrb_sym_name(cc->mrb, sym);
  size_t length;
  uint8_t *copy;

  if (!name) return;
  length = strlen(name);
  copy = (uint8_t *)mrc_malloc(cc, length);
  memcpy(copy, name, length);
  pm_string_constant_init(local, (const char *)copy, length);
}

static void
mrc_pm_options_init(mrc_ccontext *cc)
{
  if (cc->options) return;
  if (cc->upper == NULL) return;

  struct RProc *u;

  pm_options_t *options = (pm_options_t *)mrc_calloc(cc, 1, sizeof(pm_options_t));
  pm_string_t *encoding = &options->encoding;
  pm_string_constant_init(encoding, "UTF-8", 5);

  size_t scopes_count = 0;
  for (u = (struct RProc *)cc->upper; u && !MRC_PROC_CFUNC_P(u); u = (struct RProc *)u->upper) {
    if (!mrc_mruby_lvspace_proc_p(u)) {
      scopes_count++;
    }
  }

  pm_options_scopes_init(options, scopes_count + 1); // Prism requires one more scope

  u = (struct RProc *)cc->upper;
  pm_options_scope_t *scope;
  size_t scope_index = scopes_count;
  for (; u && !MRC_PROC_CFUNC_P(u); u = (struct RProc *)u->upper) {
    if (mrc_mruby_lvspace_proc_p(u)) {
      continue;
    }
    const struct mrc_irep *ir = u->body.irep;
    size_t lv_count = ir->nlocals > 0 ? ir->nlocals - 1 : 0;
    const mrc_sym *v = ir->lv;
    size_t locals_count = mrc_mruby_irep_local_count(cc->mrb, ir);

    scope = &options->scopes[--scope_index];
    pm_options_scope_init(scope, locals_count);
    if (v) {
      const char *name;
      size_t local_index = 0;
      for (size_t j = 0; j < lv_count; j++) {
        name = mrb_sym_name(cc->mrb, v[j]);
        if (name) {
          mrc_mruby_options_scope_local_init(cc, &scope->locals[local_index++], v[j]);
        }
      }
    }
  }

  cc->options = options;
}
#endif

static void
mrc_pm_parser_init(mrc_parser_state *p, uint8_t **source, size_t size, mrc_ccontext *cc)
{
  pm_lex_callback_t *cb = (pm_lex_callback_t *)mrc_malloc(cc, sizeof(pm_lex_callback_t));
  cb->data = cc;
  cb->callback = partial_hook;
#if defined(MRC_TARGET_MRUBY)
  mrc_pm_options_init(cc);
#endif
  pm_parser_init(p, *source, size, cc->options);
  p->lex_callback = cb;
  mrc_init_presym(&p->constant_pool);
  if (cc->filename_table) {
    pm_string_t filename_string;
    pm_string_constant_init(&filename_string, cc->filename_table[0].filename,
                                             strlen(cc->filename_table[0].filename));
    p->filepath = filename_string;
  }
}

#ifndef MRC_NO_STDIO

#define INITIAL_BUF_SIZE 1024
static ssize_t
append_from_stdin(mrc_ccontext *c, uint8_t **source, size_t source_length)
{
  uint8_t *buffer = (uint8_t *)mrc_malloc(c, INITIAL_BUF_SIZE);
  if (buffer == NULL) return -1;

  int capacity = INITIAL_BUF_SIZE;
  size_t length = 0;

  while (1) {
    int ch = getchar();
    if (ch == EOF) {
      buffer[length] = '\0';
      if (*source == NULL)
        *source = (uint8_t *)mrc_malloc(c, source_length + length + 1);
      else
        *source = (uint8_t *)mrc_realloc(c, *source, source_length + length + 1);
      memccpy(*source + source_length, buffer, 1, length);
      mrc_free(c, buffer);
      return length;
    }

    buffer[length++] = (uint8_t)ch;

    if (capacity <= length) {
      capacity *= 2;
      uint8_t *new_buffer = (uint8_t *)mrc_realloc(c, buffer, capacity);
      if (new_buffer == NULL) {
        mrc_free(c, buffer);
        return -1;
      }
      buffer = new_buffer;
    }
  }
}

static ssize_t
read_input_files(mrc_ccontext *c, const char **filenames, uint8_t **source, mrc_filename_table *filename_table)
{
  int i = 0;
  size_t pos = 0;
  ssize_t length = 0;
  ssize_t each_size;
  FILE *file;
  const char *filename = filenames[0];
  while (filename) {
    if (i > 0) {
      /* Separate files with a newline so that a file without a trailing
         newline does not merge its last token with the first token of the
         next file (e.g. `end` + `module` becoming `endmodule`). The separator
         precedes the file content, so filename_table[i].start still points at
         the content and the filename/line mapping is unaffected. See #6907. */
      length += 1;
      if (*source == NULL) {
        *source = (uint8_t *)mrc_malloc(c, length + 1);
      }
      else {
        *source = (uint8_t *)mrc_realloc(c, *source, length + 1);
      }
      (*source)[pos++] = '\n';
      (*source)[length] = '\0';
    }
    filename_table[i].filename = filenames[i];
    filename_table[i].start = pos;
    if (filename[0] == '-' && filename[1] == '\0') {
      each_size = append_from_stdin(c, source, length);
      if (each_size < 0) {
        fprintf(stderr, "compile.c: cannot read from stdin\n");
        return -1;
      }
      length += each_size;
    }
    else {
      file = NULL;
      file = fopen(filename, "rb");
      if (!file) {
        fprintf(stderr, "compile.c: cannot open program file. (%s)\n", filename);
        return -1;
      }
      fseek(file, 0, SEEK_END);
      each_size = ftell(file);
      fseek(file, 0, SEEK_SET);
      length += each_size;
      if (*source == NULL) {
        *source = (uint8_t *)mrc_malloc(c, length + 1);
      }
      else {
        *source = (uint8_t *)mrc_realloc(c, *source, length + 1);
      }
      if (fread(*source + pos, sizeof(char), each_size, file) != each_size) {
        fprintf(stderr, "compile.c: cannot read program file. (%s)\n", filename);
        fclose(file);
        return -1;
      }
      fclose(file);
      (*source)[length] = '\0';
    }
    pos += each_size;
    filename = filenames[++i];
  }
  return length;
}

static mrc_node *
mrc_pm_parse(mrc_ccontext *cc)
{
  mrc_node *node = pm_parse(cc->p);

#if defined(PICORB_VM_MRUBYC)
  // Workaround: save top-level locals for PicoRuby(mruby/c) IRB
  pm_program_node_t *program = (pm_program_node_t *)node;
  uint32_t nlocals = program->locals.size;
  pm_options_t *options = (pm_options_t *)mrc_malloc(cc, sizeof(pm_options_t));
  memset(options, 0, sizeof(pm_options_t));
  pm_string_t *encoding = &options->encoding;
  pm_string_constant_init(encoding, "UTF-8", 5);
  pm_options_scopes_init(options, 1);
  pm_options_scope_t *options_scope = &options->scopes[0];
  pm_options_scope_init(options_scope, nlocals);
  pm_constant_id_t id;
  pm_constant_t *local;
  pm_string_t *scope_local;
  char *allocated;
  for (int i = 0; i < nlocals; i++) {
    scope_local = &options_scope->locals[i];
    id = program->locals.ids[i];
    local = pm_constant_pool_id_to_constant(&cc->p->constant_pool, id);
    allocated = (char *)mrc_malloc(cc, local->length);
    memcpy(allocated, local->start, local->length);
    pm_string_constant_init(scope_local, (const char *)allocated, local->length);
  }
  if (cc->options && cc->options->scopes) {
    for (int i = 0; i < cc->options->scopes[0].locals_count; i++) {
      mrc_free(cc, (void *)cc->options->scopes[0].locals[i].source);
    }
    mrc_free(cc, cc->options);
  }
  cc->options = options;
#endif

  return node;
}


static mrc_node *
mrc_parse_file_cxt(mrc_ccontext *c, const char **filenames, uint8_t **source)
{
  size_t filecount = 0;
  while (filenames[filecount]) {
    filecount++;
  }
  c->filename_table = (mrc_filename_table *)mrc_malloc(c, sizeof(mrc_filename_table) * filecount);
  c->filename_table_length = filecount;
  c->current_filename_index = 0;
  ssize_t length = read_input_files(c, filenames, source, c->filename_table);
  if (length < 0) {
    fprintf(stderr, "Cannot open files: ");
    for (size_t i = 0; i < filecount; i++) {
      fprintf(stderr, "%s ", filenames[i]);
    }
    fprintf(stderr, "\n");
    return NULL;
  }
  mrc_pm_parser_init(c->p, source, length, c);
  return mrc_pm_parse(c);
}

MRC_API mrc_irep *
mrc_load_file_cxt(mrc_ccontext *c, const char **filenames, uint8_t **source)
{
  mrc_node *root = mrc_parse_file_cxt(c, filenames, source);
  if (root == NULL) {
    return NULL;
  }
  mrc_irep *irep = mrc_load_exec(c, root);
  pm_node_destroy(c->p, root);
  return irep;
}
#endif

static mrc_node *
mrc_parse_string_cxt(mrc_ccontext *c, const uint8_t **source, size_t length)
{
  c->filename_table = (mrc_filename_table *)mrc_malloc(c, sizeof(mrc_filename_table));
  c->filename_table[0].filename = c->filename ? c->filename : "-e";
  c->filename_table[0].start = 0;
  c->filename_table_length = 1;
  c->current_filename_index = 0;
  mrc_pm_parser_init(c->p, (uint8_t **)source, length, c);
  return mrc_pm_parse(c);
}

MRC_API mrc_irep *
mrc_load_string_cxt(mrc_ccontext *c, const uint8_t **source, size_t length)
{
  mrc_node *root = mrc_parse_string_cxt(c, source, length);
  mrc_irep *irep = mrc_load_exec(c, root);
  pm_node_destroy(c->p, root);
  return irep;
}

#if defined(MRC_TARGET_MRUBY)

MRC_API void
mrb_mruby_compiler2_gem_init(mrb_state *mrb)
{
}

MRC_API void
mrb_mruby_compiler2_gem_final(mrb_state *mrb)
{
}

MRC_API void
mrb_mruby_compiler_gem_init(mrb_state *mrb)
{
}

MRC_API void
mrb_mruby_compiler_gem_final(mrb_state *mrb)
{
}

#endif
