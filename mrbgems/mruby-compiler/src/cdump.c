/*
** cdump.c - mruby binary dumper (in C)
**
** See Copyright Notice in mruby.h
*/

#include <stdio.h>
#include <string.h>
#include "../include/mrc_ccontext.h"
#include "../include/mrc_irep.h"
#include "../include/mrc_dump.h"
#include "../include/mrc_debug.h"
#include "../include/mrc_irep_pool_type.h"

#ifndef MRC_NO_STDIO

#ifndef MRC_NO_FLOAT
//#include <../include/endian.h>
#define MRC_FLOAT_FMT "%.17g"
#endif

#define ISALPHA(c) ((((unsigned)(c) | 0x20) - 'a') < 26)
#define ISDIGIT(c) (((unsigned)(c) - '0') < 10)
#define ISALNUM(c) (ISALPHA(c) || ISDIGIT(c))

typedef struct mrc_string {
  char *ptr;
  size_t len;
  size_t capa;
} mrc_string;

static mrc_string*
mrc_str_new_capa(mrc_ccontext *c, size_t capa)
{
  mrc_string *s = (mrc_string *)mrc_malloc(c, sizeof(mrc_string));
  if (s) {
    s->ptr = (char *)mrc_calloc(c, 1, capa);
    if (s->ptr) {
      s->len = 0;
      s->capa = capa;
      return s;
    }
    mrc_free(c, s);
  }
  return NULL;
}

static mrc_string*
mrc_str_new(mrc_ccontext *c, const char *ptr, size_t len)
{
  mrc_string *s = mrc_str_new_capa(c, len+1);
  if (s) {
    memcpy(s->ptr, ptr, len);
    s->len = len;
    s->ptr[len] = '\0';
  }
  return s;
}

static mrc_string*
mrc_str_new_cstr(mrc_ccontext *c, const char *cstr)
{
  return mrc_str_new(c, cstr, strlen(cstr));
}

static void
mrc_str_cat_lit(mrc_ccontext *c, mrc_string *s, const char *lit)
{
  size_t len = strlen(lit);
  if (s->len+len+1 > s->capa) {
    s->capa = s->len+len+1;
    s->ptr = (char *)mrc_realloc(c, s->ptr, s->capa);
  }
  memcpy(s->ptr+s->len, lit, len);
  s->len += len;
  s->ptr[s->len] = '\0';
}

static void
mrc_str_cat_cstr(mrc_ccontext *c, mrc_string *s, const char *cstr)
{
  if (!cstr) return;
  mrc_str_cat_lit(c, s, cstr);
}

static void
mrc_str_cat_str(mrc_ccontext *c, mrc_string *s, mrc_string *s2)
{
  if (s->len+s2->len+1 > s->capa) {
    s->capa = s->len+s2->len+1;
    s->ptr = (char *)mrc_realloc(c, s->ptr, s->capa);
  }
  memcpy(s->ptr+s->len, s2->ptr, s2->len);
  s->len += s2->len;
  s->ptr[s->len] = '\0';
}

static void
mrc_str_free(mrc_ccontext *c, mrc_string *s)
{
  mrc_free(c, s->ptr);
  mrc_free(c, s);
}

static mrc_string*
mrc_str_escape(mrc_ccontext *c, mrc_string *s)
{
  mrc_string *s2 = mrc_str_new_capa(c, s->len*2+1);
  mrc_str_cat_lit(c, s2, "\"");
  if (s2) {
    for (size_t i=0; i<s->len; i++) {
      char ch[2] = {s->ptr[i], '\0'};
      if (ch[0] == '"' || ch[0] == '\\') {
        mrc_str_cat_lit(c, s2, "\\");
      }
      mrc_str_cat_lit(c, s2, ch);
    }
  }
  mrc_str_cat_lit(c, s2, "\"");
  return s2;
}

#define MRC_STRING_PTR(s) ((s)->ptr)
#define MRC_STRING_LEN(s) ((s)->len)

static int
cdump_pool(mrc_ccontext *c, const mrc_pool_value *p, FILE *fp)
{
  if (p->tt & IREP_TT_NFLAG) {  /* number */
    switch (p->tt) {
#ifdef MRC_64BIT
    case IREP_TT_INT64:
      if (p->u.i64 < INT32_MIN || INT32_MAX < p->u.i64) {
        fprintf(fp, "{IREP_TT_INT64, {.i64=%" PRId64 "}},\n", p->u.i64);
      }
      else {
        fprintf(fp, "{IREP_TT_INT32, {.i32=%" PRId32 "}},\n", (int32_t)p->u.i64);
      }
      break;
#endif
    case IREP_TT_INT32:
      fprintf(fp, "{IREP_TT_INT32, {.i32=%" PRId32 "}},\n", p->u.i32);
      break;
    case IREP_TT_FLOAT:
#ifndef MRC_NO_FLOAT
      fprintf(fp, "{IREP_TT_FLOAT, {.f=" MRC_FLOAT_FMT "}},\n", p->u.f);
#endif
      break;
    case IREP_TT_BIGINT:
      {
        const char *s = p->u.str;
        int len = s[0]+2;
        fputs("{IREP_TT_BIGINT, {\"", fp);
        for (int i=0; i<len; i++) {
          fprintf(fp, "\\x%02x", (int)s[i]&0xff);
        }
        fputs("\"}},\n", fp);
      }
      break;
    }
  }
  else {                        /* string */
    int i, len = p->tt>>2;
    const char *s = p->u.str;
    fprintf(fp, "{IREP_TT_STR|(%d<<2), {\"", len);
    for (i=0; i<len; i++) {
      fprintf(fp, "\\x%02x", (int)s[i]&0xff);
    }
    fputs("\"}},\n", fp);
  }
  return MRC_DUMP_OK;
}

static mrc_bool
sym_name_word_p(const char *name, mrc_int len)
{
  if (len == 0) return FALSE;
  if (name[0] != '_' && !ISALPHA(name[0])) return FALSE;
  for (int i = 1; i < len; i++) {
    if (name[i] != '_' && !ISALNUM(name[i])) return FALSE;
  }
  return TRUE;
}

static mrc_bool
sym_name_with_equal_p(const char *name, mrc_int len)
{
  return len >= 2 && name[len-1] == '=' && sym_name_word_p(name, len-1);
}

static mrc_bool
sym_name_with_question_mark_p(const char *name, mrc_int len)
{
  return len >= 2 && name[len-1] == '?' && sym_name_word_p(name, len-1);
}

static mrc_bool
sym_name_with_bang_p(const char *name, mrc_int len)
{
  return len >= 2 && name[len-1] == '!' && sym_name_word_p(name, len-1);
}

static mrc_bool
sym_name_ivar_p(const char *name, mrc_int len)
{
  return len >= 2 && name[0] == '@' && sym_name_word_p(name+1, len-1);
}

static mrc_bool
sym_name_cvar_p(const char *name, mrc_int len)
{
  return len >= 3 && name[0] == '@' && sym_name_ivar_p(name+1, len-1);
}

#define OPERATOR_SYMBOL(sym_name, name) {name, sym_name, sizeof(sym_name)-1}
struct operator_symbol {
  const char *name;
  const char *sym_name;
  uint16_t sym_name_len;
};
static const struct operator_symbol operator_table[] = {
  OPERATOR_SYMBOL("!", "not"),
  OPERATOR_SYMBOL("%", "mod"),
  OPERATOR_SYMBOL("&", "and"),
  OPERATOR_SYMBOL("*", "mul"),
  OPERATOR_SYMBOL("+", "add"),
  OPERATOR_SYMBOL("-", "sub"),
  OPERATOR_SYMBOL("/", "div"),
  OPERATOR_SYMBOL("<", "lt"),
  OPERATOR_SYMBOL(">", "gt"),
  OPERATOR_SYMBOL("^", "xor"),
  OPERATOR_SYMBOL("`", "tick"),
  OPERATOR_SYMBOL("|", "or"),
  OPERATOR_SYMBOL("~", "neg"),
  OPERATOR_SYMBOL("!=", "neq"),
  OPERATOR_SYMBOL("!~", "nmatch"),
  OPERATOR_SYMBOL("&&", "andand"),
  OPERATOR_SYMBOL("**", "pow"),
  OPERATOR_SYMBOL("+@", "plus"),
  OPERATOR_SYMBOL("-@", "minus"),
  OPERATOR_SYMBOL("<<", "lshift"),
  OPERATOR_SYMBOL("<=", "le"),
  OPERATOR_SYMBOL("==", "eq"),
  OPERATOR_SYMBOL("=~", "match"),
  OPERATOR_SYMBOL(">=", "ge"),
  OPERATOR_SYMBOL(">>", "rshift"),
  OPERATOR_SYMBOL("[]", "aref"),
  OPERATOR_SYMBOL("||", "oror"),
  OPERATOR_SYMBOL("<=>", "cmp"),
  OPERATOR_SYMBOL("===", "eqq"),
  OPERATOR_SYMBOL("[]=", "aset"),
};

static const char*
sym_operator_name(const char *sym_name, mrc_int len)
{
  mrc_sym table_size = sizeof(operator_table)/sizeof(struct operator_symbol);
  if (operator_table[table_size-1].sym_name_len < len) return NULL;

  for (mrc_sym start = 0; table_size != 0; table_size/=2) {
    mrc_sym idx = start+table_size/2;
    const struct operator_symbol *op_sym = &operator_table[idx];
    int cmp = (int)len-(int)op_sym->sym_name_len;
    if (cmp == 0) {
      cmp = memcmp(sym_name, op_sym->sym_name, len);
      if (cmp == 0) return op_sym->name;
    }
    if (0 < cmp) {
      start = ++idx;
      table_size--;
    }
  }
  return NULL;
}

static mrc_string*
sym_var_name_str(mrc_ccontext *c, const char *initname, const char *key, int n)
{
  char buf[32];
  mrc_string *s = mrc_str_new_cstr(c, initname);
  mrc_str_cat_lit(c, s, "_");
  mrc_str_cat_cstr(c, s, key);
  mrc_str_cat_lit(c, s, "_");
  snprintf(buf, sizeof(buf), "%d", n);
  mrc_str_cat_cstr(c, s, buf);
  return s;
}

static int
cdump_sym(mrc_ccontext *c, mrc_sym sym, const char *var_name, int idx, mrc_string *init_syms_code, FILE *fp)
{
  if (sym == 0) {
    fputs("0,", fp);
    return MRC_DUMP_OK;
  }

  const pm_constant_t *constant = pm_constant_pool_id_to_constant(&c->p->constant_pool, sym);
  mrc_string *name_obj = mrc_str_new(c, (const char *)constant->start, constant->length);
  const char *name = MRC_STRING_PTR(name_obj);
  const char *op_name;
  mrc_int len = constant->length;

  if (*name == '\0') {
    mrc_str_free(c, name_obj);
    return MRC_DUMP_INVALID_ARGUMENT;
  }
  if (sym_name_word_p(name, len)) {
    fprintf(fp, "MRB_SYM(%s)", name);
  }
  else if (sym_name_with_equal_p(name, len)) {
    fprintf(fp, "MRB_SYM_E(%.*s)", (int)(len-1), name);
  }
  else if (sym_name_with_question_mark_p(name, len)) {
    fprintf(fp, "MRB_SYM_Q(%.*s)", (int)(len-1), name);
  }
  else if (sym_name_with_bang_p(name, len)) {
    fprintf(fp, "MRB_SYM_B(%.*s)", (int)(len-1), name);
  }
  else if (sym_name_ivar_p(name, len)) {
    fprintf(fp, "MRB_IVSYM(%s)", name+1);
  }
  else if (sym_name_cvar_p(name, len)) {
    fprintf(fp, "MRB_CVSYM(%s)", name+2);
  }
  else if ((op_name = sym_operator_name(name, len))) {
    fprintf(fp, "MRB_OPSYM(%s)", op_name);
  }
  else {
    char buf[32];
    mrc_string *name_obj = mrc_str_new(c, name, len);
    mrc_str_cat_lit(c, init_syms_code, "  ");
    mrc_str_cat_cstr(c, init_syms_code, var_name);
    snprintf(buf, sizeof(buf), "[%d] = ", idx);
    mrc_str_cat_cstr(c, init_syms_code, buf);
    mrc_str_cat_lit(c, init_syms_code, "mrb_intern_lit(mrb, ");
    mrc_string *escaped = mrc_str_escape(c, name_obj);
    mrc_str_free(c, name_obj);
    mrc_str_cat_str(c, init_syms_code, escaped);
    mrc_str_free(c, escaped);
    mrc_str_cat_lit(c, init_syms_code, ");\n");
    fputs("0", fp);
  }
  fputs(", ", fp);
  mrc_str_free(c, name_obj);
  return MRC_DUMP_OK;
}

static int
cdump_syms(mrc_ccontext *c, const char *name, const char *key, int n, int syms_len, const mrc_sym *syms, mrc_string *init_syms_code, FILE *fp)
{
  int ai = mrc_gc_arena_save(c);
  mrc_int code_len = MRC_STRING_LEN(init_syms_code);
  mrc_string *var_name = sym_var_name_str(c, name, key, n);

  fprintf(fp, "mrb_DEFINE_SYMS_VAR(%s, %d, (", MRC_STRING_PTR(var_name), syms_len);
  for (int i=0; i<syms_len; i++) {
    cdump_sym(c, syms[i], MRC_STRING_PTR(var_name), i, init_syms_code, fp);
  }
  mrc_str_free(c, var_name);
  fputs("), ", fp);
  if (code_len == MRC_STRING_LEN(init_syms_code)) fputs("const", fp);
  fputs(");\n", fp);
  mrc_gc_arena_restore(c, ai);
  return MRC_DUMP_OK;
}

//Handle the simple/common case of debug_info:
// - 1 file associated with a single irep
// - mrc_debug_line_ary format only
static int
simple_debug_info(mrc_irep_debug_info *info)
{
  if (!info || info->flen != 1) {
    return 0;
  }
  return 1;
}

//Adds debug information to c-structs and
//adds filenames in init_syms_code block
static int
cdump_debug(mrc_ccontext *c, const char *name, int n, mrc_irep_debug_info *info,
            mrc_string *init_syms_code, FILE *fp)
{
  int ai = mrc_gc_arena_save(c);
  char buffer[256];
  const char *line_type = "mrb_debug_line_ary";

  if (!simple_debug_info(info))
    return MRC_DUMP_INVALID_IREP;

  int len = info->files[0]->line_entry_count;

  const pm_constant_t *fn_constant = pm_constant_pool_id_to_constant(&c->p->constant_pool, info->files[0]->filename_sym);
  const char *filename = (const char *)fn_constant->start;
  snprintf(buffer, sizeof(buffer), "  %s_debug_file_%d.filename_sym = mrb_intern_lit(mrb,", name, n);
  mrc_str_cat_cstr(c, init_syms_code, buffer);
  mrc_string *filename_str = mrc_str_new_cstr(c, filename);
  mrc_string *escaped = mrc_str_escape(c, filename_str);
  mrc_str_free(c, filename_str);
  mrc_str_cat_str(c, init_syms_code, escaped);
  mrc_str_free(c, escaped);
  mrc_str_cat_cstr(c, init_syms_code, ");\n");

  switch (info->files[0]->line_type) {
  case mrc_debug_line_ary:
    fprintf(fp, "static uint16_t %s_debug_lines_%d[%d] = {", name, n, len);
    for (int i=0; i<len; i++) {
      if (i%10 == 0) fputs("\n", fp);
      fprintf(fp, "0x%04x,", info->files[0]->lines.ary[i]);
    }
    fputs("};\n", fp);
    break;

  case mrc_debug_line_flat_map:
    line_type = "mrb_debug_line_flat_map";
    fprintf(fp, "static struct mrb_irep_debug_info_line %s_debug_lines_%d[%d] = {", name, n, len);
    for (int i=0; i<len; i++) {
      const mrc_irep_debug_info_line *fmap = &info->files[0]->lines.flat_map[i];
      fprintf(fp, "\t{.start_pos=0x%04x,.line=%d},\n", fmap->start_pos, fmap->line);
    }
    fputs("};\n", fp);
    break;

  case mrc_debug_line_packed_map:
    line_type = "mrb_debug_line_packed_map";
    fprintf(fp, "static const char %s_debug_lines_%d[] = \"", name, n);
    const uint8_t *pmap = info->files[0]->lines.packed_map;
    for (int i=0; i<len; i++) {
      fprintf(fp, "\\x%02x", pmap[i]&0xff);
    }
    fputs("\";\n", fp);
    break;
  }
  fprintf(fp, "static mrb_irep_debug_info_file %s_debug_file_%d = {\n", name, n);
  fprintf(fp, "%d, %d, %d, %s, {%s_debug_lines_%d}};\n",
          info->files[0]->start_pos,
          info->files[0]->filename_sym,
          info->files[0]->line_entry_count,
          line_type,
          name, n);
  fprintf(fp, "static mrb_irep_debug_info_file *%s_debug_file_%d_ = &%s_debug_file_%d;\n", name, n, name, n);

  fprintf(fp, "static mrb_irep_debug_info %s_debug_%d = {\n", name, n);
  fprintf(fp, "%d, %d, &%s_debug_file_%d_};\n", info->pc_count, info->flen, name, n);

  mrc_gc_arena_restore(c, ai);
  return MRC_DUMP_OK;
}

static int
cdump_irep_struct(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE *fp, const char *name, int n, mrc_string *init_syms_code, int *mp)
{
  int i, len;
  int max = *mp;
  int debug_available = 0;

  /* dump reps */
  if (0 < irep->rlen) {
    for (i=0,len=irep->rlen; i<len; i++) {
      *mp += len;
      if (cdump_irep_struct(c, irep->reps[i], flags, fp, name, max+i, init_syms_code, mp) != MRC_DUMP_OK)
        return MRC_DUMP_INVALID_ARGUMENT;
    }
    fprintf(fp,   "static const mrb_irep *%s_reps_%d[%d] = {\n", name, n, len);
    for (i=0,len=irep->rlen; i<len; i++) {
      fprintf(fp,   "  &%s_irep_%d,\n", name, max+i);
    }
    fputs("};\n", fp);
  }
  /* dump pool */
  if (0 < irep->plen) {
    len=irep->plen;
    fprintf(fp,   "static const mrb_irep_pool %s_pool_%d[%d] = {\n", name, n, len);
    for (i=0; i<len; i++) {
      if (cdump_pool(c, &irep->pool[i], fp) != MRC_DUMP_OK)
        return MRC_DUMP_INVALID_ARGUMENT;
    }
    fputs("};\n", fp);
  }
  /* dump syms */
  if (0 < irep->slen) {
    cdump_syms(c, name, "syms", n, irep->slen, irep->syms, init_syms_code, fp);
  }
  /* dump iseq */
  len=irep->ilen+sizeof(struct mrc_irep_catch_handler)*irep->clen;
  fprintf(fp,   "static const mrb_code %s_iseq_%d[%d] = {", name, n, len);
  for (i=0; i<len; i++) {
    if (i%20 == 0) fputs("\n", fp);
    fprintf(fp, "0x%02x,", irep->iseq[i]);
  }
  fputs("};\n", fp);
  /* dump lv */
  if (irep->lv) {
    cdump_syms(c, name, "lv", n, irep->nlocals-1, irep->lv, init_syms_code, fp);
  }
  /* dump debug */
  if (flags & MRC_DUMP_DEBUG_INFO) {
    if (cdump_debug(c, name, n, irep->debug_info, init_syms_code, fp) == MRC_DUMP_OK) {
      debug_available = 1;
    }
  }


  /* dump irep */
  fprintf(fp, "static const mrb_irep %s_irep_%d = {\n", name, n);
  fprintf(fp,   "  %d,%d,%d,\n", irep->nlocals, irep->nregs, irep->clen);
  fprintf(fp,   "  MRB_IREP_STATIC,%s_iseq_%d,\n", name, n);
  if (0 < irep->plen) {
    fprintf(fp, "  %s_pool_%d,", name, n);
  }
  else {
    fputs(      "  NULL,", fp);
  }
  if (0 < irep->slen) {
    fprintf(fp, "%s_syms_%d,", name, n);
  }
  else {
    fputs(      "NULL,", fp);
  }
  if (0 < irep->rlen) {
    fprintf(fp, "%s_reps_%d,\n", name, n);
  }
  else {
    fputs(      "NULL,\n", fp);
  }
  if (irep->lv) {
    fprintf(fp, "  %s_lv_%d,\n", name, n);
  }
  else {
    fputs(      "  NULL,\t\t\t\t\t/* lv */\n", fp);
  }
  if (debug_available) {
    fprintf(fp, "  &%s_debug_%d,\n", name, n);
  }
  else {
    fputs("  NULL,\t\t\t\t\t/* debug_info */\n", fp);
  }
  fprintf(fp,   "  %d,%d,%d,%d,0\n};\n", irep->ilen, irep->plen, irep->slen, irep->rlen);

  return MRC_DUMP_OK;
}

int
mrc_dump_irep_cstruct(mrc_ccontext *c, const mrc_irep *irep, uint8_t flags, FILE *fp, const char *initname)
{
  if (fp == NULL || initname == NULL || initname[0] == '\0') {
    return MRC_DUMP_INVALID_ARGUMENT;
  }
  if (fprintf(fp, "#include <mruby.h>\n"
                  "#include <mruby/irep.h>\n"
                  "#include <mruby/debug.h>\n"
                  "#include <mruby/proc.h>\n"
                  "#include <mruby/presym.h>\n"
                  "\n") < 0) {
    return MRC_DUMP_WRITE_FAULT;
  }
  fputs("#define mrb_BRACED(...) {__VA_ARGS__}\n", fp);
  fputs("#define mrb_DEFINE_SYMS_VAR(name, len, syms, qualifier) \\\n", fp);
  fputs("  static qualifier mrb_sym name[len] = mrb_BRACED syms\n", fp);
  fputs("\n", fp);
  mrc_string *init_syms_code = mrc_str_new_capa(c, 1);
  int max = 1;
  int n = cdump_irep_struct(c, irep, flags, fp, initname, 0, init_syms_code, &max);
  if (n != MRC_DUMP_OK) return n;
  fprintf(fp,
          "%s\n"
          "const struct RProc %s[] = {{\n",
          (flags & MRC_DUMP_STATIC) ? "static"
                                    : "#ifdef __cplusplus\n"
                                      "extern\n"
                                      "#endif",
          initname);
  fprintf(fp, "NULL,MRB_TT_PROC,MRB_GC_RED,MRB_OBJ_IS_FROZEN,0,{&%s_irep_0},NULL,{NULL},\n}};\n", initname);
  fputs("static void\n", fp);
  fprintf(fp, "%s_init_syms(mrb_state *mrb)\n", initname);
  fputs("{\n", fp);
  fputs(MRC_STRING_PTR(init_syms_code), fp);
  fputs("}\n", fp);
  mrc_str_free(c, init_syms_code);
  return MRC_DUMP_OK;
}
#endif /* MRC_NO_STDIO */
