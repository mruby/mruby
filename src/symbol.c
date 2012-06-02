/*
** symbol.c - Symbol class
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/khash.h"
#include <string.h>

#include <stdarg.h>
#include <string.h>
#include "mruby/string.h"
#include <ctype.h>
#include "mruby/class.h"
#include "mruby/variable.h"
#include <stdio.h>

/* ------------------------------------------------------ */
KHASH_INIT(s2n, mrb_sym, const char*, 1, kh_int_hash_func, kh_int_hash_equal)
KHASH_MAP_INIT_STR(n2s, mrb_sym);
/* ------------------------------------------------------ */
mrb_sym
mrb_intern(mrb_state *mrb, const char *name)
{
  khash_t(n2s) *h = mrb->name2sym;
  khash_t(s2n) *rh = mrb->sym2name;
  khiter_t k;
  size_t len;
  char *p;
  mrb_sym sym;

  k = kh_get(n2s, h, name);
  if (k != kh_end(h))
    return kh_value(h, k);

  sym = ++mrb->symidx;
  len = strlen(name);
  p = mrb_malloc(mrb, len+1);
  memcpy(p, name, len);
  p[len] = 0;
  k = kh_put(n2s, h, p);
  kh_value(h, k) = sym;

  k = kh_put(s2n, rh, sym);
  kh_value(rh, k) = p;

  return sym;
}

const char*
mrb_sym2name(mrb_state *mrb, mrb_sym sym)
{
  khash_t(s2n) *h = mrb->sym2name;
  khiter_t k;

  k = kh_get(s2n, h, sym);
  if (k == kh_end(h)) {
    return NULL;                /* missing */
  }
  return kh_value(h, k);
}

void
mrb_free_symtbls(mrb_state *mrb)
{
  khash_t(s2n) *h = mrb->sym2name;
  khiter_t k;

  for (k = kh_begin(h); k != kh_end(h); ++k)
    if (kh_exist(h, k)) mrb_free(mrb, (char*)kh_value(h, k));
  kh_destroy(s2n,mrb->sym2name);
  kh_destroy(n2s,mrb->name2sym);
}

void
mrb_init_symtbl(mrb_state *mrb)
{
  mrb->name2sym = kh_init(n2s, mrb);
  mrb->sym2name = kh_init(s2n, mrb);
}

/**********************************************************************
 * Document-class: Symbol
 *
 *  <code>Symbol</code> objects represent names and some strings
 *  inside the Ruby
 *  interpreter. They are generated using the <code>:name</code> and
 *  <code>:"string"</code> literals
 *  syntax, and by the various <code>to_sym</code> methods. The same
 *  <code>Symbol</code> object will be created for a given name or string
 *  for the duration of a program's execution, regardless of the context
 *  or meaning of that name. Thus if <code>Fred</code> is a constant in
 *  one context, a method in another, and a class in a third, the
 *  <code>Symbol</code> <code>:Fred</code> will be the same object in
 *  all three contexts.
 *
 *     module One
 *       class Fred
 *       end
 *       $f1 = :Fred
 *     end
 *     module Two
 *       Fred = 1
 *       $f2 = :Fred
 *     end
 *     def Fred()
 *     end
 *     $f3 = :Fred
 *     $f1.object_id   #=> 2514190
 *     $f2.object_id   #=> 2514190
 *     $f3.object_id   #=> 2514190
 *
 */


/* 15.2.11.3.1  */
/*
 *  call-seq:
 *     sym == obj   -> true or false
 *
 *  Equality---If <i>sym</i> and <i>obj</i> are exactly the same
 *  symbol, returns <code>true</code>.
 */

static mrb_value
sym_equal(mrb_state *mrb, mrb_value sym1)
{
  mrb_value sym2;

  mrb_get_args(mrb, "o", &sym2);
  if (mrb_obj_equal(mrb, sym1, sym2)) return mrb_true_value();
    return mrb_false_value();
}

/* 15.2.11.3.2  */
/* 15.2.11.3.3  */
/*
 *  call-seq:
 *     sym.id2name   -> string
 *     sym.to_s      -> string
 *
 *  Returns the name or string corresponding to <i>sym</i>.
 *
 *     :fred.id2name   #=> "fred"
 */
mrb_value
mrb_sym_to_s(mrb_state *mrb, mrb_value sym)
{
  mrb_sym id = SYM2ID(sym);

  return mrb_str_new_cstr(mrb, mrb_sym2name(mrb, id));
}

/* 15.2.11.3.4  */
/*
 * call-seq:
 *   sym.to_sym   -> sym
 *   sym.intern   -> sym
 *
 * In general, <code>to_sym</code> returns the <code>Symbol</code> corresponding
 * to an object. As <i>sym</i> is already a symbol, <code>self</code> is returned
 * in this case.
 */

static mrb_value
sym_to_sym(mrb_state *mrb, mrb_value sym)
{
    return sym;
}

/* 15.2.11.3.5(x)  */
/*
 *  call-seq:
 *     sym.inspect    -> string
 *
 *  Returns the representation of <i>sym</i> as a symbol literal.
 *
 *     :fred.inspect   #=> ":fred"
 */

#if __STDC__
# define SIGN_EXTEND_CHAR(c) ((signed char)(c))
#else  /* not __STDC__ */
/* As in Harbison and Steele.  */
# define SIGN_EXTEND_CHAR(c) ((((unsigned char)(c)) ^ 128) - 128)
#endif
#define is_identchar(c) (SIGN_EXTEND_CHAR(c)!=-1&&(ISALNUM(c) || (c) == '_'))

static int
is_special_global_name(const char* m)
{
    switch (*m) {
      case '~': case '*': case '$': case '?': case '!': case '@':
      case '/': case '\\': case ';': case ',': case '.': case '=':
      case ':': case '<': case '>': case '\"':
      case '&': case '`': case '\'': case '+':
      case '0':
        ++m;
        break;
      case '-':
        ++m;
        if (is_identchar(*m)) m += 1;
        break;
      default:
        if (!ISDIGIT(*m)) return 0;
        do ++m; while (ISDIGIT(*m));
    }
    return !*m;
}

static int
symname_p(const char *name)
{
    const char *m = name;
    int localid = FALSE;

    if (!m) return FALSE;
    switch (*m) {
      case '\0':
        return FALSE;

      case '$':
        if (is_special_global_name(++m)) return TRUE;
        goto id;

      case '@':
        if (*++m == '@') ++m;
        goto id;

      case '<':
        switch (*++m) {
          case '<': ++m; break;
          case '=': if (*++m == '>') ++m; break;
          default: break;
        }
        break;

      case '>':
        switch (*++m) {
          case '>': case '=': ++m; break;
        }
        break;

      case '=':
        switch (*++m) {
          case '~': ++m; break;
          case '=': if (*++m == '=') ++m; break;
          default: return FALSE;
        }
        break;

      case '*':
        if (*++m == '*') ++m;
        break;

      case '+': case '-':
        if (*++m == '@') ++m;
        break;

      case '|': case '^': case '&': case '/': case '%': case '~': case '`':
        ++m;
        break;

      case '[':
        if (*++m != ']') return FALSE;
        if (*++m == '=') ++m;
        break;

      default:
        localid = !ISUPPER(*m);
id:
        if (*m != '_' && !ISALPHA(*m)) return FALSE;
        while (is_identchar(*m)) m += 1;
        if (localid) {
            switch (*m) {
              case '!': case '?': case '=': ++m;
            }
        }
        break;
    }
    return *m ? FALSE : TRUE;
}

static mrb_value
sym_inspect(mrb_state *mrb, mrb_value sym)
{
  mrb_value str;
  const char *name;
  mrb_sym id = SYM2ID(sym);

  name = mrb_sym2name(mrb, id); //mrb_id2name(id);
  str = mrb_str_new(mrb, 0, strlen(name)+1);
  RSTRING(str)->buf[0] = ':';
  strcpy(RSTRING(str)->buf+1, name);
  if (!symname_p(name)) {
    str = mrb_str_dump(mrb, str);
    strncpy(RSTRING(str)->buf, ":\"", 2);
  }
  return str;
}


void
mrb_init_symbols(mrb_state *mrb)
{
  struct RClass *sym;

  sym = mrb->symbol_class = mrb_define_class(mrb, "Symbol", mrb->object_class);

  mrb_define_method(mrb, sym, "===",             sym_equal,               ARGS_REQ(1));              /* 15.2.11.3.1  */
  mrb_define_method(mrb, sym, "id2name",         mrb_sym_to_s,            ARGS_NONE());              /* 15.2.11.3.2  */
  mrb_define_method(mrb, sym, "to_s",            mrb_sym_to_s,            ARGS_NONE());              /* 15.2.11.3.3  */
  mrb_define_method(mrb, sym, "to_sym",          sym_to_sym,              ARGS_NONE());              /* 15.2.11.3.4  */

  mrb_define_method(mrb, sym, "inspect",         sym_inspect,             ARGS_NONE());              /* 15.2.11.3.5(x)  */
}
