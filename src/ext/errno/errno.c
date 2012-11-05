/*
** errno.c - SystemCallError and Errno::EXXX
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/array.h"
#include "mruby/hash.h"
#include "mruby/class.h"
#include "mruby/numeric.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include <errno.h>
#include <stdio.h>
#include <string.h>

#ifdef ENABLE_ERRNO
static struct _errno2class {
  mrb_int no;
  struct RClass *cl;
} errno2class[] = {
#include "known_errors_e2c.cstub"
};

#define ERRNO2CLASS_LEN		(sizeof(errno2class) / sizeof(errno2class[0]))

static int
e2c_cmp(const void *a0, const void *b0)
{
  const struct _errno2class *a = a0, *b = b0;
  return a->no - b->no;
}

static struct RClass *
e2c_lookup(mrb_int no)
{
  int i;
  for (i = 0; i < ERRNO2CLASS_LEN; i++) {
    if (errno2class[i].no == no)
      return errno2class[i].cl;
  }
  return NULL;
}

static void
e2c_set(int no, struct RClass *c)
{
  int i;
  for (i = 0; i < ERRNO2CLASS_LEN; i++) {
    if (errno2class[i].no == no) {
      errno2class[i].cl = c;
      return;
    }
  }
}

static void
e2c_sort(void)
{
  qsort(errno2class, ERRNO2CLASS_LEN, sizeof(errno2class[0]), e2c_cmp);
}

static mrb_value
mrb_sce_init(mrb_state *mrb, mrb_value self)
{
  struct RClass *c;
  mrb_value m, str;
  mrb_int n;
  int argc, no_errno = 0;
  char buf[20];

  argc = mrb_get_args(mrb, "o|i", &m, &n);
  if (argc == 1) {
    if (mrb_type(m) == MRB_TT_FIXNUM) {
      n = mrb_fixnum(m);
      m = mrb_nil_value();
    } else {
      no_errno = 1;
    }
  }
  if (!no_errno) {
    c = e2c_lookup(n);
    if (c) {
      mrb_obj_ptr(self)->c = c;
      str = mrb_str_new2(mrb, strerror(n));
    } else {
      mrb_iv_set(mrb, self, mrb_intern(mrb, "errno"), mrb_fixnum_value(n));
      str = mrb_str_new2(mrb, "Unknown error: ");
      snprintf(buf, sizeof(buf), "%d", n);
      mrb_str_cat2(mrb, str, buf);
    }
  } else {
    str = mrb_str_new2(mrb, "unknown error");
  }
  if (!mrb_nil_p(m)) {
    mrb_str_cat2(mrb, str, " - ");
    mrb_str_append(mrb, str, m);
  }
  mrb_iv_set(mrb, self, mrb_intern(mrb, "mesg"), str);
  return self;
}

static mrb_value
mrb_sce_errno(mrb_state *mrb, mrb_value self)
{
  mrb_sym sym;
  mrb_value klass;

  klass = mrb_obj_value(mrb_class(mrb, self));
  sym = mrb_intern(mrb, "Errno");
  if (mrb_const_defined(mrb, klass, sym))
    return mrb_const_get(mrb, klass, sym);
  else {
    sym = mrb_intern(mrb, "errno");
    return mrb_attr_get(mrb, self, sym);
  }
}

static mrb_value
mrb_sce_to_s(mrb_state *mrb, mrb_value self)
{
  return mrb_attr_get(mrb, self, mrb_intern(mrb, "mesg"));
}

static mrb_value
mrb_exxx_init(mrb_state *mrb, mrb_value self)
{
  mrb_value m = mrb_nil_value();
  mrb_value no, str;

  no = mrb_const_get(mrb, mrb_obj_value(mrb_class(mrb, self)), mrb_intern(mrb, "Errno"));
  str = mrb_str_new2(mrb, strerror(mrb_fixnum(no)));

  mrb_get_args(mrb, "|S", &m);
  if (!mrb_nil_p(m)) {
    mrb_str_cat2(mrb, str, " - ");
    mrb_str_append(mrb, str, m);
  }
  mrb_iv_set(mrb, self, mrb_intern(mrb, "mesg"), str);
  return self;
}

void
mrb_sys_fail(mrb_state *mrb, const char *msg)
{
  struct RClass *cl, *sce;
  mrb_value e;
  int no;
  char name[8];

  no = errno; /* save it */
  sce = mrb_class_obj_get(mrb, "SystemCallError");
  if (msg) {
    e = mrb_funcall(mrb, mrb_obj_value(sce), "new", 2, mrb_str_new2(mrb, msg), mrb_fixnum_value(no));
  } else {
    e = mrb_funcall(mrb, mrb_obj_value(sce), "new", 1, mrb_fixnum_value(no));
  }
  if (mrb_obj_class(mrb, e) == sce) {
    snprintf(name, sizeof(name), "E%03d", no);
    cl = mrb_define_class_under(mrb, mrb_class_get(mrb, "Errno"), name, sce);
    mrb_define_const(mrb, cl, "Errno", mrb_fixnum_value(no));
    mrb_obj_ptr(e)->c = cl;
  }
  mrb_exc_raise(mrb, e);
}

void
mrb_init_errno(mrb_state *mrb)
{
  struct RClass *e, *eno, *sce, *ste;
  mrb_value noerror;

  e2c_sort();

  ste = mrb_class_obj_get(mrb, "StandardError");

  sce = mrb_define_class(mrb, "SystemCallError", ste);
  mrb_define_method(mrb, sce, "errno", mrb_sce_errno, ARGS_NONE());
  mrb_define_method(mrb, sce, "to_s", mrb_sce_to_s, ARGS_NONE());
  mrb_define_method(mrb, sce, "initialize", mrb_sce_init, ARGS_REQ(1)|ARGS_OPT(1));

  eno = mrb_define_module(mrb, "Errno");

  e = mrb_define_class_under(mrb, eno, "NOERROR", sce);
  mrb_define_const(mrb, e, "Errno", mrb_fixnum_value(0));
  mrb_define_method(mrb, e, "initialize", mrb_exxx_init, ARGS_OPT(1));
  //mrb_define_method(mrb, e, "===", mrb_exxx_cmp, ARGS_REQ(1));
  noerror = mrb_obj_value(e);

#define itsdefined(SYM) \
  do {									\
    int ai = mrb_gc_arena_save(mrb);					\
    e = mrb_define_class_under(mrb, eno, #SYM, sce);			\
    mrb_define_const(mrb, e, "Errno", mrb_fixnum_value(SYM));		\
    mrb_define_method(mrb, e, "initialize", mrb_exxx_init, ARGS_OPT(1)); \
    e2c_set(SYM, e);							\
    mrb_gc_arena_restore(mrb, ai);					\
  } while (0)

#define itsnotdefined(SYM) \
  do {									\
    mrb_define_const(mrb, eno, #SYM, noerror);				\
  } while (0)

#include "known_errors_def.cstub"
}
#endif /* ENABLE_ERRNO */
