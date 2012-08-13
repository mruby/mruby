/*
** mruby/value.h - mrb_value definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_VALUE_H
#define MRUBY_VALUE_H

enum mrb_vtype {
  MRB_TT_FALSE = 0,   /*   0 */
  MRB_TT_FREE,        /*   1 */
  MRB_TT_TRUE,        /*   2 */
  MRB_TT_FIXNUM,      /*   3 */
  MRB_TT_SYMBOL,      /*   4 */
  MRB_TT_UNDEF,       /*   5 */
  MRB_TT_FLOAT,       /*   6 */
  MRB_TT_OBJECT,      /*   7 */
  MRB_TT_CLASS,       /*   8 */
  MRB_TT_MODULE,      /*   9 */
  MRB_TT_ICLASS,      /*  10 */
  MRB_TT_SCLASS,      /*  11 */
  MRB_TT_PROC,        /*  12 */
  MRB_TT_ARRAY,       /*  13 */
  MRB_TT_HASH,        /*  14 */
  MRB_TT_STRING,      /*  15 */
  MRB_TT_RANGE,       /*  16 */
  MRB_TT_REGEX,       /*  17 */
  MRB_TT_STRUCT,      /*  18 */
  MRB_TT_EXCEPTION,   /*  19 */
  MRB_TT_MATCH,       /*  20 */
  MRB_TT_FILE,        /*  21 */
  MRB_TT_ENV,         /*  22 */
  MRB_TT_DATA,        /*  23 */
  MRB_TT_MAXDEFINE    /*  24 */
};

typedef struct mrb_value {
  union {
    mrb_float f;
    void *p;
    mrb_int i;
    mrb_sym sym;
  } value;
  enum mrb_vtype tt:8;
} mrb_value;

#define mrb_type(o)   (o).tt
#define mrb_nil_p(o)  ((o).tt == MRB_TT_FALSE && !(o).value.i)
#define mrb_test(o)   ((o).tt != MRB_TT_FALSE)
#define mrb_fixnum(o) (o).value.i
#define mrb_float(o)  (o).value.f
#define mrb_symbol(o) (o).value.sym
#define mrb_object(o) ((struct RBasic *) (o).value.p)
#define FIXNUM_P(o)   ((o).tt == MRB_TT_FIXNUM)
#define mrb_undef_p(o) ((o).tt == MRB_TT_UNDEF)

#include "mruby/object.h"

#define IMMEDIATE_P(x) (mrb_type(x) <= MRB_TT_FLOAT)
#define SPECIAL_CONST_P(x) IMMEDIATE_P(x)
#define SYMBOL_P(o) (mrb_type(o) == MRB_TT_SYMBOL)
#define RTEST(o) mrb_test(o)

#define FL_ABLE(x) (!SPECIAL_CONST_P(x))
#define FL_TEST(x,f) (FL_ABLE(x)?(RBASIC(x)->flags&(f)):0)
#define FL_ANY(x,f) FL_TEST(x,f)
#define FL_ALL(x,f) (FL_TEST(x,f) == (f))
#define FL_SET(x,f) do {if (FL_ABLE(x)) RBASIC(x)->flags |= (f);} while (0)
#define FL_UNSET(x,f) do {if (FL_ABLE(x)) RBASIC(x)->flags &= ~(f);} while (0)

static inline mrb_int
mrb_special_const_p(mrb_value obj)
{
  if (SPECIAL_CONST_P(obj)) return 1;
  return 0;
}
static inline mrb_value
mrb_fixnum_value(mrb_int i)
{
  mrb_value v;

  v.tt = MRB_TT_FIXNUM;
  v.value.i = i;
  return v;
}

static inline mrb_value
mrb_float_value(mrb_float f)
{
  mrb_value v;

  v.tt = MRB_TT_FLOAT;
  v.value.f = f;
  return v;
}

static inline mrb_value
mrb_symbol_value(mrb_sym i)
{
  mrb_value v;

  v.tt = MRB_TT_SYMBOL;
  v.value.sym = i;
  return v;
}

static inline mrb_value
mrb_obj_value(void *p)
{
  mrb_value v;
  struct RBasic *b = (struct RBasic*) p;

  v.tt = b->tt;
  v.value.p = p;
  return v;
}

static inline mrb_value
mrb_false_value(void)
{
  mrb_value v;

  v.tt = MRB_TT_FALSE;
  v.value.i = 1;
  return v;
}

static inline mrb_value
mrb_nil_value(void)
{
  mrb_value v;

  v.tt = MRB_TT_FALSE;
  v.value.i = 0;
  return v;
}

static inline mrb_value
mrb_true_value(void)
{
  mrb_value v;

  v.tt = MRB_TT_TRUE;
  v.value.i = 1;
  return v;
}

static inline mrb_value
mrb_undef_value(void)
{
  mrb_value v;

  v.tt = MRB_TT_UNDEF;
  v.value.i = 0;
  return v;
}

#endif  /* MRUBY_OBJECT_H */
