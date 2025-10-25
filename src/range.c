/*
** range.c - Range class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#define RANGE_INITIALIZED_FLAG 1
#define RANGE_INITIALIZED(p) ((p)->flags |= RANGE_INITIALIZED_FLAG)
#define RANGE_INITIALIZED_P(p) ((p)->flags & RANGE_INITIALIZED_FLAG)

static void
r_check(mrb_state *mrb, mrb_value a, mrb_value b)
{
  enum mrb_vtype ta = mrb_type(a);
  enum mrb_vtype tb = mrb_type(b);

#ifdef MRB_NO_FLOAT
  if (ta == MRB_TT_INTEGER && tb == MRB_TT_INTEGER ) return;
#else
  if ((ta == MRB_TT_INTEGER || ta == MRB_TT_FLOAT) &&
      (tb == MRB_TT_INTEGER || tb == MRB_TT_FLOAT)) {
    return;
  }
#endif

  if (mrb_nil_p(a) || mrb_nil_p(b)) return;

  mrb_int n = mrb_cmp(mrb, a, b);
  if (n == -2) {                /* can not be compared */
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad value for range");
  }
}

static mrb_bool
r_le(mrb_state *mrb, mrb_value a, mrb_value b)
{
  mrb_int n = mrb_cmp(mrb, a, b);

  if (n == 0 || n == -1) return TRUE;
  return FALSE;
}

static mrb_bool
r_gt(mrb_state *mrb, mrb_value a, mrb_value b)
{
  return mrb_cmp(mrb, a, b) == 1;
}

static mrb_bool
r_ge(mrb_state *mrb, mrb_value a, mrb_value b)
{
  mrb_int n = mrb_cmp(mrb, a, b);

  if (n == 0 || n == 1) return TRUE;
  return FALSE;
}

static void
range_ptr_alloc_edges(mrb_state *mrb, struct RRange *r)
{
#ifndef MRB_RANGE_EMBED
  r->edges = (mrb_range_edges*)mrb_malloc(mrb, sizeof(mrb_range_edges));
#endif
}

static struct RRange *
range_ptr_init(mrb_state *mrb, struct RRange *r, mrb_value beg, mrb_value end, mrb_bool excl)
{
  r_check(mrb, beg, end);

  if (r) {
    if (RANGE_INITIALIZED_P(r)) {
      /* Ranges are immutable, so that they should be initialized only once. */
      mrb_name_error(mrb, MRB_SYM(initialize), "'initialize' called twice");
    }
    else {
      range_ptr_alloc_edges(mrb, r);
    }
  }
  else {
    r = MRB_OBJ_ALLOC(mrb, MRB_TT_RANGE, mrb->range_class);
    range_ptr_alloc_edges(mrb, r);
  }

  RANGE_BEG(r) = beg;
  RANGE_END(r) = end;
  RANGE_EXCL(r) = excl;
  RANGE_INITIALIZED(r);

  return r;
}

static void
range_ptr_replace(mrb_state *mrb, struct RRange *r, mrb_value beg, mrb_value end, mrb_bool excl)
{
  range_ptr_init(mrb, r, beg, end, excl);
  mrb_write_barrier(mrb, (struct RBasic*)r);
}

/*
 *  call-seq:
 *     rng.first    => obj
 *     rng.begin    => obj
 *
 *  Returns the first object in `rng`.
 */
static mrb_value
range_beg(mrb_state *mrb, mrb_value range)
{
  return mrb_range_beg(mrb, range);
}

/*
 *  call-seq:
 *     rng.end    => obj
 *     rng.last   => obj
 *
 *  Returns the object that defines the end of `rng`.
 *
 *     (1..10).end    #=> 10
 *     (1...10).end   #=> 10
 */
static mrb_value
range_end(mrb_state *mrb, mrb_value range)
{
  return mrb_range_end(mrb, range);
}

/*
 *  call-seq:
 *     range.exclude_end?    => true or false
 *
 *  Returns `true` if `range` excludes its end value.
 */
static mrb_value
range_excl(mrb_state *mrb, mrb_value range)
{
  return mrb_bool_value(mrb_range_excl_p(mrb, range));
}

/*
 *  call-seq:
 *     Range.new(start, end, exclusive=false)    => range
 *
 *  Constructs a range using the given `start` and `end`. If the third
 *  parameter is omitted or is `false`, the `range` will include
 *  the end object; otherwise, it will be excluded.
 */
static mrb_value
range_initialize(mrb_state *mrb, mrb_value range)
{
  mrb_value beg, end;
  mrb_bool exclusive = FALSE;

  mrb_get_args(mrb, "oo|b", &beg, &end, &exclusive);
  range_ptr_replace(mrb, mrb_range_raw_ptr(range), beg, end, exclusive);
  mrb_obj_freeze(mrb, range);
  return range;
}

/*
 *  call-seq:
 *     range == obj    => true or false
 *
 *  Returns `true` only if
 *  1) `obj` is a Range,
 *  2) `obj` has equivalent beginning and end items (by comparing them with `==`),
 *  3) `obj` has the same #exclude_end? setting as `rng`.
 *
 *    (0..2) == (0..2)            #=> true
 *    (0..2) == Range.new(0,2)    #=> true
 *    (0..2) == (0...2)           #=> false
 */
static mrb_value
range_eq(mrb_state *mrb, mrb_value range)
{
  struct RRange *rr;
  struct RRange *ro;
  mrb_value obj = mrb_get_arg1(mrb);
  mrb_bool v1, v2;

  if (mrb_obj_equal(mrb, range, obj)) return mrb_true_value();
  if (!mrb_obj_is_instance_of(mrb, obj, mrb_obj_class(mrb, range))) { /* same class? */
    return mrb_false_value();
  }

  rr = mrb_range_ptr(mrb, range);
  ro = mrb_range_ptr(mrb, obj);
  v1 = mrb_equal(mrb, RANGE_BEG(rr), RANGE_BEG(ro));
  v2 = mrb_equal(mrb, RANGE_END(rr), RANGE_END(ro));
  if (!v1 || !v2 || RANGE_EXCL(rr) != RANGE_EXCL(ro)) {
    return mrb_false_value();
  }
  return mrb_true_value();
}

/*
 *  call-seq:
 *     range === obj       =>  true or false
 *     range.member?(val)  =>  true or false
 *     range.include?(val) =>  true or false
 */
static mrb_value
range_include(mrb_state *mrb, mrb_value range)
{
  mrb_value val = mrb_get_arg1(mrb);
  struct RRange *r = mrb_range_ptr(mrb, range);
  mrb_value beg, end;

  beg = RANGE_BEG(r);
  end = RANGE_END(r);
  if (mrb_nil_p(beg)) {
    if (RANGE_EXCL(r) ? r_gt(mrb, end, val)    /* end >  val */
                      : r_ge(mrb, end, val)) { /* end >= val */
      return mrb_true_value();
    }
  }
  else if (r_le(mrb, beg, val)) {              /* beg <= val */
    if (mrb_nil_p(end)) {
      return mrb_true_value();
    }
    if (RANGE_EXCL(r) ? r_gt(mrb, end, val)    /* end >  val */
                      : r_ge(mrb, end, val)) { /* end >= val */
      return mrb_true_value();
    }
  }
  return mrb_false_value();
}

/* 15.2.14.4.12(x) */
/*
 * call-seq:
 *   rng.to_s   -> string
 *
 * Convert this range object to a printable form.
 */
static mrb_value
range_to_s(mrb_state *mrb, mrb_value range)
{
  mrb_value str, str2;
  struct RRange *r = mrb_range_ptr(mrb, range);

  str  = mrb_obj_as_string(mrb, RANGE_BEG(r));
  str2 = mrb_obj_as_string(mrb, RANGE_END(r));
  str  = mrb_str_dup(mrb, str);
  mrb_str_cat(mrb, str, "...", RANGE_EXCL(r) ? 3 : 2);
  mrb_str_cat_str(mrb, str, str2);

  return str;
}

/* 15.2.14.4.13(x) */
/*
 * call-seq:
 *   rng.inspect  -> string
 *
 * Convert this range object to a printable form (using
 * `inspect` to convert the start and end
 * objects).
 */
static mrb_value
range_inspect(mrb_state *mrb, mrb_value range)
{
  mrb_value str;
  struct RRange *r = mrb_range_ptr(mrb, range);

  if (!mrb_nil_p(RANGE_BEG(r))) {
    str  = mrb_inspect(mrb, RANGE_BEG(r));
    str  = mrb_str_dup(mrb, str);
    mrb_str_cat(mrb, str, "...", RANGE_EXCL(r) ? 3 : 2);
  }
  else {
    str = mrb_str_new(mrb, "...", RANGE_EXCL(r) ? 3 : 2);
  }
  if (!mrb_nil_p(RANGE_END(r))) {
    mrb_value str2 = mrb_inspect(mrb, RANGE_END(r));
    mrb_str_cat_str(mrb, str, str2);
  }

  return str;
}

/* 15.2.14.4.14(x) */
/*
 *  call-seq:
 *     rng.eql?(obj)    -> true or false
 *
 *  Returns `true` only if `obj` is a Range, has equivalent
 *  beginning and end items (by comparing them with #eql?), and has the same
 *  #exclude_end? setting as `rng`.
 *
 *    (0..2).eql?(0..2)            #=> true
 *    (0..2).eql?(Range.new(0,2))  #=> true
 *    (0..2).eql?(0...2)           #=> false
 */
static mrb_value
range_eql(mrb_state *mrb, mrb_value range)
{
  mrb_value obj = mrb_get_arg1(mrb);
  struct RRange *r, *o;

  if (mrb_obj_equal(mrb, range, obj)) return mrb_true_value();
  if (!mrb_range_p(obj)) return mrb_false_value();

  r = mrb_range_ptr(mrb, range);
  o = mrb_range_ptr(mrb, obj);
  if (!mrb_eql(mrb, RANGE_BEG(r), RANGE_BEG(o)) ||
      !mrb_eql(mrb, RANGE_END(r), RANGE_END(o)) ||
      (RANGE_EXCL(r) != RANGE_EXCL(o))) {
    return mrb_false_value();
  }
  return mrb_true_value();
}

/* 15.2.14.4.15(x) */
static mrb_value
range_initialize_copy(mrb_state *mrb, mrb_value copy)
{
  mrb_value src = mrb_get_arg1(mrb);
  struct RRange *r;

  if (mrb_obj_equal(mrb, copy, src)) return copy;
  if (!mrb_obj_is_instance_of(mrb, src, mrb_obj_class(mrb, copy))) {
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument class");
  }

  r = mrb_range_ptr(mrb, src);
  range_ptr_replace(mrb, mrb_range_raw_ptr(copy), RANGE_BEG(r), RANGE_END(r), RANGE_EXCL(r));
  mrb_obj_freeze(mrb, copy);

  return copy;
}

static mrb_value
range_num_to_a(mrb_state *mrb, mrb_value range)
{
  struct RRange *r = mrb_range_ptr(mrb, range);
  mrb_value beg = RANGE_BEG(r);
  mrb_value end = RANGE_END(r);

  mrb->c->ci->mid = 0;
  if (mrb_nil_p(end)) {
    mrb_raise(mrb, E_RANGE_ERROR, "cannot convert endless range to an array");
  }
  if (mrb_integer_p(beg)) {
    if (mrb_integer_p(end)) {
      mrb_int a = mrb_integer(beg);
      mrb_int b = mrb_integer(end);

      if (a > b) {
        return mrb_ary_new_capa(mrb, 0);
      }
      mrb_int len;

      if (mrb_int_sub_overflow(b, a, &len)) {
      too_long:
        mrb_raise(mrb, E_RANGE_ERROR, "integer range too long");
      }
      if (!RANGE_EXCL(r)) {
        if (len == MRB_INT_MAX) goto too_long;
        len++;
      }
      mrb_value ary = mrb_ary_new_capa(mrb, len);
      mrb_value *ptr = RARRAY_PTR(ary);
      for (mrb_int i=0; i<len; i++) {
        ptr[i] = mrb_int_value(mrb, a+i);
        ARY_SET_LEN(RARRAY(ary), i+1);
      }
      return ary;
    }
#ifndef MRB_NO_FLOAT
    if (mrb_float_p(end)) {
      mrb_int a = mrb_integer(beg);
      mrb_float b = mrb_float(end);

      if (a > b) {
        return mrb_ary_new_capa(mrb, 0);
      }
      mrb_int alen = (mrb_int)(b - a) + RANGE_EXCL(r);
      mrb_value ary = mrb_ary_new_capa(mrb, alen);
      mrb_value *ptr = RARRAY_PTR(ary);
      for (mrb_int i=0; i<alen; i++) {
        ptr[i] = mrb_int_value(mrb, a+i);
        ARY_SET_LEN(RARRAY(ary), i);
      }
      return ary;
    }
#endif
  }
#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(beg)) {
    end = mrb_as_bint(mrb, end);
    switch (mrb_bint_cmp(mrb, end, beg)) {
    case 0: case -1:
      return mrb_ary_new_capa(mrb, 0);
    case -2:
      return mrb_nil_value();
    default:
      break;
    }
    mrb_value lenv = mrb_bint_sub(mrb, end, beg);
    if (!mrb_fixnum_p(lenv)) {
      mrb_raise(mrb, E_RANGE_ERROR, "integer range too long");
    }
    mrb_int len = mrb_integer(lenv);
    if (!RANGE_EXCL(r)) len++;
    mrb_value ary = mrb_ary_new_capa(mrb, len);
    for (mrb_int i=0; i<len; i++) {
      RARRAY_PTR(ary)[i] = mrb_bint_add(mrb, beg, mrb_int_value(mrb, i));
      ARY_SET_LEN(RARRAY(ary), i+1);
    }
    return ary;
  }
#endif
  return mrb_nil_value();
}

mrb_value
mrb_get_values_at(mrb_state *mrb, mrb_value obj, mrb_int olen, mrb_int argc, const mrb_value *argv, mrb_value (*func)(mrb_state*, mrb_value, mrb_int))
{
  mrb_int i, j, beg, len;
  mrb_value result;
  result = mrb_ary_new(mrb);

  for (i = 0; i < argc; i++) {
    mrb_value v = argv[i];

    if (mrb_integer_p(v)
#ifdef MRB_USE_BIGINT
        || mrb_bigint_p(v)
#endif
        ) {
      mrb_int i = mrb_as_int(mrb, v);
      mrb_ary_push(mrb, result, func(mrb, obj, i));
    }
    else if (mrb_range_beg_len(mrb, v, &beg, &len, olen, FALSE) == MRB_RANGE_OK) {
      mrb_int const end = olen < beg + len ? olen : beg + len;
      for (j = beg; j < end; j++) {
        mrb_ary_push(mrb, result, func(mrb, obj, j));
      }

      for (; j < beg + len; j++) {
        mrb_ary_push(mrb, result, mrb_nil_value());
      }
    }
    else {
      mrb_raisef(mrb, E_TYPE_ERROR, "invalid values selector: %v", v);
    }
  }

  return result;
}

size_t
mrb_gc_mark_range(mrb_state *mrb, struct RRange *r)
{
  if (!RANGE_INITIALIZED_P(r)) return 0;
  mrb_gc_mark_value(mrb, RANGE_BEG(r));
  mrb_gc_mark_value(mrb, RANGE_END(r));
  return 2;
}

/**
 * Retrieves a pointer to the internal RRange structure from a Range object.
 *
 * This function ensures that the given Range object is properly initialized
 * before returning a pointer to its data. If the Range is not initialized,
 * it raises an E_ARGUMENT_ERROR.
 *
 * @param mrb The mruby state.
 * @param range The Range object (mrb_value) from which to get the pointer.
 * @return A pointer to the struct RRange.
 */
MRB_API struct RRange*
mrb_range_ptr(mrb_state *mrb, mrb_value range)
{
  struct RRange *r = mrb_range_raw_ptr(range);

  /* check for if #initialize_copy was removed [#3320] */
  if (!RANGE_INITIALIZED_P(r)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized range");
  }
  return r;
}

/**
 * Creates a new Range object.
 *
 * This function allocates and initializes a new Range object with the given
 * beginning and end values, and an exclude_end flag.
 *
 * @param mrb The mruby state.
 * @param beg The beginning value of the range.
 * @param end The end value of the range.
 * @param excl A boolean flag indicating whether the end value is excluded (true) or included (false).
 * @return The new Range object (mrb_value).
 */
MRB_API mrb_value
mrb_range_new(mrb_state *mrb, mrb_value beg, mrb_value end, mrb_bool excl)
{
  struct RRange *r = range_ptr_init(mrb, NULL, beg, end, excl);
  return mrb_range_value(r);
}

/**
 * Calculates the beginning position and length of a range.
 *
 * This function is typically used to get array offsets.
 * It interprets the range as a sequence of integers.
 * - If the beginning of the range is nil, it's treated as 0.
 * - If the end of the range is nil, it's treated as -1 (or the last element).
 *
 * The function handles negative indices, out-of-bounds conditions,
 * and truncation based on the provided length and trunc flag.
 *
 * @param mrb The mruby state.
 * @param range The Range object (mrb_value).
 * @param[out] begp Pointer to store the calculated beginning position.
 * @param[out] lenp Pointer to store the calculated length.
 * @param len The length of the sequence the range is being applied to.
 * @param trunc If true, truncate the range to fit within the given length.
 * @return An enum mrb_range_beg_len indicating success (MRB_RANGE_OK),
 *         type mismatch (MRB_RANGE_TYPE_MISMATCH), or out of bounds (MRB_RANGE_OUT).
 */
MRB_API enum mrb_range_beg_len
mrb_range_beg_len(mrb_state *mrb, mrb_value range, mrb_int *begp, mrb_int *lenp, mrb_int len, mrb_bool trunc)
{
  mrb_int beg, end;
  mrb_bool excl;
  struct RRange *r;

  if (!mrb_range_p(range)) return MRB_RANGE_TYPE_MISMATCH;
  r = mrb_range_ptr(mrb, range);

  beg = mrb_nil_p(RANGE_BEG(r)) ? 0 : mrb_as_int(mrb, RANGE_BEG(r));
  end = mrb_nil_p(RANGE_END(r)) ? -1 : mrb_as_int(mrb, RANGE_END(r));
  excl = mrb_nil_p(RANGE_END(r)) ? 0 : RANGE_EXCL(r);

  if (beg < 0) {
    beg += len;
    if (beg < 0) return MRB_RANGE_OUT;
  }

  if (trunc) {
    if (beg > len) return MRB_RANGE_OUT;
    if (end > len) end = len;
  }

  if (end < 0) end += len;
  if (!excl && (!trunc || end < len)) end++;  /* include end point */
  len = end - beg;
  if (len < 0) len = 0;

  *begp = beg;
  *lenp = len;
  return MRB_RANGE_OK;
}

void
mrb_init_range(mrb_state *mrb)
{
  struct RClass *r;

  r = mrb_define_class_id(mrb, MRB_SYM(Range), mrb->object_class);                                /* 15.2.14 */
  mrb->range_class = r;
  MRB_SET_INSTANCE_TT(r, MRB_TT_RANGE);

  mrb_define_method_id(mrb, r, MRB_SYM(begin),           range_beg,             MRB_ARGS_NONE()); /* 15.2.14.4.3  */
  mrb_define_method_id(mrb, r, MRB_SYM(end),             range_end,             MRB_ARGS_NONE()); /* 15.2.14.4.5  */
  mrb_define_method_id(mrb, r, MRB_OPSYM(eq),            range_eq,              MRB_ARGS_REQ(1)); /* 15.2.14.4.1  */
  mrb_define_method_id(mrb, r, MRB_OPSYM(eqq),           range_include,         MRB_ARGS_REQ(1)); /* 15.2.14.4.2  */
  mrb_define_method_id(mrb, r, MRB_SYM_Q(exclude_end),   range_excl,            MRB_ARGS_NONE()); /* 15.2.14.4.6  */
  mrb_define_method_id(mrb, r, MRB_SYM(first),           range_beg,             MRB_ARGS_NONE()); /* 15.2.14.4.7  */
  mrb_define_method_id(mrb, r, MRB_SYM_Q(include),       range_include,         MRB_ARGS_REQ(1)); /* 15.2.14.4.8  */
  mrb_define_method_id(mrb, r, MRB_SYM(initialize),      range_initialize,      MRB_ARGS_ANY());  /* 15.2.14.4.9  */
  mrb_define_method_id(mrb, r, MRB_SYM(last),            range_end,             MRB_ARGS_NONE()); /* 15.2.14.4.10 */
  mrb_define_method_id(mrb, r, MRB_SYM_Q(member),        range_include,         MRB_ARGS_REQ(1)); /* 15.2.14.4.11 */
  mrb_define_method_id(mrb, r, MRB_SYM(to_s),            range_to_s,            MRB_ARGS_NONE()); /* 15.2.14.4.12(x) */
  mrb_define_method_id(mrb, r, MRB_SYM(inspect),         range_inspect,         MRB_ARGS_NONE()); /* 15.2.14.4.13(x) */
  mrb_define_method_id(mrb, r, MRB_SYM_Q(eql),           range_eql,             MRB_ARGS_REQ(1)); /* 15.2.14.4.14(x) */
  mrb_define_private_method_id(mrb, r, MRB_SYM(initialize_copy), range_initialize_copy, MRB_ARGS_REQ(1)); /* 15.2.14.4.15(x) */
  mrb_define_method_id(mrb, r, MRB_SYM(__num_to_a),      range_num_to_a,        MRB_ARGS_NONE());
}
