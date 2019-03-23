/*
** mrbtest - Test for Embeddable Ruby
**
** This program runs Ruby test programs in test/t directory
** against the current mruby implementation.
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/array.h>

extern const uint8_t mrbtest_assert_irep[];

void mrbgemtest_init(mrb_state* mrb);

/* Print a short remark for the user */
static void
print_hint(void)
{
  printf("mrbtest - Embeddable Ruby Test\n\n");
}

static int
eval_test(mrb_state *mrb)
{
  /* evaluate the test */
  mrb_value result = mrb_funcall(mrb, mrb_top_self(mrb), "report", 0);
  /* did an exception occur? */
  if (mrb->exc) {
    mrb_print_error(mrb);
    mrb->exc = 0;
    return EXIT_FAILURE;
  }
  else {
    return mrb_bool(result) ? EXIT_SUCCESS : EXIT_FAILURE;
  }
}

/* Implementation of print due to the reason that there might be no print */
static mrb_value
t_print(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*!", &argv, &argc);
  for (mrb_int i = 0; i < argc; ++i) {
    mrb_value s = mrb_obj_as_string(mrb, argv[i]);
    fwrite(RSTRING_PTR(s), RSTRING_LEN(s), 1, stdout);
  }
  fflush(stdout);

  return mrb_nil_value();
}

void
mrb_init_test_driver(mrb_state *mrb, mrb_bool verbose)
{
  struct RClass *krn, *mrbtest;

  krn = mrb->kernel_module;
  mrb_define_method(mrb, krn, "t_print", t_print, MRB_ARGS_ANY());

  mrbtest = mrb_define_module(mrb, "Mrbtest");

  mrb_define_const(mrb, mrbtest, "FIXNUM_MAX", mrb_fixnum_value(MRB_INT_MAX));
  mrb_define_const(mrb, mrbtest, "FIXNUM_MIN", mrb_fixnum_value(MRB_INT_MIN));
  mrb_define_const(mrb, mrbtest, "FIXNUM_BIT", mrb_fixnum_value(MRB_INT_BIT));

#ifndef MRB_WITHOUT_FLOAT
#ifdef MRB_USE_FLOAT
  mrb_define_const(mrb, mrbtest, "FLOAT_TOLERANCE", mrb_float_value(mrb, 1e-6));
#else
  mrb_define_const(mrb, mrbtest, "FLOAT_TOLERANCE", mrb_float_value(mrb, 1e-12));
#endif
#endif

  if (verbose) {
    mrb_gv_set(mrb, mrb_intern_lit(mrb, "$mrbtest_verbose"), mrb_true_value());
  }
}

void
mrb_t_pass_result(mrb_state *mrb_dst, mrb_state *mrb_src)
{
  mrb_value res_src;

  if (mrb_src->exc) {
    mrb_print_error(mrb_src);
    exit(EXIT_FAILURE);
  }

#define TEST_COUNT_PASS(name)                                           \
  do {                                                                  \
    res_src = mrb_gv_get(mrb_src, mrb_intern_lit(mrb_src, "$" #name));  \
    if (mrb_fixnum_p(res_src)) {                                        \
      mrb_value res_dst = mrb_gv_get(mrb_dst, mrb_intern_lit(mrb_dst, "$" #name)); \
      mrb_gv_set(mrb_dst, mrb_intern_lit(mrb_dst, "$" #name), mrb_fixnum_value(mrb_fixnum(res_dst) + mrb_fixnum(res_src))); \
    }                                                                   \
  } while (FALSE)                                                       \

  TEST_COUNT_PASS(ok_test);
  TEST_COUNT_PASS(ko_test);
  TEST_COUNT_PASS(kill_test);
  TEST_COUNT_PASS(skip_test);

#undef TEST_COUNT_PASS

  res_src = mrb_gv_get(mrb_src, mrb_intern_lit(mrb_src, "$asserts"));

  if (mrb_array_p(res_src)) {
    mrb_int i;
    mrb_value res_dst = mrb_gv_get(mrb_dst, mrb_intern_lit(mrb_dst, "$asserts"));
    for (i = 0; i < RARRAY_LEN(res_src); ++i) {
      mrb_value val_src = RARRAY_PTR(res_src)[i];
      mrb_ary_push(mrb_dst, res_dst, mrb_str_new(mrb_dst, RSTRING_PTR(val_src), RSTRING_LEN(val_src)));
    }
  }
}

int
main(int argc, char **argv)
{
  mrb_state *mrb;
  int ret;
  mrb_bool verbose = FALSE;

  print_hint();

  /* new interpreter instance */
  mrb = mrb_open();
  if (mrb == NULL) {
    fprintf(stderr, "Invalid mrb_state, exiting test driver");
    return EXIT_FAILURE;
  }

  if (argc == 2 && argv[1][0] == '-' && argv[1][1] == 'v') {
    printf("verbose mode: enable\n\n");
    verbose = TRUE;
  }

  mrb_init_test_driver(mrb, verbose);
  mrb_load_irep(mrb, mrbtest_assert_irep);
  mrbgemtest_init(mrb);
  ret = eval_test(mrb);
  mrb_close(mrb);

  return ret;
}
