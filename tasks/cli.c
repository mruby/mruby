#include <stdio.h>
#include <stdlib.h>

#include "mruby.h"
#include "mruby/array.h"
#include "mruby/irep.h"
#include "mruby/variable.h"

/*
  irep that will be generated from *.rb files
  if there is main.rb in it, main.rb must be loaded after all other scripts is loaded
*/
extern const uint8_t mrb_main_irep[];

int
main(int argc, char **argv)
{
  mrb_state* mrb = NULL;
  mrb_value ARGV;
  int i;
  mrb_value result;
  int exit_result = EXIT_SUCCESS;

  /* open mrb_state */
  mrb = mrb_open();
  if (mrb == NULL) {
    fputs("cannot open mrb_state\n", stderr);
    return EXIT_FAILURE;
  }

  /* set 'ARGV' */
  ARGV = mrb_ary_new_capa(mrb, argc - 1);
  for (i = 1; i < argc; ++i) {
    mrb_ary_push(mrb, ARGV, mrb_str_new_cstr(mrb, argv[i]));
  }
  mrb_define_global_const(mrb, "ARGV", ARGV);

  /* set '$0'-> argv[0] */
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$0"), mrb_str_new_cstr(mrb, argv[0]));

  /* load and run script */
  result = mrb_load_irep(mrb, mrb_main_irep);

  /* check error */
  if (mrb->exc) {
    mrb_print_error(mrb);
    exit_result = EXIT_FAILURE;
  }

  /* close mrb_state */
  mrb_close(mrb);

  return exit_result;
}
