/*
** mrbtest - Test for Embeddable Ruby
**
** This program runs Ruby test programs in test/t directory
** against the current mruby implementation.
*/

#include <string.h>

#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>

void
mrb_init_mrbtest(mrb_state *);

/* Print a short remark for the user */
void print_hint(void)
{
  printf("mrbtest - Embeddable Ruby Test\n");
  printf("\nThis is a very early version, please test and report errors.\n");
  printf("Thanks :)\n\n");
}

int
main(void)
{
  struct mrb_parser_state *parser;
  mrb_state *mrb;
  mrb_value return_value;
  int byte_code;
  const char *prog = "report()";

  print_hint();

  /* new interpreter instance */
  mrb = mrb_open();
  if (mrb == NULL) {
    fprintf(stderr, "Invalid mrb_state, exiting test driver");
    return EXIT_FAILURE;
  }

  mrb_init_mrbtest(mrb);
  /* evaluate the test */
  return_value = mrb_load_string(mrb, prog);
  /* did an exception occur? */
  if (mrb->exc) {
    mrb_p(mrb, return_value);
    mrb->exc = 0;
  }
  else {
    /* no */
  }
  mrb_close(mrb);

  return 0;
}
