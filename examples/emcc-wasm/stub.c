#include <mruby.h>
#include <mruby/irep.h>
#include CFILE

int
main(void)
{
  mrb_state *mrb = mrb_open();
  if (!mrb) { /* handle error */ }
  mrb_load_irep(mrb, test_symbol);
  mrb_close(mrb);
  return 0;
}