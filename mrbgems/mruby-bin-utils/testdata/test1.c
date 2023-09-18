#include <mruby.h>

int
main(int argc, char **argv)
{
  mrb_state *mrb = mrb_open();
  mrb_close(mrb);
  return 0;
}
