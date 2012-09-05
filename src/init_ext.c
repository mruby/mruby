/*
** init_ext.c - initialize extend libraries
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"

void
mrb_init_ext(mrb_state *mrb)
{
#ifdef ENABLE_IO
  extern void mrb_init_io(mrb_state *mrb);
  mrb_init_io(mrb);
  extern void mrb_init_file(mrb_state *mrb);
  mrb_init_file(mrb);
#ifdef ENABLE_SOCKET
  extern void mrb_init_socket(mrb_state *mrb);
  mrb_init_socket(mrb);
#endif
#endif
#ifdef ENABLE_PROCESS
  extern void mrb_init_process(mrb_state *mrb);
  mrb_init_process(mrb);
#endif
}
