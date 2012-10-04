/*
** init_ext.c - initialize extend libraries
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"

void mrb_init_mrblib_ext(mrb_state*);

void
mrb_init_ext(mrb_state *mrb)
{
#ifdef ENABLE_ERRNO
  extern void mrb_init_errno(mrb_state *mrb);
  mrb_init_errno(mrb);
#endif
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
#ifdef ENABLE_DIGEST
  extern void mrb_init_digest(mrb_state *mrb);
  mrb_init_digest(mrb);
#endif
#ifdef ENABLE_ENV
  extern void mrb_init_env(mrb_state *mrb);
  mrb_init_env(mrb);
#endif
#ifdef ENABLE_PACK
  extern void mrb_init_pack(mrb_state *mrb);
  mrb_init_pack(mrb);
#endif
#ifdef ENABLE_SYSLOG
  extern void mrb_init_syslog(mrb_state *mrb);
  mrb_init_syslog(mrb);
#endif
  mrb_gc_arena_restore(mrb, 0);

  mrb_init_mrblib_ext(mrb);
}
