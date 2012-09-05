/*
** socket.c - Socket classes
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stddef.h>
#include <string.h>
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include "mruby/ext/io.h"

#include <err.h>	// for debug

static mrb_value
mrb_unixsocket_open(mrb_state *mrb, mrb_value klass)
{ 
  struct RFile *f;
  struct mrb_io *io;
  struct sockaddr_un sun;
  mrb_value argv[3], us;
  char *path;
  int pathlen, s;

  mrb_get_args(mrb, "s", &path, &pathlen);
  if (pathlen > sizeof(sun.sun_path))
    mrb_raise(mrb, E_ARGUMENT_ERROR, "too long pathname");

  s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s == -1)
    mrb_raise(mrb, E_RUNTIME_ERROR, "socket(2) failed");

  memset(&sun, 0, sizeof(sun));
  sun.sun_family = AF_UNIX;
  strncpy(sun.sun_path, path, sizeof(sun.sun_path));
  if (connect(s, (struct sockaddr *)&sun, sizeof(sun)) == -1)
    mrb_raise(mrb, E_RUNTIME_ERROR, "connect(2) failed");

  f = (struct RFile *)mrb_obj_alloc(mrb, MRB_TT_FILE, mrb_class_ptr(klass));
  f->fptr = NULL;
  us = mrb_obj_value(f);

  argv[0] = mrb_fixnum_value(s);
  argv[1] = mrb_fixnum_value(O_RDWR);
  argv[2] = mrb_nil_value();
  rb_io_initialize(mrb, 2, argv, us);

  io = RFILE(us)->fptr;
  io->path = mrb_str_new_cstr(mrb, "");

  return us;
}

static mrb_value
mrb_unixsocket_addr(mrb_state *mrb, mrb_value self)
{
  struct mrb_io *fptr;
  mrb_value ary, path;

  fptr = RFILE(self)->fptr;
  mrb_io_check_initialized(mrb, fptr);
  if (mrb_nil_p(fptr->path))
    path = mrb_nil_value();
  else
    path = mrb_str_dup(mrb, fptr->path);
  ary = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, ary, mrb_str_new2(mrb, "AF_UNIX"));
  mrb_ary_push(mrb, ary, path);
  return ary;
}

static mrb_value
mrb_unixsocket_peeraddr(mrb_state *mrb, mrb_value self)
{
  struct sockaddr_un sun;
  struct mrb_io *fptr;
  mrb_value ary;
  socklen_t len;

  fptr = RFILE(self)->fptr;
  mrb_io_check_initialized(mrb, fptr);

  memset(&sun, 0, sizeof(sun));
  len = sizeof(sun);
  if (getpeername(fileno(fptr->f), (struct sockaddr *)&sun, &len) == -1)
    mrb_raise(mrb, E_ARGUMENT_ERROR, "getpeername failed");

  ary = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, ary, mrb_str_new2(mrb, "AF_UNIX"));
  mrb_ary_push(mrb, ary, mrb_str_new2(mrb, sun.sun_path));
  return ary;
}

void
mrb_init_socket(mrb_state *mrb)
{
  struct RClass *usock, *io;

  io = mrb_class_obj_get(mrb, "IO");

  usock = mrb_define_class(mrb, "UNIXSocket", io);
  mrb_define_class_method(mrb, usock, "open", mrb_unixsocket_open, ARGS_REQ(1));
  mrb_define_class_method(mrb, usock, "new", mrb_unixsocket_open, ARGS_REQ(1));
  //mrb_define_class_method(mrb, usock, "pair", mrb_unixsocket_open, ARGS_OPT(2));
  //mrb_define_class_method(mrb, usock, "socketpair", mrb_unixsocket_open, ARGS_OPT(2));

  mrb_define_method(mrb, usock, "addr", mrb_unixsocket_addr, ARGS_NONE());
  mrb_define_method(mrb, usock, "peeraddr", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "recv_io", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "recvfrom", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "send_io", mrb_unixsocket_peeraddr, ARGS_NONE());
}
