/*
** socket.c - Socket classes
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stddef.h>
#include <string.h>
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include "mruby/ext/io.h"

static mrb_value
mrb_ipsocket_ntop(mrb_state *mrb, mrb_value klass)
{ 
  mrb_int af, n;
  char *addr, buf[50];

  mrb_get_args(mrb, "is", &af, &addr, &n);
  if (n != 4 || inet_ntop(af, addr, buf, sizeof(buf)) == NULL)
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
  return mrb_str_new2(mrb, buf);
}

static mrb_value
mrb_ipsocket_pton(mrb_state *mrb, mrb_value klass)
{ 
  mrb_int af, n;
  char *bp, buf[50];

  mrb_get_args(mrb, "is", &af, &bp, &n);
  if (n > sizeof(buf) - 1)
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
  memcpy(buf, bp, n);
  buf[n] = '\0';

  if (af == AF_INET) {
    struct in_addr in;
    if (inet_pton(AF_INET, buf, (void *)&in.s_addr) != 1)
      goto invalid;
    return mrb_str_new(mrb, (char *)&in.s_addr, 4);
  } else if (af == AF_INET6) {
    struct in6_addr in6;
    if (inet_pton(AF_INET6, buf, (void *)&in6.s6_addr) != 1)
      goto invalid;
    return mrb_str_new(mrb, (char *)&in6.s6_addr, 16);
  } else
    mrb_raise(mrb, E_ARGUMENT_ERROR, "unsupported address family");

invalid:
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
  return mrb_nil_value(); /* dummy */
}

static mrb_value
mrb_tcpsocket_open(mrb_state *mrb, mrb_value klass)
{ 
  struct RFile *f;
  struct addrinfo hints, *res, *res0;
  struct mrb_io *io;
  mrb_value argv[3], servo, so;
  char *host;
  int error, s;

  mrb_get_args(mrb, "zo", &host, &servo);
  if (mrb_type(servo) != MRB_TT_STRING) {
    if (!mrb_respond_to(mrb, servo, mrb_intern(mrb, "to_s"))) {
      mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, servo));
    }
    servo = mrb_funcall(mrb, servo, "to_s", 0);
  }

  memset(&hints, 0, sizeof(hints));
  hints.ai_socktype = SOCK_STREAM;
  error = getaddrinfo(host, RSTRING_PTR(servo), &hints, &res0);
  if (error == -1)
    mrb_raise(mrb, E_RUNTIME_ERROR, "getaddrinfo(2) failed");
  res = res0;

  s = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if (s == -1) {
    freeaddrinfo(res0);
    mrb_raise(mrb, E_RUNTIME_ERROR, "socket(2) failed");
  }

  if (connect(s, res->ai_addr, res->ai_addrlen) == -1) {
    freeaddrinfo(res0);
    mrb_raise(mrb, E_RUNTIME_ERROR, "connect(2) failed");
  }
  freeaddrinfo(res0);

  f = (struct RFile *)mrb_obj_alloc(mrb, MRB_TT_FILE, mrb_class_ptr(klass));
  f->fptr = NULL;
  so = mrb_obj_value(f);

  argv[0] = mrb_fixnum_value(s);
  argv[1] = mrb_fixnum_value(O_RDWR);
  argv[2] = mrb_nil_value();
  rb_io_initialize(mrb, 2, argv, so);

  io = RFILE(so)->fptr;
  io->path = mrb_str_new_cstr(mrb, "");

  return so;
}


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
  struct RClass *io, *sock, *ipsock, *tcpsock, *usock;

  sock = mrb_define_module(mrb, "Socket");

#define define_const(SYM) \
  do {								\
    mrb_define_const(mrb, sock, #SYM, mrb_fixnum_value(SYM));	\
  } while (0)

#include "const.cstub"

  io = mrb_class_obj_get(mrb, "IO");

  ipsock = mrb_define_class(mrb, "IPSocket", io);
  mrb_define_class_method(mrb, ipsock, "ntop", mrb_ipsocket_ntop, ARGS_REQ(1));
  mrb_define_class_method(mrb, ipsock, "pton", mrb_ipsocket_pton, ARGS_REQ(2));

  tcpsock = mrb_define_class(mrb, "TCPSocket", io);
  mrb_define_class_method(mrb, tcpsock, "open", mrb_tcpsocket_open, ARGS_REQ(2));
  mrb_define_class_method(mrb, tcpsock, "new", mrb_tcpsocket_open, ARGS_REQ(2));
  // who uses gethostbyname...?

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
