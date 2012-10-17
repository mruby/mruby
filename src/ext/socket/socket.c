/*
** socket.c - Socket classes
**
** See Copyright Notice in mruby.h
*/
// TODO: Addrinfo

#include "mruby.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
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
sa2addrlist(mrb_state *mrb, const struct sockaddr *sa, socklen_t salen)
{
  mrb_value ary, host;
  unsigned short port;
  const char *afstr;

  switch (sa->sa_family) {
  case AF_INET:
    afstr = "AF_INET";
    port = ((struct sockaddr_in *)sa)->sin_port;
    break;
  case AF_INET6:
    afstr = "AF_INET6";
    port = ((struct sockaddr_in6 *)sa)->sin6_port;
    break;
  default:
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad af");
    return mrb_nil_value();
  }
  port = ntohs(port);
  host = mrb_str_buf_new(mrb, 256);
  if (getnameinfo(sa, salen, RSTRING_PTR(host), 256, NULL, 0, NI_NUMERICHOST) == -1)
    mrb_sys_fail(mrb, "getnameinfo");
  mrb_str_resize(mrb, host, strlen(RSTRING_PTR(host)));
  ary = mrb_ary_new_capa(mrb, 4);
  mrb_ary_push(mrb, ary, mrb_str_new2(mrb, afstr));
  mrb_ary_push(mrb, ary, mrb_fixnum_value(port));
  mrb_ary_push(mrb, ary, host);
  mrb_ary_push(mrb, ary, host);
  return ary;
}

static int
socket_fd(mrb_value sock)
{
  return fileno(RFILE(sock)->fptr->f);
}

static mrb_value
mrb_basicsocket_recv(mrb_state *mrb, mrb_value self)
{ 
  int n, s;
  mrb_int maxlen, flags = 0;
  mrb_value buf;

  mrb_get_args(mrb, "i|i", &maxlen, &flags);
  s = socket_fd(self);

  buf = mrb_str_buf_new(mrb, maxlen);
  n = recv(s, RSTRING_PTR(buf), maxlen, flags);
  if (n == -1)
    mrb_sys_fail(mrb, "recv");
  mrb_str_resize(mrb, buf, n);
  return buf;
}

static mrb_value
mrb_basicsocket_setnonblock(mrb_state *mrb, mrb_value self)
{ 
  int fd, flags;
  mrb_value bool;

  mrb_get_args(mrb, "o", &bool);
  fd = socket_fd(self);

  flags = fcntl(fd, F_GETFL, 0);
  if (flags == 1)
    mrb_sys_fail(mrb, "fcntl");
  if (mrb_test(bool))
    flags |= O_NONBLOCK;
  else
    flags &= ~O_NONBLOCK;
  if (fcntl(fd, F_SETFL, flags) == -1)
    mrb_sys_fail(mrb, "fcntl");
  return mrb_nil_value();
}


static mrb_value
mrb_basicsocket_shutdown(mrb_state *mrb, mrb_value self)
{ 
  struct mrb_io *fptr;
  mrb_int how = SHUT_RDWR;

  fptr = RFILE(self)->fptr;
  rb_io_check_closed(mrb, fptr);
  mrb_get_args(mrb, "|i", &how);
  if (shutdown(fileno(fptr->f), how) != 0)
    mrb_sys_fail(mrb, "shutdown");
  return mrb_fixnum_value(0);
}

static mrb_value
mrb_ipsocket_ntop(mrb_state *mrb, mrb_value klass)
{ 
  mrb_int af, n;
  char *addr, buf[50];

  mrb_get_args(mrb, "is", &af, &addr, &n);
  if ((af == AF_INET && n != 4) || (af == AF_INET6 && n != 16))
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
  if (inet_ntop(af, addr, buf, sizeof(buf)) == NULL)
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
mrb_ipsocket_recvfrom(mrb_state *mrb, mrb_value self)
{ 
  struct mrb_io *fptr;
  struct sockaddr_storage ss;
  socklen_t socklen;
  mrb_value a, buf, pair;
  mrb_int flags, maxlen, n;

  fptr = RFILE(self)->fptr;
  rb_io_check_closed(mrb, fptr);

  flags = 0;
  mrb_get_args(mrb, "i|i", &maxlen, &flags);
  //fprintf(stderr, "maxlen=%d, flags=%d\n", maxlen, flags);
  buf = mrb_str_buf_new(mrb, maxlen);
  socklen = sizeof(ss);
  n = recvfrom(fileno(fptr->f), RSTRING_PTR(buf), maxlen, flags,
  	       (struct sockaddr *)&ss, &socklen);
  if (n == -1)
    mrb_sys_fail(mrb, "recvfrom");
  mrb_str_resize(mrb, buf, n);
  a = sa2addrlist(mrb, (struct sockaddr *)&ss, socklen);
  pair = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, pair, buf);
  mrb_ary_push(mrb, pair, a);
  return pair;
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
mrb_udpsocket_open(mrb_state *mrb, mrb_value klass)
{
  struct RFile *f;
  struct mrb_io *io;
  mrb_int af = AF_INET;
  int s;
  mrb_value argv[3], so;

  mrb_get_args(mrb, "|i", &af);
  s = socket(af, SOCK_DGRAM, 0);
  if (s == -1)
      mrb_sys_fail(mrb, "socket");

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
mrb_udpsocket_bind(mrb_state *mrb, mrb_value self)
{ 
  struct addrinfo hints, *res;
  mrb_int n, port;
  int error, s;
  char *host, hostbuf[256], portbuf[6];

  s = socket_fd(self);
  mrb_get_args(mrb, "si", &host, &n, &port);

  //if (n > sizeof(host))
  memcpy(hostbuf, host, n);
  hostbuf[n] = '\0';

  memset(&hints, 0, sizeof(hints));
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_flags = AI_NUMERICSERV|AI_PASSIVE;
  snprintf(portbuf, sizeof(portbuf), "%d", port);
  error = getaddrinfo(hostbuf, portbuf, &hints, &res);
  if (error == -1)
    mrb_raise(mrb, E_RUNTIME_ERROR, "getaddrinfo(2) failed");
  if (bind(s, res->ai_addr, res->ai_addrlen) == -1) {
    freeaddrinfo(res);
    mrb_raise(mrb, E_RUNTIME_ERROR, "getaddrinfo(2) failed");
  }
  freeaddrinfo(res);
  return mrb_fixnum_value(0);
}

static mrb_value
mrb_udpsocket_connect(mrb_state *mrb, mrb_value self)
{ 
  struct addrinfo hints, *res;
  mrb_int n, port;
  int error, s;
  char *host, hostbuf[256], portbuf[6];

  mrb_get_args(mrb, "si", &host, &n, &port);
  s = socket_fd(self);

  //if (n > sizeof(host))
  memcpy(hostbuf, host, n);
  hostbuf[n] = '\0';

  memset(&hints, 0, sizeof(hints));
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_flags = AI_NUMERICSERV;
  snprintf(portbuf, sizeof(portbuf), "%d", port);
  error = getaddrinfo(hostbuf, portbuf, &hints, &res);
  if (error == -1)
    mrb_raise(mrb, E_RUNTIME_ERROR, "getaddrinfo(2) failed");
  if (connect(s, res->ai_addr, res->ai_addrlen) == -1) {
    freeaddrinfo(res);
    mrb_raise(mrb, E_RUNTIME_ERROR, "getaddrinfo(2) failed");
  }
  freeaddrinfo(res);
  return mrb_fixnum_value(0);
}

static mrb_value
mrb_udpsocket_send(mrb_state *mrb, mrb_value self)
{ 
  struct addrinfo hints, *res;
  mrb_int n, flags;
  mrb_value host, port;
  int argc, hlen, s;
  char hostbuf[256], *msg, portbuf[6];

  argc = mrb_get_args(mrb, "si|oo", &msg, &hlen, &flags, &host, &port);
  s = socket_fd(self);
  n = -1;
  if (argc == 2) {
    n = send(s, msg, hlen, flags);
  } else if (argc == 4) {
    memset(&hints, 0, sizeof(hints));
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_flags = AI_NUMERICHOST|AI_NUMERICSERV;
    memcpy(hostbuf, RSTRING_PTR(host), RSTRING_LEN(host));
    hostbuf[RSTRING_LEN(host)] = '\0';
    snprintf(portbuf, sizeof(portbuf), "%u", mrb_fixnum(port));
    if (getaddrinfo(hostbuf, portbuf, &hints, &res) == -1)
      mrb_sys_fail(mrb, "getaddrinfo");
    // XXX: try all addresses
    n = sendto(s, msg, hlen, flags, res->ai_addr, res->ai_addrlen);
    freeaddrinfo(res);
  }
  if (n == -1)
    mrb_sys_fail(mrb, "send");
  return mrb_fixnum_value(n);
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
  struct RClass *io, *sock, *bsock, *ipsock, *tcpsock, *udpsock, *usock;

  sock = mrb_define_module(mrb, "Socket");

#define define_const(SYM) \
  do {								\
    mrb_define_const(mrb, sock, #SYM, mrb_fixnum_value(SYM));	\
  } while (0)

#include "const.cstub"

  io = mrb_class_obj_get(mrb, "IO");

  bsock = mrb_define_class(mrb, "BasicSocket", io);
  // .for_fd
  // #getpeername
  // #getsockname
  //mrb_define_method(mrb, bsock, "getsockopt", mrb_basicsocket_getsockopt, ARGS_REQ(2));
  // #recv(maxlen, flags=0)
  mrb_define_method(mrb, bsock, "recv", mrb_basicsocket_recv, ARGS_REQ(1)|ARGS_OPT(1));
  // #recv_nonblock(maxlen, flags=0)
  // #recvmsg(maxlen, flags=0)
  // #recvmsg_nonblock(maxlen, flags=0)
  // #send
  // #sendmsg
  // #sendmsg_nonblock
  // #setsockopt
  mrb_define_method(mrb, bsock, "setnonblock", mrb_basicsocket_setnonblock, ARGS_REQ(1));
  mrb_define_method(mrb, bsock, "shutdown", mrb_basicsocket_shutdown, ARGS_OPT(1));

  ipsock = mrb_define_class(mrb, "IPSocket", bsock);
  mrb_define_class_method(mrb, ipsock, "ntop", mrb_ipsocket_ntop, ARGS_REQ(1));
  mrb_define_class_method(mrb, ipsock, "pton", mrb_ipsocket_pton, ARGS_REQ(2));
  mrb_define_method(mrb, ipsock, "recvfrom", mrb_ipsocket_recvfrom, ARGS_REQ(1)|ARGS_OPT(1));

  tcpsock = mrb_define_class(mrb, "TCPSocket", ipsock);
  mrb_define_class_method(mrb, tcpsock, "open", mrb_tcpsocket_open, ARGS_REQ(2));
  mrb_define_class_method(mrb, tcpsock, "new", mrb_tcpsocket_open, ARGS_REQ(2));
  // who uses gethostbyname...?

  udpsock = mrb_define_class(mrb, "UDPSocket", ipsock);
  mrb_define_class_method(mrb, udpsock, "new", mrb_udpsocket_open, ARGS_OPT(1));
  mrb_define_class_method(mrb, udpsock, "open", mrb_udpsocket_open, ARGS_OPT(1));
  mrb_define_method(mrb, udpsock, "bind", mrb_udpsocket_bind, ARGS_REQ(2));
  mrb_define_method(mrb, udpsock, "connect", mrb_udpsocket_connect, ARGS_REQ(2));
  //#recvfrom_nonblock
  mrb_define_method(mrb, udpsock, "send", mrb_udpsocket_send, ARGS_REQ(2)|ARGS_OPT(2));

  usock = mrb_define_class(mrb, "UNIXSocket", io);
  mrb_define_class_method(mrb, usock, "new", mrb_unixsocket_open, ARGS_REQ(1));
  mrb_define_class_method(mrb, usock, "open", mrb_unixsocket_open, ARGS_REQ(1));
  //mrb_define_class_method(mrb, usock, "pair", mrb_unixsocket_open, ARGS_OPT(2));
  //mrb_define_class_method(mrb, usock, "socketpair", mrb_unixsocket_open, ARGS_OPT(2));

  mrb_define_method(mrb, usock, "addr", mrb_unixsocket_addr, ARGS_NONE());
  mrb_define_method(mrb, usock, "peeraddr", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "recv_io", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "recvfrom", mrb_unixsocket_peeraddr, ARGS_NONE());
  //mrb_define_method(mrb, usock, "send_io", mrb_unixsocket_peeraddr, ARGS_NONE());
}
