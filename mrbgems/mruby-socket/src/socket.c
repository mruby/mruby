/*
** socket.c - Socket module
**
** See Copyright Notice in mruby.h
*/

#ifdef _WIN32
  #define _WIN32_WINNT 0x0501
  #include <winsock2.h>
  #include <ws2tcpip.h>
  #include <windows.h>
  #define SHUT_RDWR SD_BOTH
  typedef int fsize_t;
#else
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <sys/param.h>
  #include <sys/un.h>
  #include <netinet/in.h>
  #include <netinet/tcp.h>
  #include <arpa/inet.h>
  #include <fcntl.h>
  #include <netdb.h>
  #include <unistd.h>
  typedef size_t fsize_t;
#endif

#include <string.h>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#include <mruby/ext/io.h>
#include "socket_hal.h"

/* Address family information for compact lookup table */
typedef struct {
  int family;                 /* AF_INET, AF_INET6, etc. */
  const char *name;           /* "AF_INET", "AF_INET6", etc. */
  int port_offset;            /* Offset to port field in sockaddr structure */
  mrb_bool has_port;          /* TRUE if this family has a port field */
} af_info_t;

/* Protocol family lookup table for socket option inspection */
typedef struct {
  int family;                 /* PF_INET, PF_INET6, etc. */
  const char *name;           /* "INET", "INET6", etc. */
} pf_info_t;

/* Compact address family lookup table (memory-efficient) */
static const af_info_t af_table[] = {
  /* Internet Protocol families with port numbers */
  {AF_INET,  "AF_INET",  offsetof(struct sockaddr_in, sin_port),   TRUE},
  {AF_INET6, "AF_INET6", offsetof(struct sockaddr_in6, sin6_port), TRUE},

  /* Local/Unix domain sockets without port numbers */
#ifdef AF_UNIX
  {AF_UNIX,  "AF_UNIX",  -1,                                       FALSE},
#endif
#ifdef AF_LOCAL
  {AF_LOCAL, "AF_LOCAL", -1,                                       FALSE},
#endif

  /* Additional protocol families (platform-dependent) */
#ifdef AF_LINK
  {AF_LINK,  "AF_LINK",  -1,                                       FALSE},
#endif
#ifdef AF_ROUTE
  {AF_ROUTE, "AF_ROUTE", -1,                                       FALSE},
#endif
#ifdef AF_UNSPEC
  {AF_UNSPEC, "AF_UNSPEC", -1,                                     FALSE},
#endif
};

#define AF_TABLE_SIZE (sizeof(af_table) / sizeof(af_table[0]))

/* Get address family info for given family constant (compact linear search) */
static inline const af_info_t *get_af_info(int family) {
  for (size_t i = 0; i < AF_TABLE_SIZE; i++) {
    if (af_table[i].family == family) {
      return &af_table[i];
    }
  }
  return NULL;
}

/* Compact protocol family lookup table (memory-efficient) */
static const pf_info_t pf_table[] = {
  {PF_INET,  "INET"},
#ifdef PF_INET6
  {PF_INET6, "INET6"},
#endif
#ifdef PF_IPX
  {PF_IPX,   "IPX"},
#endif
#ifdef PF_AX25
  {PF_AX25,  "AX25"},
#endif
#ifdef PF_APPLETALK
  {PF_APPLETALK, "APPLETALK"},
#endif
#ifdef PF_UNIX
  {PF_UNIX,  "UNIX"},
#endif
};

#define PF_TABLE_SIZE (sizeof(pf_table) / sizeof(pf_table[0]))

/* Get protocol family name for given family constant (compact linear search) */
static inline const char *get_pf_name(int family) {
  for (size_t i = 0; i < PF_TABLE_SIZE; i++) {
    if (pf_table[i].family == family) {
      return pf_table[i].name;
    }
  }
  return NULL;
}

#if !defined(HAVE_SA_LEN)
#if (defined(BSD) && (BSD >= 199006))
#define HAVE_SA_LEN  1
#else
#define HAVE_SA_LEN  0
#endif
#endif

#define E_SOCKET_ERROR             mrb_class_get_id(mrb, MRB_SYM(SocketError))

struct gen_addrinfo_args {
  struct RClass *klass;
  struct addrinfo *addrinfo;
};

/* Helper to generate array of Addrinfo objects from addrinfo linked list */
static mrb_value
gen_addrinfo(mrb_state *mrb, mrb_value args)
{
  mrb_value ary = mrb_ary_new(mrb);
  int arena_idx = mrb_gc_arena_save(mrb);  /* ary must be on arena! */
  struct gen_addrinfo_args *a = (struct gen_addrinfo_args*)mrb_cptr(args);

  for (struct addrinfo *res = a->addrinfo; res != NULL; res = res->ai_next) {
    mrb_value sa = mrb_str_new(mrb, (char*)res->ai_addr, res->ai_addrlen);
    mrb_value args[4] = {sa, mrb_fixnum_value(res->ai_family), mrb_fixnum_value(res->ai_socktype), mrb_fixnum_value(res->ai_protocol)};
    mrb_value ai = mrb_obj_new(mrb, a->klass, 4, args);
    mrb_ary_push(mrb, ary, ai);
    mrb_gc_arena_restore(mrb, arena_idx);
  }
  return ary;
}

/* Helper to free addrinfo structure - used with mrb_ensure */
static mrb_value
free_addrinfo(mrb_state *mrb, mrb_value addrinfo)
{
  freeaddrinfo((struct addrinfo*)mrb_cptr(addrinfo));
  return mrb_nil_value();
}

/*
 * call-seq:
 *   Addrinfo.getaddrinfo(nodename, servname, family=nil, socktype=nil, protocol=nil, flags=0) -> array
 *
 * Returns an array of Addrinfo objects for the given nodename and servname.
 *
 *   Addrinfo.getaddrinfo("localhost", "http")
 *   Addrinfo.getaddrinfo("www.example.com", 80, Socket::AF_INET)
 */
static mrb_value
mrb_addrinfo_getaddrinfo(mrb_state *mrb, mrb_value klass)
{
  struct addrinfo hints = {0}, *addr;
  mrb_value family, protocol, service, socktype;
  mrb_int flags = 0;
  const char *hostname;

  family = socktype = protocol = mrb_nil_value();
  mrb_get_args(mrb, "z!o|oooi", &hostname, &service, &family, &socktype, &protocol, &flags);

  const char *servname = NULL;
  if (mrb_string_p(service)) {
    servname = RSTRING_CSTR(mrb, service);
  }
  else if (mrb_integer_p(service)) {
    servname = RSTRING_PTR(mrb_integer_to_str(mrb, service, 10));
  }
  else if (mrb_nil_p(service)) {
    servname = NULL;
  }
  else {
    mrb_raise(mrb, E_TYPE_ERROR, "service must be String, Integer, or nil");
  }

  hints.ai_flags = (int)flags;

  if (mrb_integer_p(family)) {
    hints.ai_family = (int)mrb_integer(family);
  }

  if (mrb_integer_p(socktype)) {
    hints.ai_socktype = (int)mrb_integer(socktype);
  }

  if (mrb_integer_p(protocol)) {
    hints.ai_protocol = (int)mrb_integer(protocol);
  }

  int error = getaddrinfo(hostname, servname, &hints, &addr);
  if (error) {
    mrb_raisef(mrb, E_SOCKET_ERROR, "getaddrinfo: %s", gai_strerror(error));
  }

  struct gen_addrinfo_args args = {mrb_class_ptr(klass), addr};
  return mrb_ensure(mrb, gen_addrinfo, mrb_cptr_value(mrb, &args), free_addrinfo, mrb_cptr_value(mrb, addr));
}

/*
 * call-seq:
 *   addrinfo.getnameinfo(flags=0) -> [hostname, service]
 *
 * Returns the hostname and service name for the address.
 *
 *   addr.getnameinfo  #=> ["localhost", "http"]
 *   addr.getnameinfo(Socket::NI_NUMERICHOST)  #=> ["127.0.0.1", "80"]
 */
static mrb_value
mrb_addrinfo_getnameinfo(mrb_state *mrb, mrb_value self)
{
  mrb_int flags = 0;

  mrb_get_args(mrb, "|i", &flags);

  mrb_value host = mrb_str_new_capa(mrb, NI_MAXHOST);
  mrb_value serv = mrb_str_new_capa(mrb, NI_MAXSERV);
  mrb_value sastr = mrb_iv_get(mrb, self, MRB_IVSYM(sockaddr));
  if (!mrb_string_p(sastr)) {
    mrb_raise(mrb, E_SOCKET_ERROR, "invalid sockaddr");
  }
  int error = getnameinfo((struct sockaddr*)RSTRING_PTR(sastr), (socklen_t)RSTRING_LEN(sastr), RSTRING_PTR(host), NI_MAXHOST, RSTRING_PTR(serv), NI_MAXSERV, (int)flags);
  if (error) {
    mrb_raisef(mrb, E_SOCKET_ERROR, "getnameinfo: %s", gai_strerror(error));
  }
  mrb_value ary = mrb_ary_new_capa(mrb, 2);
  mrb_str_resize(mrb, host, strlen(RSTRING_PTR(host)));
  mrb_ary_push(mrb, ary, host);
  mrb_str_resize(mrb, serv, strlen(RSTRING_PTR(serv)));
  mrb_ary_push(mrb, ary, serv);
  return ary;
}

/*
 * call-seq:
 *   addrinfo.unix_path -> string
 *
 * Returns the Unix domain socket path.
 *
 *   addr.unix_path  #=> "/tmp/socket"
 */
static mrb_value
mrb_addrinfo_unix_path(mrb_state *mrb, mrb_value self)
{
  mrb_value sastr = mrb_iv_get(mrb, self, MRB_IVSYM(sockaddr));

  if (!mrb_string_p(sastr)) {
    mrb_raise(mrb, E_SOCKET_ERROR, "invalid sockaddr");
  }

  return mrb_hal_socket_unix_path(mrb, RSTRING_PTR(sastr), (size_t)RSTRING_LEN(sastr));
}

/* Helper to convert sockaddr to address list array [family, port, host, host] */
static mrb_value
sa2addrlist(mrb_state *mrb, const struct sockaddr *sa, socklen_t salen)
{
  /* Use lookup table for O(1) address family dispatch */
  const af_info_t *af_info = get_af_info(sa->sa_family);
  if (!af_info) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad af");
    return mrb_nil_value();
  }

  /* Extract port using table-driven offset calculation */
  unsigned short port = 0;
  if (af_info->has_port) {
    port = *(unsigned short*)((char*)sa + af_info->port_offset);
  }
  port = ntohs(port);
  mrb_value host = mrb_str_new_capa(mrb, NI_MAXHOST);
  if (getnameinfo(sa, salen, RSTRING_PTR(host), NI_MAXHOST, NULL, 0, NI_NUMERICHOST) == -1)
    mrb_sys_fail(mrb, "getnameinfo");
  mrb_str_resize(mrb, host, strlen(RSTRING_PTR(host)));

  mrb_value ary = mrb_ary_new_capa(mrb, 4);
  mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, af_info->name));
  mrb_ary_push(mrb, ary, mrb_fixnum_value(port));
  mrb_ary_push(mrb, ary, host);
  mrb_ary_push(mrb, ary, host);
  return ary;
}

int mrb_io_fileno(mrb_state *mrb, mrb_value io);

/* Helper to extract file descriptor from socket object */
static int
socket_fd(mrb_state *mrb, mrb_value sock)
{
  return mrb_io_fileno(mrb, sock);
}

/* Helper to get address family of socket by file descriptor */
static int
socket_family(int s)
{
  struct sockaddr_storage ss;
  socklen_t salen = sizeof(ss);

  if (getsockname(s, (struct sockaddr*)&ss, &salen) == -1)
    return AF_UNSPEC;
  return ss.ss_family;
}

/*
 * call-seq:
 *   basicsocket.getpeereid -> [euid, egid]
 *
 * Returns the effective user ID and group ID of the peer process.
 * Only available on systems that support getpeereid().
 *
 *   euid, egid = sock.getpeereid
 */
static mrb_value
mrb_basicsocket_getpeereid(mrb_state *mrb, mrb_value self)
{
#ifdef HAVE_GETPEEREID
  gid_t egid;
  uid_t euid;
  int s = socket_fd(mrb, self);
  if (getpeereid(s, &euid, &egid) != 0)
    mrb_sys_fail(mrb, "getpeereid");

  mrb_value ary = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int)euid));
  mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int)egid));
  return ary;
#else
  mrb_raise(mrb, E_RUNTIME_ERROR, "getpeereid is not available on this system");
  return mrb_nil_value();
#endif
}

/*
 * call-seq:
 *   basicsocket.getpeername -> string
 *
 * Returns the remote socket address as a packed sockaddr string.
 *
 *   sockaddr = sock.getpeername
 */
static mrb_value
mrb_basicsocket_getpeername(mrb_state *mrb, mrb_value self)
{
  struct sockaddr_storage ss;
  socklen_t salen = sizeof(ss);

  if (getpeername(socket_fd(mrb, self), (struct sockaddr*)&ss, &salen) != 0)
    mrb_sys_fail(mrb, "getpeername");

  return mrb_str_new(mrb, (char*)&ss, salen);
}

/*
 * call-seq:
 *   basicsocket.getsockname -> string
 *
 * Returns the local socket address as a packed sockaddr string.
 *
 *   sockaddr = sock.getsockname
 */
static mrb_value
mrb_basicsocket_getsockname(mrb_state *mrb, mrb_value self)
{
  struct sockaddr_storage ss;
  socklen_t salen = sizeof(ss);

  if (getsockname(socket_fd(mrb, self), (struct sockaddr*)&ss, &salen) != 0)
    mrb_sys_fail(mrb, "getsockname");

  return mrb_str_new(mrb, (char*)&ss, salen);
}

/* Helper to get Socket::Option class reference */
static struct RClass *
socket_option_class(mrb_state *mrb)
{
  return mrb_class_get_under_id(mrb, mrb_class_get_id(mrb, MRB_SYM(Socket)), MRB_SYM(Option));
}

/*
 * call-seq:
 *   Socket::Option.new(family, level, optname, data) -> socket_option
 *
 * Creates a new Socket::Option object with the given parameters.
 *
 *   opt = Socket::Option.new(Socket::AF_INET, Socket::SOL_SOCKET, Socket::SO_REUSEADDR, [1].pack("i"))
 */
static mrb_value
socket_option_init(mrb_state *mrb, mrb_value self)
{
  mrb_int family, level, optname;
  mrb_value data;

  mrb_get_args(mrb, "iiio", &family, &level, &optname, &data);
  mrb_iv_set(mrb, self, MRB_SYM(family), mrb_int_value(mrb, family));
  mrb_iv_set(mrb, self, MRB_SYM(level), mrb_int_value(mrb, level));
  mrb_iv_set(mrb, self, MRB_SYM(optname), mrb_int_value(mrb, optname));
  mrb_iv_set(mrb, self, MRB_SYM(data), data);

  return self;
}

/*
 * call-seq:
 *   Socket::Option.bool(family, level, optname, bool) -> socket_option
 *
 * Creates a new Socket::Option object from a boolean value.
 *
 *   opt = Socket::Option.bool(Socket::AF_INET, Socket::SOL_SOCKET, Socket::SO_REUSEADDR, true)
 */
static mrb_value
socket_option_s_bool(mrb_state *mrb, mrb_value klass)
{
  mrb_value args[4];
  mrb_bool data;

  mrb_get_args(mrb, "ooob", &args[0], &args[1], &args[2], &data);

  int tmp = (int)data;
  args[3] = mrb_str_new(mrb, (char*)&tmp, sizeof(int));
  return mrb_obj_new(mrb, mrb_class_ptr(klass), 4, args);
}

/*
 * call-seq:
 *   Socket::Option.int(family, level, optname, integer) -> socket_option
 *
 * Creates a new Socket::Option object from an integer value.
 *
 *   opt = Socket::Option.int(Socket::AF_INET, Socket::SOL_SOCKET, Socket::SO_KEEPALIVE, 1)
 */
static mrb_value
socket_option_s_int(mrb_state *mrb, mrb_value klass)
{
  mrb_value args[4];
  mrb_int data;

  mrb_get_args(mrb, "oooi", &args[0], &args[1], &args[2], &data);

  int tmp = (int)data;
  args[3] = mrb_str_new(mrb, (char*)&tmp, sizeof(int));
  return mrb_obj_new(mrb, mrb_class_ptr(klass), 4, args);
}

/*
 * call-seq:
 *   socket_option.family -> integer
 *
 * Returns the address family of the socket option.
 *
 *   opt.family  #=> Socket::AF_INET
 */
static mrb_value
socket_option_family(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, MRB_SYM(family));
}

/*
 * call-seq:
 *   socket_option.level -> integer
 *
 * Returns the protocol level of the socket option.
 *
 *   opt.level  #=> Socket::SOL_SOCKET
 */
static mrb_value
socket_option_level(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, MRB_SYM(level));
}

/*
 * call-seq:
 *   socket_option.optname -> integer
 *
 * Returns the option name of the socket option.
 *
 *   opt.optname  #=> Socket::SO_REUSEADDR
 */
static mrb_value
socket_option_optname(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, MRB_SYM(optname));
}

/*
 * call-seq:
 *   socket_option.data -> string
 *
 * Returns the raw data of the socket option as a string.
 *
 *   opt.data  #=> "\x01\x00\x00\x00"
 */
static mrb_value
socket_option_data(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, MRB_SYM(data));
}

/* Helper to extract integer value from Socket::Option data */
static int
option_int(mrb_state *mrb, mrb_value self)
{
  mrb_value data = mrb_obj_as_string(mrb, mrb_iv_get(mrb, self, MRB_SYM(data)));

  if (RSTRING_LEN(data) != sizeof(int)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "size differ; expected as sizeof(int)=%i but %i", (mrb_int)sizeof(int), RSTRING_LEN(data));
  }

  int tmp;
  memcpy((char*)&tmp, RSTRING_PTR(data), sizeof(int));
  return tmp;
}

/*
 * call-seq:
 *   socket_option.int -> integer
 *
 * Returns the socket option data as an integer value.
 *
 *   opt.int  #=> 1
 */
static mrb_value
socket_option_int(mrb_state *mrb, mrb_value self)
{
  int i = option_int(mrb, self);
  return mrb_int_value(mrb, (mrb_int)i);
}

/*
 * call-seq:
 *   socket_option.bool -> true or false
 *
 * Returns the socket option data as a boolean value.
 *
 *   opt.bool  #=> true
 */
static mrb_value
socket_option_bool(mrb_state *mrb, mrb_value self)
{
  int i = option_int(mrb, self);
  return mrb_bool_value((mrb_bool)i);
}

/* Helper to raise not implemented error for unimplemented Socket::Option methods */
static mrb_value
socket_option_notimp(mrb_state *mrb, mrb_value self)
{
  mrb_notimplement(mrb);
  return mrb_nil_value();
}

/*
 * call-seq:
 *   socket_option.inspect -> string
 *
 * Returns a string representation of the socket option for debugging.
 *
 *   opt.inspect  #=> "#<Socket::Option: INET level:1 optname:2 \"\\x01\\x00\\x00\\x00\">"
 */
static mrb_value
socket_option_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value str = mrb_str_new_cstr(mrb, "#<Socket::Option: ");

  mrb_value family = mrb_iv_get(mrb, self, MRB_SYM(family));
  const char *pf = NULL;

  if (mrb_integer_p(family)) {
    mrb_int fm = mrb_integer(family);
    pf = get_pf_name((int)fm);
  }

  if (pf) {
    mrb_str_cat_cstr(mrb, str, pf);
  }
  else {
    mrb_str_cat_cstr(mrb, str, "family:");
    mrb_str_cat_str(mrb, str, mrb_inspect(mrb, family));
  }
  mrb_str_cat_cstr(mrb, str, " level:");
  mrb_str_cat_str(mrb, str, mrb_inspect(mrb, mrb_iv_get(mrb, self, MRB_SYM(level))));
  mrb_str_cat_cstr(mrb, str, " optname:");
  mrb_str_cat_str(mrb, str, mrb_inspect(mrb, mrb_iv_get(mrb, self, MRB_SYM(optname))));
  mrb_str_cat_cstr(mrb, str, " ");
  mrb_str_cat_str(mrb, str, mrb_inspect(mrb, mrb_iv_get(mrb, self, MRB_SYM(data))));
  mrb_str_cat_cstr(mrb, str, ">");

  return str;
}

/*
 * call-seq:
 *   basicsocket.getsockopt(level, optname) -> string
 *
 * Gets a socket option. Returns the option value as a string.
 *
 *   val = sock.getsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR)
 */
static mrb_value
mrb_basicsocket_getsockopt(mrb_state *mrb, mrb_value self)
{
  mrb_int level, optname;

  mrb_get_args(mrb, "ii", &level, &optname);

  int s = socket_fd(mrb, self);
  char opt[8];
  socklen_t optlen = sizeof(opt);

  if (getsockopt(s, (int)level, (int)optname, opt, &optlen) == -1)
    mrb_sys_fail(mrb, "getsockopt");
  mrb_int family = socket_family(s);
  mrb_value data = mrb_str_new(mrb, opt, optlen);
  mrb_value args[4] = {mrb_fixnum_value(family), mrb_fixnum_value(level), mrb_fixnum_value(optname), data};
  return mrb_obj_new(mrb, socket_option_class(mrb), 4, args);
}

/*
 * call-seq:
 *   basicsocket.recv(maxlen, flags=0) -> string
 *
 * Receives data from the socket.
 *
 *   data = sock.recv(1024)
 *   data = sock.recv(512, 0)
 */
static mrb_value
mrb_basicsocket_recv(mrb_state *mrb, mrb_value self)
{
  mrb_int maxlen, flags = 0;

  mrb_get_args(mrb, "i|i", &maxlen, &flags);

  mrb_value buf = mrb_str_new_capa(mrb, maxlen);
  ssize_t n = recv(socket_fd(mrb, self), RSTRING_PTR(buf), (fsize_t)maxlen, (int)flags);
  if (n == -1)
    mrb_sys_fail(mrb, "recv");
  mrb_str_resize(mrb, buf, (mrb_int)n);
  return buf;
}

/*
 * call-seq:
 *   basicsocket._recvfrom(maxlen, flags=0) -> [data, sockaddr]
 *
 * Internal method to receive data and sender address from socket.
 * Returns data and packed sockaddr.
 */
static mrb_value
mrb_basicsocket_recvfrom(mrb_state *mrb, mrb_value self)
{
  mrb_int maxlen, flags = 0;

  mrb_get_args(mrb, "i|i", &maxlen, &flags);

  mrb_value buf = mrb_str_new_capa(mrb, maxlen);
  socklen_t socklen = sizeof(struct sockaddr_storage);
  mrb_value sa = mrb_str_new_capa(mrb, socklen);
  ssize_t n = recvfrom(socket_fd(mrb, self), RSTRING_PTR(buf), (fsize_t)maxlen, (int)flags, (struct sockaddr*)RSTRING_PTR(sa), &socklen);
  if (n == -1)
    mrb_sys_fail(mrb, "recvfrom");
  mrb_str_resize(mrb, buf, (mrb_int)n);
  mrb_str_resize(mrb, sa, (mrb_int)socklen);

  mrb_value ary = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, ary, buf);
  mrb_ary_push(mrb, ary, sa);
  return ary;
}

/*
 * call-seq:
 *   basicsocket.send(mesg, flags) -> integer
 *
 * Sends data through the socket. Returns the number of bytes sent.
 *
 *   bytes_sent = sock.send("Hello", 0)
 */
static mrb_value
mrb_basicsocket_send(mrb_state *mrb, mrb_value self)
{
  mrb_int flags;
  mrb_value mesg;
  mrb_value dest = mrb_nil_value();

  mrb_get_args(mrb, "Si|S", &mesg, &flags, &dest);

  ssize_t n;
  if (mrb_nil_p(dest)) {
    n = send(socket_fd(mrb, self), RSTRING_PTR(mesg), (fsize_t)RSTRING_LEN(mesg), (int)flags);
  }
  else {
    n = sendto(socket_fd(mrb, self), RSTRING_PTR(mesg), (fsize_t)RSTRING_LEN(mesg), (int)flags, (const struct sockaddr*)RSTRING_PTR(dest), (fsize_t)RSTRING_LEN(dest));
  }
  if (n == -1)
    mrb_sys_fail(mrb, "send");
  return mrb_fixnum_value((mrb_int)n);
}

/*
 * call-seq:
 *   basicsocket._setnonblock(flag) -> nil
 *
 * Internal method to set or unset non-blocking mode on the socket.
 *
 *   sock._setnonblock(true)   # enable non-blocking
 *   sock._setnonblock(false)  # disable non-blocking
 */
static mrb_value
mrb_basicsocket_setnonblock(mrb_state *mrb, mrb_value self)
{
  mrb_bool nonblocking;

  mrb_get_args(mrb, "b", &nonblocking);
  int fd = socket_fd(mrb, self);

  if (mrb_hal_socket_set_nonblock(mrb, fd, nonblocking) == -1)
    mrb_sys_fail(mrb, "set_nonblock");

  return mrb_nil_value();
}

/*
 * call-seq:
 *   basicsocket.setsockopt(level, optname, optval) -> 0
 *
 * Sets a socket option. Level and optname are constants, optval is the value.
 *
 *   sock.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, 1)
 */
static mrb_value
mrb_basicsocket_setsockopt(mrb_state *mrb, mrb_value self)
{
  mrb_int level = 0, optname;
  mrb_value so, optval;
  mrb_int argc = mrb_get_args(mrb, "o|io", &so, &optname, &optval);

  if (argc == 3) {
    mrb_ensure_int_type(mrb, so);
    level = mrb_integer(so);
    if (mrb_string_p(optval)) {
      /* that's good */
    }
    else if (mrb_true_p(optval) || mrb_false_p(optval)) {
      mrb_int i = mrb_test(optval) ? 1 : 0;
      optval = mrb_str_new(mrb, (char*)&i, sizeof(i));
    }
    else if (mrb_integer_p(optval)) {
      if (optname == IP_MULTICAST_TTL || optname == IP_MULTICAST_LOOP) {
        char uc = (char)mrb_integer(optval);
        optval = mrb_str_new(mrb, &uc, sizeof(uc));
      }
      else {
        mrb_int i = mrb_integer(optval);
        optval = mrb_str_new(mrb, (char*)&i, sizeof(i));
      }
    }
    else {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "optval should be true, false, an integer, or a string");
    }
  }
  else if (argc == 1) {
    if (!mrb_obj_is_instance_of(mrb, so, socket_option_class(mrb)))
      mrb_raise(mrb, E_ARGUMENT_ERROR, "not an instance of Socket::Option");
    level = mrb_as_int(mrb, mrb_iv_get(mrb, so, MRB_SYM(level)));
    optname = mrb_as_int(mrb, mrb_iv_get(mrb, so, MRB_SYM(optname)));
    optval = mrb_iv_get(mrb, so, MRB_SYM(data));
    mrb_ensure_string_type(mrb, optval);
  }
  else {
    mrb_argnum_error(mrb, argc, 3, 3);
  }

  int s = socket_fd(mrb, self);
  if (setsockopt(s, (int)level, (int)optname, RSTRING_PTR(optval), (socklen_t)RSTRING_LEN(optval)) == -1)
    mrb_sys_fail(mrb, "setsockopt");
  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   basicsocket.shutdown(how=Socket::SHUT_RDWR) -> 0
 *
 * Shuts down part of the socket connection.
 *
 *   sock.shutdown(Socket::SHUT_RD)    # shutdown reading
 *   sock.shutdown(Socket::SHUT_WR)    # shutdown writing
 *   sock.shutdown(Socket::SHUT_RDWR)  # shutdown both (default)
 */
static mrb_value
mrb_basicsocket_shutdown(mrb_state *mrb, mrb_value self)
{
  mrb_int how = SHUT_RDWR;

  mrb_get_args(mrb, "|i", &how);
  if (shutdown(socket_fd(mrb, self), (int)how) != 0)
    mrb_sys_fail(mrb, "shutdown");
  return mrb_fixnum_value(0);
}

/* Helper to set socket flag on IO object */
static mrb_value
mrb_basicsocket_set_is_socket(mrb_state *mrb, mrb_value self)
{
  mrb_bool b;
  mrb_get_args(mrb, "b", &b);

  struct mrb_io *io_p = (struct mrb_io*)DATA_PTR(self);
  if (io_p) {
    io_p->is_socket = b;
  }

  return mrb_bool_value(b);
}

/*
 * call-seq:
 *   IPSocket.ntop(af, addr) -> string
 *
 * Converts a network address to a string representation.
 *
 *   IPSocket.ntop(Socket::AF_INET, "\x7f\x00\x00\x01")  #=> "127.0.0.1"
 */
static mrb_value
mrb_ipsocket_ntop(mrb_state *mrb, mrb_value klass)
{
  mrb_int af, n;
  const char *addr;
  char buf[50];

  mrb_get_args(mrb, "is", &af, &addr, &n);
  if ((af == AF_INET && n != 4) || (af == AF_INET6 && n != 16) ||
      mrb_hal_socket_inet_ntop((int)af, addr, buf, sizeof(buf)) == NULL)
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
  return mrb_str_new_cstr(mrb, buf);
}

/*
 * call-seq:
 *   IPSocket.pton(af, hostname) -> string
 *
 * Converts a string representation of an address to network format.
 *
 *   IPSocket.pton(Socket::AF_INET, "127.0.0.1")  #=> "\x7f\x00\x00\x01"
 */
static mrb_noreturn void
invalid_address_error(mrb_state *mrb)
{
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid address");
}

static mrb_value
mrb_ipsocket_pton(mrb_state *mrb, mrb_value klass)
{
  mrb_int af, n;
  const char *bp;
  char buf[50];

  mrb_get_args(mrb, "is", &af, &bp, &n);
  if ((size_t)n > sizeof(buf) - 1) {
    invalid_address_error(mrb);
  }
  memcpy(buf, bp, n);
  buf[n] = '\0';

  if (af == AF_INET) {
    struct in_addr in;
    if (mrb_hal_socket_inet_pton(AF_INET, buf, (void*)&in.s_addr) != 1) {
      invalid_address_error(mrb);
    }
    return mrb_str_new(mrb, (char*)&in.s_addr, 4);
  }
  else if (af == AF_INET6) {
    struct in6_addr in6;
    if (mrb_hal_socket_inet_pton(AF_INET6, buf, (void*)&in6.s6_addr) != 1) {
      invalid_address_error(mrb);
    }
    return mrb_str_new(mrb, (char*)&in6.s6_addr, 16);
  }
  else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "unsupported address family");
  }
  return mrb_nil_value(); /* not reached */
}

/*
 * call-seq:
 *   ipsocket.recvfrom(maxlen, flags=0) -> [data, [family, port, hostname, ip]]
 *
 * Receives data from the socket and returns sender address information.
 *
 *   data, addr = sock.recvfrom(1024)
 *   # addr => ["AF_INET", 12345, "hostname", "192.168.1.1"]
 */
static mrb_value
mrb_ipsocket_recvfrom(mrb_state *mrb, mrb_value self)
{
  mrb_int maxlen;
  mrb_int flags = 0;

  mrb_get_args(mrb, "i|i", &maxlen, &flags);

  mrb_value buf = mrb_str_new_capa(mrb, maxlen);
  struct sockaddr_storage ss;
  socklen_t socklen = sizeof(ss);
  int fd = socket_fd(mrb, self);
  ssize_t n = recvfrom(fd, RSTRING_PTR(buf), (fsize_t)maxlen, (int)flags,
                       (struct sockaddr*)&ss, &socklen);
  if (n == -1) {
    mrb_sys_fail(mrb, "recvfrom");
  }
  mrb_str_resize(mrb, buf, (mrb_int)n);

  mrb_value a = sa2addrlist(mrb, (struct sockaddr*)&ss, socklen);
  mrb_value pair = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, pair, buf);
  mrb_ary_push(mrb, pair, a);
  return pair;
}

/*
 * call-seq:
 *   Socket.gethostname -> string
 *
 * Returns the hostname of the current machine.
 *
 *   Socket.gethostname  #=> "localhost"
 */
static mrb_value
mrb_socket_gethostname(mrb_state *mrb, mrb_value cls)
{
#ifdef HOST_NAME_MAX
  size_t bufsize = HOST_NAME_MAX + 1;
#else
  size_t bufsize = 256;
#endif
  mrb_value buf = mrb_str_new_capa(mrb, (mrb_int)bufsize);

  if (gethostname(RSTRING_PTR(buf), (fsize_t)bufsize) != 0)
    mrb_sys_fail(mrb, "gethostname");
  mrb_str_resize(mrb, buf, (mrb_int)strlen(RSTRING_PTR(buf)));
  return buf;
}

/*
 * call-seq:
 *   Socket._accept(fd) -> [new_fd, sockaddr]
 *
 * Internal method to accept a connection on a socket file descriptor.
 * Returns the new file descriptor and remote address.
 */
static mrb_value
mrb_socket_accept(mrb_state *mrb, mrb_value klass)
{
  mrb_int s0;

  mrb_get_args(mrb, "i", &s0);
  int s1 = (int)accept(s0, NULL, NULL);
  if (s1 == -1) {
    mrb_sys_fail(mrb, "accept");
  }
  return mrb_fixnum_value(s1);
}

static mrb_value
mrb_socket_accept2(mrb_state *mrb, mrb_value klass)
{
  mrb_int s0;

  mrb_get_args(mrb, "i", &s0);

  socklen_t socklen = sizeof(struct sockaddr_storage);
  mrb_value sastr = mrb_str_new_capa(mrb, (mrb_int)socklen);
  mrb_value ary = mrb_ary_new_capa(mrb, 2);

  int s1 = (int)accept(s0, (struct sockaddr*)RSTRING_PTR(sastr), &socklen);
  if (s1 == -1) {
    mrb_sys_fail(mrb, "accept");
  }

  mrb_str_resize(mrb, sastr, socklen);
  mrb_ary_push(mrb, ary, mrb_fixnum_value(s1));
  mrb_ary_push(mrb, ary, sastr);
  return ary;
}

/*
 * call-seq:
 *   Socket._bind(fd, sockaddr) -> 0
 *
 * Internal method to bind a socket file descriptor to the given address.
 */
static mrb_value
mrb_socket_bind(mrb_state *mrb, mrb_value klass)
{
  mrb_value sastr;
  mrb_int s;

  mrb_get_args(mrb, "iS", &s, &sastr);
  if (bind((int)s, (struct sockaddr*)RSTRING_PTR(sastr), (socklen_t)RSTRING_LEN(sastr)) == -1) {
    mrb_sys_fail(mrb, "bind");
  }
  return mrb_nil_value();
}

/*
 * call-seq:
 *   Socket._connect(fd, sockaddr) -> 0
 *
 * Internal method to connect a socket file descriptor to the given address.
 */
static mrb_value
mrb_socket_connect(mrb_state *mrb, mrb_value klass)
{
  mrb_value sastr;
  mrb_int s;

  mrb_get_args(mrb, "iS", &s, &sastr);
  if (connect((int)s, (struct sockaddr*)RSTRING_PTR(sastr), (socklen_t)RSTRING_LEN(sastr)) == -1) {
    mrb_sys_fail(mrb, "connect");
  }
  return mrb_nil_value();
}

/*
 * call-seq:
 *   Socket._listen(fd, backlog) -> 0
 *
 * Internal method to set a socket file descriptor to listen for connections.
 */
static mrb_value
mrb_socket_listen(mrb_state *mrb, mrb_value klass)
{
  mrb_int s, backlog;

  mrb_get_args(mrb, "ii", &s, &backlog);
  if (listen((int)s, (int)backlog) == -1) {
    mrb_sys_fail(mrb, "listen");
  }
  return mrb_nil_value();
}

static mrb_value
mrb_socket_sockaddr_family(mrb_state *mrb, mrb_value klass)
{
  mrb_value str;
  const struct sockaddr *sa;

  mrb_get_args(mrb, "S", &str);
  if ((size_t)RSTRING_LEN(str) < offsetof(struct sockaddr, sa_family) + sizeof(sa->sa_family)) {
    mrb_raise(mrb, E_SOCKET_ERROR, "invalid sockaddr (too short)");
  }
  sa = (const struct sockaddr*)RSTRING_PTR(str);
  return mrb_fixnum_value(sa->sa_family);
}

/*
 * call-seq:
 *   Socket.sockaddr_un(path) -> string
 *
 * Returns a packed sockaddr_un structure for the given Unix socket path.
 *
 *   Socket.sockaddr_un("/tmp/socket")
 *   Socket.sockaddr_un("/var/run/daemon.sock")
 */
static mrb_value
mrb_socket_sockaddr_un(mrb_state *mrb, mrb_value klass)
{
  mrb_value path;

  mrb_get_args(mrb, "S", &path);
  return mrb_hal_socket_sockaddr_un(mrb, RSTRING_PTR(path), (size_t)RSTRING_LEN(path));
}

/*
 * call-seq:
 *   Socket.socketpair(domain, type, protocol=0) -> [socket1, socket2]
 *   Socket.pair(domain, type, protocol=0) -> [socket1, socket2]
 *
 * Creates a pair of connected sockets.
 *
 *   sock1, sock2 = Socket.socketpair(Socket::AF_UNIX, Socket::SOCK_STREAM)
 *   sock1, sock2 = Socket.pair(Socket::AF_UNIX, Socket::SOCK_DGRAM)
 */
static mrb_value
mrb_socket_socketpair(mrb_state *mrb, mrb_value klass)
{
  mrb_int domain, type, protocol;
  int sv[2];

  mrb_get_args(mrb, "iii", &domain, &type, &protocol);

  if (mrb_hal_socket_socketpair(mrb, (int)domain, (int)type, (int)protocol, sv) == -1) {
    mrb_sys_fail(mrb, "socketpair");
  }

  mrb_value ary = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, ary, mrb_fixnum_value(sv[0]));
  mrb_ary_push(mrb, ary, mrb_fixnum_value(sv[1]));
  return ary;
}

/*
 * call-seq:
 *   Socket._socket(domain, type, protocol) -> fd
 *
 * Internal method to create a new socket and return its file descriptor.
 *
 *   fd = Socket._socket(Socket::AF_INET, Socket::SOCK_STREAM, 0)
 */
static mrb_value
mrb_socket_socket(mrb_state *mrb, mrb_value klass)
{
  mrb_int domain, type, protocol;

  mrb_get_args(mrb, "iii", &domain, &type, &protocol);

  int s = (int)socket((int)domain, (int)type, (int)protocol);
  if (s == -1)
    mrb_sys_fail(mrb, "socket");
  return mrb_fixnum_value(s);
}

/* Helper to allocate TCPSocket object */
static mrb_value
mrb_tcpsocket_allocate(mrb_state *mrb, mrb_value klass)
{
  struct RClass *c = mrb_class_ptr(klass);
  enum mrb_vtype ttype = MRB_INSTANCE_TT(c);

  /* copied from mrb_instance_alloc() */
  if (ttype == 0) ttype = MRB_TT_OBJECT;
  return mrb_obj_value((struct RObject*)mrb_obj_alloc(mrb, ttype, c));
}

/* Windows overrides for IO methods on BasicSocket objects.
 * This is because sockets on Windows are not the same as file
 * descriptors, and thus functions which operate on file descriptors
 * will break on socket descriptors.
 */
#ifdef _WIN32
/*
 * call-seq:
 *   basicsocket.close -> nil
 *
 * Windows-specific implementation to close socket using closesocket().
 * Overrides IO#close for socket objects on Windows.
 */
static mrb_value
mrb_win32_basicsocket_close(mrb_state *mrb, mrb_value self)
{
  if (closesocket(socket_fd(mrb, self)) != NO_ERROR)
    mrb_raise(mrb, E_SOCKET_ERROR, "closesocket unsuccessful");
  return mrb_nil_value();
}

/*
 * call-seq:
 *   basicsocket.sysread(maxlen, outbuf=nil) -> string
 *
 * Windows-specific implementation to read from socket using recv().
 * Overrides IO#sysread for socket objects on Windows.
 */
static mrb_value
mrb_win32_basicsocket_sysread(mrb_state *mrb, mrb_value self)
{
  mrb_value buf = mrb_nil_value();
  mrb_int maxlen;

  mrb_get_args(mrb, "i|S", &maxlen, &buf);
  if (maxlen < 0) {
    return mrb_nil_value();
  }

  if (mrb_nil_p(buf)) {
    buf = mrb_str_new(mrb, NULL, maxlen);
  }
  if (RSTRING_LEN(buf) != maxlen) {
    buf = mrb_str_resize(mrb, buf, maxlen);
  }

  int sd = socket_fd(mrb, self);
  int ret = recv(sd, RSTRING_PTR(buf), (int)maxlen, 0);

  switch (ret) {
    case 0: /* EOF */
      if (maxlen == 0) {
        buf = mrb_str_new_cstr(mrb, "");
      }
      else {
        mrb_raise(mrb, E_EOF_ERROR, "sysread failed: End of File");
      }
      break;
    case SOCKET_ERROR: /* Error */
      mrb_sys_fail(mrb, "recv");
      break;
    default:
      if (RSTRING_LEN(buf) != ret) {
        buf = mrb_str_resize(mrb, buf, ret);
      }
      break;
  }

  return buf;
}

/*
 * call-seq:
 *   basicsocket.sysseek(offset, whence) -> integer
 *
 * Windows-specific implementation that raises NotImplementedError.
 * Sockets don't support seeking operations.
 */
static mrb_value
mrb_win32_basicsocket_sysseek(mrb_state *mrb, mrb_value self)
{
  mrb_raise(mrb, E_NOTIMP_ERROR, "sysseek not implemented for windows sockets");
  return mrb_nil_value();
}

/*
 * call-seq:
 *   basicsocket.syswrite(string) -> integer
 *
 * Windows-specific implementation to write to socket using send().
 * Overrides IO#syswrite for socket objects on Windows.
 */
static mrb_value
mrb_win32_basicsocket_syswrite(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  SOCKET sd = socket_fd(mrb, self);

  mrb_get_args(mrb, "S", &str);

  int n = send(sd, RSTRING_PTR(str), (int)RSTRING_LEN(str), 0);
  if (n == SOCKET_ERROR)
    mrb_sys_fail(mrb, "send");
  return mrb_int_value(mrb, n);
}

#endif

void
mrb_mruby_socket_gem_init(mrb_state* mrb)
{
  mrb_hal_socket_init(mrb);

  struct RClass *ainfo = mrb_define_class_id(mrb, MRB_SYM(Addrinfo), mrb->object_class);
  mrb_define_class_method_id(mrb, ainfo, MRB_SYM(getaddrinfo), mrb_addrinfo_getaddrinfo, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(4));
  mrb_define_method_id(mrb, ainfo, MRB_SYM(getnameinfo), mrb_addrinfo_getnameinfo, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, ainfo, MRB_SYM(unix_path), mrb_addrinfo_unix_path, MRB_ARGS_NONE());

  struct RClass *io = mrb_class_get_id(mrb, MRB_SYM(IO));

  struct RClass *bsock = mrb_define_class_id(mrb, MRB_SYM(BasicSocket), io);
  mrb_define_method_id(mrb, bsock, MRB_SYM(_recvfrom), mrb_basicsocket_recvfrom, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(_setnonblock), mrb_basicsocket_setnonblock, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(getpeereid), mrb_basicsocket_getpeereid, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bsock, MRB_SYM(getpeername), mrb_basicsocket_getpeername, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bsock, MRB_SYM(getsockname), mrb_basicsocket_getsockname, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bsock, MRB_SYM(getsockopt), mrb_basicsocket_getsockopt, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, bsock, MRB_SYM(recv), mrb_basicsocket_recv, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(send), mrb_basicsocket_send, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(setsockopt), mrb_basicsocket_setsockopt, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(2));
  mrb_define_method_id(mrb, bsock, MRB_SYM(shutdown), mrb_basicsocket_shutdown, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM_E(_is_socket), mrb_basicsocket_set_is_socket, MRB_ARGS_REQ(1));

  struct RClass *ipsock = mrb_define_class_id(mrb, MRB_SYM(IPSocket), bsock);
  mrb_define_class_method_id(mrb, ipsock, MRB_SYM(ntop), mrb_ipsocket_ntop, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, ipsock, MRB_SYM(pton), mrb_ipsocket_pton, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, ipsock, MRB_SYM(recvfrom), mrb_ipsocket_recvfrom, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  struct RClass *tcpsock = mrb_define_class_id(mrb, MRB_SYM(TCPSocket), ipsock);
  mrb_define_class_method_id(mrb, tcpsock, MRB_SYM(_allocate), mrb_tcpsocket_allocate, MRB_ARGS_NONE());

  struct RClass *sock = mrb_define_class_id(mrb, MRB_SYM(Socket), bsock);
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_accept), mrb_socket_accept, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_accept2), mrb_socket_accept2, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_bind), mrb_socket_bind, MRB_ARGS_REQ(3));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_connect), mrb_socket_connect, MRB_ARGS_REQ(3));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_listen), mrb_socket_listen, MRB_ARGS_REQ(2));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_sockaddr_family), mrb_socket_sockaddr_family, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(_socket), mrb_socket_socket, MRB_ARGS_REQ(3));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(gethostname), mrb_socket_gethostname, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, sock, MRB_SYM(sockaddr_un), mrb_socket_sockaddr_un, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, sock, MRB_SYM(socketpair), mrb_socket_socketpair, MRB_ARGS_REQ(3));

  /* Windows IO Methods Overridden on BasicSocket */
#ifdef _WIN32
  mrb_define_method_id(mrb, bsock, MRB_SYM(close), mrb_win32_basicsocket_close, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bsock, MRB_SYM(sysread), mrb_win32_basicsocket_sysread, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(sysseek), mrb_win32_basicsocket_sysseek, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(syswrite), mrb_win32_basicsocket_syswrite, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(read), mrb_win32_basicsocket_sysread, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, bsock, MRB_SYM(write), mrb_win32_basicsocket_syswrite, MRB_ARGS_REQ(1));
#endif

  struct RClass *option = mrb_define_class_under_id(mrb, sock, MRB_SYM(Option), mrb->object_class);
  mrb_define_class_method_id(mrb, option, MRB_SYM(bool), socket_option_s_bool, MRB_ARGS_REQ(4));
  mrb_define_class_method_id(mrb, option, MRB_SYM(int), socket_option_s_int, MRB_ARGS_REQ(4));
  mrb_define_method_id(mrb, option, MRB_SYM(initialize), socket_option_init, MRB_ARGS_REQ(4));
  mrb_define_method_id(mrb, option, MRB_SYM(inspect), socket_option_inspect, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(family), socket_option_family, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(level), socket_option_level, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(optname), socket_option_optname, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(data), socket_option_data, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(bool), socket_option_bool, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(int), socket_option_int, MRB_ARGS_REQ(0));

  mrb_define_method_id(mrb, option, MRB_SYM(linger), socket_option_notimp, MRB_ARGS_REQ(0));
  mrb_define_method_id(mrb, option, MRB_SYM(unpack), socket_option_notimp, MRB_ARGS_REQ(1));

  struct RClass *constants = mrb_define_module_under_id(mrb, sock, MRB_SYM(Constants));

#define define_const(SYM) \
  do {                                                                \
    mrb_define_const(mrb, constants, #SYM, mrb_int_value(mrb, SYM));  \
  } while (0)

#include "const.cstub"

  mrb_include_module(mrb, sock, constants);
}

void
mrb_mruby_socket_gem_final(mrb_state* mrb)
{
  mrb_hal_socket_final(mrb);
}
