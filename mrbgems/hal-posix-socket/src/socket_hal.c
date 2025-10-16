/*
** socket_hal.c - POSIX HAL implementation for mruby-socket
**
** See Copyright Notice in mruby.h
**
** POSIX implementation for socket operations using standard POSIX APIs.
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include "socket_hal.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

/*
 * Socket HAL Initialization/Finalization
 */

void
mrb_hal_socket_init(mrb_state *mrb)
{
  (void)mrb;
  /* No initialization needed for POSIX sockets */
}

void
mrb_hal_socket_final(mrb_state *mrb)
{
  (void)mrb;
  /* No cleanup needed for POSIX sockets */
}

/*
 * Socket Control Operations
 */

int
mrb_hal_socket_set_nonblock(mrb_state *mrb, int fd, int nonblock)
{
  (void)mrb;

  int flags = fcntl(fd, F_GETFL, 0);
  if (flags == -1) {
    return -1;
  }

  if (nonblock) {
    flags |= O_NONBLOCK;
  }
  else {
    flags &= ~O_NONBLOCK;
  }

  if (fcntl(fd, F_SETFL, flags) == -1) {
    return -1;
  }

  return 0;
}

/*
 * Address Conversion Functions
 */

const char*
mrb_hal_socket_inet_ntop(int af, const void *src, char *dst, size_t size)
{
  return inet_ntop(af, src, dst, (socklen_t)size);
}

int
mrb_hal_socket_inet_pton(int af, const char *src, void *dst)
{
  return inet_pton(af, src, dst);
}

/*
 * Platform-Specific Socket Features
 */

mrb_value
mrb_hal_socket_sockaddr_un(mrb_state *mrb, const char *path, size_t pathlen)
{
  struct sockaddr_un *sunp;

  if (pathlen > sizeof(sunp->sun_path) - 1) {
    mrb_raisef(mrb, mrb_class_get_id(mrb, MRB_SYM(ArgumentError)),
               "too long unix socket path (max: %d bytes)",
               (int)sizeof(sunp->sun_path) - 1);
  }

  mrb_value s = mrb_str_new_capa(mrb, sizeof(struct sockaddr_un));
  sunp = (struct sockaddr_un*)RSTRING_PTR(s);

#ifdef HAVE_SA_LEN
  sunp->sun_len = sizeof(struct sockaddr_un);
#endif

  sunp->sun_family = AF_UNIX;
  memcpy(sunp->sun_path, path, pathlen);
  sunp->sun_path[pathlen] = '\0';
  mrb_str_resize(mrb, s, sizeof(struct sockaddr_un));

  return s;
}

int
mrb_hal_socket_socketpair(mrb_state *mrb, int domain, int type, int protocol, int sv[2])
{
  (void)mrb;
  return socketpair(domain, type, protocol, sv);
}

mrb_value
mrb_hal_socket_unix_path(mrb_state *mrb, const char *sockaddr, size_t socklen)
{
  const struct sockaddr *sa = (const struct sockaddr*)sockaddr;

  if (sa->sa_family != AF_UNIX) {
    mrb_raise(mrb, mrb_class_get_id(mrb, MRB_SYM(SocketError)), "need AF_UNIX address");
  }

  if (socklen < offsetof(struct sockaddr_un, sun_path) + 1) {
    return mrb_str_new(mrb, "", 0);
  }

  return mrb_str_new_cstr(mrb, ((const struct sockaddr_un*)sockaddr)->sun_path);
}

/*
 * Gem initialization
 */

void
mrb_hal_posix_socket_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-socket gem */
}

void
mrb_hal_posix_socket_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_socket_final called from mruby-socket */
}
