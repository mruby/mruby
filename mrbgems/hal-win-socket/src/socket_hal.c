/*
** socket_hal.c - Windows HAL implementation for mruby-socket
**
** See Copyright Notice in mruby.h
**
** Windows implementation for socket operations using Winsock APIs.
** Supported platforms: Windows, MinGW
*/

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501 // need Windows XP or later
#endif

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include "socket_hal.h"
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <errno.h>
#include <string.h>

/*
 * Socket HAL Initialization/Finalization
 */

void
mrb_hal_socket_init(mrb_state *mrb)
{
  WSADATA wsaData;
  int result = WSAStartup(MAKEWORD(2, 2), &wsaData);
  if (result != NO_ERROR) {
    mrb_raise(mrb, mrb_class_get_id(mrb, MRB_SYM(RuntimeError)), "WSAStartup failed");
  }
}

void
mrb_hal_socket_final(mrb_state *mrb)
{
  (void)mrb;
  WSACleanup();
}

/*
 * Socket Control Operations
 */

int
mrb_hal_socket_set_nonblock(mrb_state *mrb, int fd, int nonblock)
{
  (void)mrb;
  u_long mode = nonblock ? 1 : 0;
  int result = ioctlsocket(fd, FIONBIO, &mode);
  if (result != NO_ERROR) {
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
  if (af == AF_INET) {
    struct sockaddr_in in = {0};
    in.sin_family = AF_INET;
    memcpy(&in.sin_addr, src, sizeof(struct in_addr));
    if (getnameinfo((struct sockaddr*)&in, sizeof(struct sockaddr_in),
                    dst, (DWORD)size, NULL, 0, NI_NUMERICHOST) == 0) {
      return dst;
    }
    return NULL;
  }
  else if (af == AF_INET6) {
    struct sockaddr_in6 in = {0};
    in.sin6_family = AF_INET6;
    memcpy(&in.sin6_addr, src, sizeof(struct in6_addr));
    if (getnameinfo((struct sockaddr*)&in, sizeof(struct sockaddr_in6),
                    dst, (DWORD)size, NULL, 0, NI_NUMERICHOST) == 0) {
      return dst;
    }
    return NULL;
  }
  return NULL;
}

int
mrb_hal_socket_inet_pton(int af, const char *src, void *dst)
{
  struct addrinfo hints = {0};
  hints.ai_family = af;
  hints.ai_flags = AI_NUMERICHOST;

  struct addrinfo *res;
  if (getaddrinfo(src, NULL, &hints, &res) != 0) {
    return 0;  /* Invalid address */
  }

  if (res == NULL) {
    return 0;
  }

  if (af == AF_INET && res->ai_family == AF_INET) {
    memcpy(dst, &((struct sockaddr_in*)res->ai_addr)->sin_addr, sizeof(struct in_addr));
    freeaddrinfo(res);
    return 1;
  }
  else if (af == AF_INET6 && res->ai_family == AF_INET6) {
    memcpy(dst, &((struct sockaddr_in6*)res->ai_addr)->sin6_addr, sizeof(struct in6_addr));
    freeaddrinfo(res);
    return 1;
  }

  freeaddrinfo(res);
  return 0;
}

/*
 * Platform-Specific Socket Features
 */

mrb_value
mrb_hal_socket_sockaddr_un(mrb_state *mrb, const char *path, size_t pathlen)
{
  (void)path;
  (void)pathlen;
  mrb_raise(mrb, mrb_class_get_id(mrb, MRB_SYM(NotImplementedError)),
            "sockaddr_un unsupported on Windows");
  return mrb_nil_value();
}

int
mrb_hal_socket_socketpair(mrb_state *mrb, int domain, int type, int protocol, int sv[2])
{
  (void)mrb;
  (void)domain;
  (void)type;
  (void)protocol;
  (void)sv;
  /* socketpair is not supported on Windows */
  errno = ENOSYS;
  return -1;
}

mrb_value
mrb_hal_socket_unix_path(mrb_state *mrb, const char *sockaddr, size_t socklen)
{
  (void)sockaddr;
  (void)socklen;
  mrb_raise(mrb, mrb_class_get_id(mrb, MRB_SYM(NotImplementedError)),
            "unix_path unsupported on Windows");
  return mrb_nil_value();
}

/*
 * Gem initialization
 */

void
mrb_hal_win_socket_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-socket gem */
}

void
mrb_hal_win_socket_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_socket_final called from mruby-socket */
}
