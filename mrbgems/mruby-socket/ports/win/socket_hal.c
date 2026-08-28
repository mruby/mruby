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
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/error.h>
#include "socket_hal.h"
#include <winsock2.h>
#include <ws2tcpip.h>
#include <iphlpapi.h>
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
 * Error Handling
 */

/* Map a Winsock error code to a POSIX errno value. Each case is guarded
 * with #ifdef so older MSVC CRTs that lack a particular Exxx still build;
 * unknown codes fall back to EIO so that the errno mrb_sys_fail reports is
 * still a failure rather than "success." Which exception carries it is the
 * build's business: Errno::EIO where mruby-errno is present, a RuntimeError
 * spelling the number where it is not. */
static int
wsa_to_errno(int wsa_err)
{
  switch (wsa_err) {
    case 0: return 0;
#ifdef EINTR
    case WSAEINTR:           return EINTR;
#endif
#ifdef EBADF
    case WSAEBADF:           return EBADF;
#endif
#ifdef EACCES
    case WSAEACCES:          return EACCES;
#endif
#ifdef EFAULT
    case WSAEFAULT:          return EFAULT;
#endif
#ifdef EINVAL
    case WSAEINVAL:          return EINVAL;
#endif
#ifdef EMFILE
    case WSAEMFILE:          return EMFILE;
#endif
#ifdef EWOULDBLOCK
    case WSAEWOULDBLOCK:     return EWOULDBLOCK;
#endif
#ifdef EINPROGRESS
    case WSAEINPROGRESS:     return EINPROGRESS;
#endif
#ifdef EALREADY
    case WSAEALREADY:        return EALREADY;
#endif
#ifdef ENOTSOCK
    case WSAENOTSOCK:        return ENOTSOCK;
#endif
#ifdef EDESTADDRREQ
    case WSAEDESTADDRREQ:    return EDESTADDRREQ;
#endif
#ifdef EMSGSIZE
    case WSAEMSGSIZE:        return EMSGSIZE;
#endif
#ifdef EPROTOTYPE
    case WSAEPROTOTYPE:      return EPROTOTYPE;
#endif
#ifdef ENOPROTOOPT
    case WSAENOPROTOOPT:     return ENOPROTOOPT;
#endif
#ifdef EPROTONOSUPPORT
    case WSAEPROTONOSUPPORT: return EPROTONOSUPPORT;
#endif
#ifdef EOPNOTSUPP
    case WSAEOPNOTSUPP:      return EOPNOTSUPP;
#endif
#ifdef EAFNOSUPPORT
    case WSAEAFNOSUPPORT:    return EAFNOSUPPORT;
    case WSAEPFNOSUPPORT:    return EAFNOSUPPORT;
    case WSAESOCKTNOSUPPORT: return EAFNOSUPPORT;
#endif
#ifdef EADDRINUSE
    case WSAEADDRINUSE:      return EADDRINUSE;
#endif
#ifdef EADDRNOTAVAIL
    case WSAEADDRNOTAVAIL:   return EADDRNOTAVAIL;
#endif
#ifdef ENETDOWN
    case WSAENETDOWN:        return ENETDOWN;
#endif
#ifdef ENETUNREACH
    case WSAENETUNREACH:     return ENETUNREACH;
#endif
#ifdef ENETRESET
    case WSAENETRESET:       return ENETRESET;
#endif
#ifdef ECONNABORTED
    case WSAECONNABORTED:    return ECONNABORTED;
#endif
#ifdef ECONNRESET
    case WSAECONNRESET:      return ECONNRESET;
#endif
#ifdef ENOBUFS
    case WSAENOBUFS:         return ENOBUFS;
#endif
#ifdef EISCONN
    case WSAEISCONN:         return EISCONN;
#endif
#ifdef ENOTCONN
    case WSAENOTCONN:        return ENOTCONN;
#endif
#ifdef ETIMEDOUT
    case WSAETIMEDOUT:       return ETIMEDOUT;
#endif
#ifdef ECONNREFUSED
    case WSAECONNREFUSED:    return ECONNREFUSED;
#endif
#ifdef EHOSTUNREACH
    case WSAEHOSTUNREACH:    return EHOSTUNREACH;
#endif
#ifdef ENAMETOOLONG
    case WSAENAMETOOLONG:    return ENAMETOOLONG;
#endif
    default:                 return EIO;
  }
}

void
mrb_hal_socket_set_errno_from_last_error(void)
{
  errno = wsa_to_errno(WSAGetLastError());
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
    mrb_hal_socket_set_errno_from_last_error();
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

mrb_value
mrb_hal_socket_ip_address_list(mrb_state *mrb)
{
  /* MSDN recommends 15 KiB as the initial buffer size to handle most
     adapter configurations in a single call. */
  ULONG buflen = 15000;
  IP_ADAPTER_ADDRESSES *adapters = (IP_ADAPTER_ADDRESSES*)mrb_malloc(mrb, buflen);
  ULONG ret = ERROR_BUFFER_OVERFLOW;
  for (int retries = 0; retries < 3 && ret == ERROR_BUFFER_OVERFLOW; retries++) {
    ret = GetAdaptersAddresses(AF_UNSPEC,
            GAA_FLAG_SKIP_ANYCAST | GAA_FLAG_SKIP_MULTICAST | GAA_FLAG_SKIP_DNS_SERVER,
            NULL, adapters, &buflen);
    if (ret == ERROR_BUFFER_OVERFLOW) {
      adapters = (IP_ADAPTER_ADDRESSES*)mrb_realloc(mrb, adapters, buflen);
    }
  }
  if (ret != ERROR_SUCCESS) {
    mrb_free(mrb, adapters);
    mrb_raisef(mrb, mrb_class_get_id(mrb, MRB_SYM(SocketError)),
               "GetAdaptersAddresses failed (Win32 error %u)", (unsigned int)ret);
  }

  mrb_value ary = mrb_ary_new(mrb);
  int arena_idx = mrb_gc_arena_save(mrb);
  for (IP_ADAPTER_ADDRESSES *ad = adapters; ad != NULL; ad = ad->Next) {
    for (IP_ADAPTER_UNICAST_ADDRESS *ua = ad->FirstUnicastAddress; ua != NULL; ua = ua->Next) {
      SOCKADDR *sa = ua->Address.lpSockaddr;
      int salen;
      switch (sa->sa_family) {
        case AF_INET:  salen = sizeof(SOCKADDR_IN);  break;
        case AF_INET6: salen = sizeof(SOCKADDR_IN6); break;
        default: continue;
      }
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, (const char*)sa, salen));
      mrb_gc_arena_restore(mrb, arena_idx);
    }
  }
  mrb_free(mrb, adapters);
  return ary;
}
