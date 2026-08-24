/*
** socket_hal.h - Socket HAL (Hardware Abstraction Layer) interface
**
** See Copyright Notice in mruby.h
**
** This header defines the platform-independent socket HAL interface.
** Platform-specific implementations are provided by HAL gems:
**   - hal-posix-socket: POSIX socket implementation (Linux, macOS, BSD, Unix)
**   - hal-win-socket: Windows socket implementation (Windows, MinGW)
*/

#ifndef MRUBY_SOCKET_HAL_H
#define MRUBY_SOCKET_HAL_H

#include <mruby.h>

MRB_BEGIN_DECL

/*
 * Socket HAL Initialization/Finalization
 */

/* Initialize socket subsystem (e.g., WSAStartup on Windows) */
void mrb_hal_socket_init(mrb_state *mrb);

/* Finalize socket subsystem (e.g., WSACleanup on Windows) */
void mrb_hal_socket_final(mrb_state *mrb);

/*
 * Error Handling
 */

/* Translate the most recent socket-API error into a POSIX errno value and
 * store it in errno. On Windows, this reads WSAGetLastError() and maps it;
 * on POSIX, this is a no-op (errno is already set by the failed call).
 * Call this immediately after a socket-API failure, before mrb_sys_fail. */
void mrb_hal_socket_set_errno_from_last_error(void);

/*
 * Socket Control Operations
 */

/* Set non-blocking mode on socket
 * Returns 0 on success, -1 on error (sets errno) */
int mrb_hal_socket_set_nonblock(mrb_state *mrb, int fd, int nonblock);

/*
 * Address Conversion Functions
 */

/* Convert network address to presentation format (string)
 * af: address family (AF_INET, AF_INET6)
 * src: network address in binary form
 * dst: buffer for string result
 * size: size of dst buffer
 * Returns: dst on success, NULL on error */
const char* mrb_hal_socket_inet_ntop(int af, const void *src, char *dst, size_t size);

/* Convert presentation format (string) to network address
 * af: address family (AF_INET, AF_INET6)
 * src: string representation of address
 * dst: buffer for network address result
 * Returns: 1 on success, 0 if src is not valid, -1 on error */
int mrb_hal_socket_inet_pton(int af, const char *src, void *dst);

/*
 * Platform-Specific Socket Features
 */

/* Create Unix domain socket address structure
 * path: Unix socket path
 * Returns: packed sockaddr string, or raises exception if not supported */
mrb_value mrb_hal_socket_sockaddr_un(mrb_state *mrb, const char *path, size_t pathlen);

/* Create a pair of connected sockets
 * domain: address family (e.g., AF_UNIX)
 * type: socket type (e.g., SOCK_STREAM)
 * protocol: protocol (usually 0)
 * sv: array to receive the two socket descriptors
 * Returns: 0 on success, -1 on error (sets errno) */
int mrb_hal_socket_socketpair(mrb_state *mrb, int domain, int type, int protocol, int sv[2]);

/* Get Unix socket path from sockaddr
 * Returns: Unix socket path string, or raises exception if not supported */
mrb_value mrb_hal_socket_unix_path(mrb_state *mrb, const char *sockaddr, size_t socklen);

/* Enumerate local IP addresses for all network interfaces.
 * Returns an Array of String values, each a binary sockaddr_in (AF_INET) or
 * sockaddr_in6 (AF_INET6) ready to be passed to Addrinfo.new.  Loopback,
 * link-local, and other interface-local addresses are included; the caller
 * is responsible for filtering if needed.
 * Raises a SystemCallError on failure of the underlying platform call
 * (getifaddrs / GetAdaptersAddresses). */
mrb_value mrb_hal_socket_ip_address_list(mrb_state *mrb);

MRB_END_DECL

#endif /* MRUBY_SOCKET_HAL_H */
