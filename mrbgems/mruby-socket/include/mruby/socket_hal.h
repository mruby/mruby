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

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Socket HAL Initialization/Finalization
 */

/* Initialize socket subsystem (e.g., WSAStartup on Windows) */
void mrb_hal_socket_init(mrb_state *mrb);

/* Finalize socket subsystem (e.g., WSACleanup on Windows) */
void mrb_hal_socket_final(mrb_state *mrb);

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

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_SOCKET_HAL_H */
