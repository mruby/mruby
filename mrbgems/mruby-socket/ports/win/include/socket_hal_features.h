/*
** socket_hal_features.h - what the Windows port of mruby-socket implements
**
** See Copyright Notice in mruby.h
**
** The gem's include/socket_hal.h reads this before it declares anything.  A macro
** defined here guards three things at once: the prototype there, the
** implementation in socket_hal.c, and the method definition under src/.  A
** port that declared a capability and did not implement it would fail to
** link, and one that declares nothing owes nothing.
*/

#ifndef MRUBY_SOCKET_HAL_FEATURES_H
#define MRUBY_SOCKET_HAL_FEATURES_H

/* No Unix domain addresses: `Socket.sockaddr_un` and `Addrinfo#unix_path`
   are left undefined rather than defined to fail. */

/* No socketpair(2): `Socket.socketpair` is likewise undefined. */

/* A SOCKET is not a C runtime descriptor: read(), write() and lseek() do
   not take it, so MRB_HAL_SOCKET_HAS_FD_IO is not declared and `src/` reads
   and writes a socket through recv() and send() instead.  That mruby-io's
   `IO.new` accepts the handle and `IO#close` closes it with closesocket()
   is mruby-io's own doing for Winsock, not this header's. */

#endif /* MRUBY_SOCKET_HAL_FEATURES_H */
