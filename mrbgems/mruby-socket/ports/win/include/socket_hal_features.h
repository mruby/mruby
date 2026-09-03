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

#endif /* MRUBY_SOCKET_HAL_FEATURES_H */
