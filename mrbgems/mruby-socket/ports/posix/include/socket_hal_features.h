/*
** socket_hal_features.h - what the POSIX port of mruby-socket implements
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

/* struct sockaddr_un: `Socket.sockaddr_un` and `Addrinfo#unix_path`, and
   through them every `UNIXSocket` and `UNIXServer` address. */
#define MRB_HAL_SOCKET_HAS_SOCKADDR_UN

#endif /* MRUBY_SOCKET_HAL_FEATURES_H */
