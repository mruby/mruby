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

/* socketpair(2): `Socket.socketpair`. */
#define MRB_HAL_SOCKET_HAS_SOCKETPAIR

/* A socket is a file descriptor: read(2), write(2), lseek(2) and close(2)
   take it, so `BasicSocket` inherits every method of `IO` unchanged.  This
   guards no port function; without it `src/` reads and writes a socket
   through recv(2) and send(2), and leaves `BasicSocket#sysseek` undefined.
   It changes nothing in mruby-io, whose `IO.new` and `IO#close` must still
   know the socket for one; of the bundled io ports only Windows does. */
#define MRB_HAL_SOCKET_HAS_FD_IO

/* getpeereid(2): `BasicSocket#getpeereid`.  A BSD interface, which macOS
   and the BSDs have and Linux does not; a build on another libc that has
   one may say so with HAVE_GETPEEREID. */
#if defined(HAVE_GETPEEREID) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
# define MRB_HAL_SOCKET_HAS_GETPEEREID
#endif

#endif /* MRUBY_SOCKET_HAL_FEATURES_H */
