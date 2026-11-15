/*
** urandom_hal.h - random bytes HAL interface for mruby-random-urandom
**
** See Copyright Notice in mruby.h
**
** Hardware Abstraction Layer for the platform's source of random bytes:
** what Random.urandom reads. The bundled ports are posix (/dev/urandom)
** and win (BCryptGenRandom); a board supplies its own, from a hardware
** generator for instance, with a `hal-urandom-<conf>` gem.
*/

#ifndef MRUBY_URANDOM_HAL_H
#define MRUBY_URANDOM_HAL_H

#include <mruby.h>

MRB_BEGIN_DECL

/* Fills `len` bytes at `buf` with random bytes fit for keys and nonces.
   Returns 0, or -1 when the platform cannot give them. */
int mrb_hal_urandom(mrb_state *mrb, void *buf, size_t len);

MRB_END_DECL

#endif /* MRUBY_URANDOM_HAL_H */
