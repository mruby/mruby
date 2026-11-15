/*
** urandom_hal.c - Windows HAL implementation for mruby-random-urandom
**
** See Copyright Notice in mruby.h
**
** Asks BCryptGenRandom for the system's preferred generator.
*/

#include <mruby.h>
#include "urandom_hal.h"

#include <windows.h>
#include <bcrypt.h>

#ifndef STATUS_SUCCESS
#define STATUS_SUCCESS ((NTSTATUS)0x00000000L)
#endif

int
mrb_hal_urandom(mrb_state *mrb, void *buf, size_t len)
{
  unsigned char *p = (unsigned char*)buf;

  (void)mrb;
  while (len > 0) {
    ULONG n = len > 0x10000000 ? 0x10000000 : (ULONG)len;
    if (BCryptGenRandom(NULL, p, n, BCRYPT_USE_SYSTEM_PREFERRED_RNG) != STATUS_SUCCESS) {
      return -1;
    }
    p += n;
    len -= n;
  }
  return 0;
}
