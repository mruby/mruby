/*
** urandom_hal.c - POSIX HAL implementation for mruby-random-urandom
**
** See Copyright Notice in mruby.h
**
** Reads /dev/urandom, which Linux, macOS, the BSDs and the other Unixes
** all have.
*/

#include <mruby.h>
#include "urandom_hal.h"

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

int
mrb_hal_urandom(mrb_state *mrb, void *buf, size_t len)
{
  unsigned char *p = (unsigned char*)buf;
  int fd;

  (void)mrb;
  do {
    fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
  } while (fd < 0 && errno == EINTR);
  if (fd < 0) return -1;

  while (len > 0) {
    ssize_t n = read(fd, p, len);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) {
      close(fd);
      return -1;
    }
    p += n;
    len -= (size_t)n;
  }
  close(fd);
  return 0;
}
