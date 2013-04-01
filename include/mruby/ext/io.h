/*
** io.h - IO class
*/

#ifndef MRUBY_IO_H
#define MRUBY_IO_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <errno.h>

#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

struct mrb_io {
  int fd;                                 /* file descriptor          */
  int fd2;                                /* file descriptor          */
  int pid;                                /* child's pid (for pipes)  */
};

struct mrb_io_type {
  const char *struct_name;
  void (*dfree)(mrb_state *mrb, void *);
};

#define FMODE_READABLE             0x00000001
#define FMODE_WRITABLE             0x00000002
#define FMODE_READWRITE            (FMODE_READABLE|FMODE_WRITABLE)
#define FMODE_BINMODE              0x00000004
#define FMODE_SYNC                 0x00000008
#define FMODE_TTY                  0x00000010
#define FMODE_DUPLEX               0x00000020
#define FMODE_APPEND               0x00000040
#define FMODE_CREATE               0x00000080
#define FMODE_WSPLIT               0x00000200
#define FMODE_WSPLIT_INITIALIZED   0x00000400
#define FMODE_TRUNC                0x00000800
#define FMODE_TEXTMODE             0x00001000
#define FMODE_SETENC_BY_BOM        0x00100000

#define E_IO_ERROR                 (mrb_class_obj_get(mrb, "IOError"))
#define E_EOF_ERROR                (mrb_class_obj_get(mrb, "EOFError"))

mrb_value mrb_open_file(mrb_state *mrb, int argc, mrb_value *argv, mrb_value io);
void fptr_finalize(mrb_state *mrb, struct mrb_io *fptr, int noraise);
mrb_value mrb_file_exist(mrb_state *mrb, mrb_value fname);

#if defined(__cplusplus)
} /* extern "C" { */
#endif
#endif /* MRUBY_IO_H */
