/*
** io.h - IO class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_IO_H
#define MRUBY_IO_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#include <io.h>
#endif

#if defined(HAVE_STDIO_EXT_H)
#include <stdio_ext.h>
#endif

#define HAVE_FCNTL_H
#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#elif defined(HAVE_SYS_FCNTL_H)
#include <sys/fcntl.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

struct mrb_io {
  int fd;                                 /* file descriptor          */
  FILE *f;                                /* stdio ptr for read/write */
  FILE *f2;                               /* stdio ptr for read/write */
  int mode;                               /* mode flags: FMODE_XXXs   */
  int pid;                                /* child's pid (for pipes)  */
  int lineno;                             /* number of lines read     */
  mrb_value path;                         /* pathname for file        */
  void (*finalize)(struct mrb_io *, int); /* finalize proc            */
};

struct RFile {
  MRB_OBJECT_HEADER;
  struct mrb_io *fptr;
};

#define RFILE(obj) ((struct RFile *)((obj).value.p))

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

#define FMODE_WBUF 16
#define FMODE_RBUF 32

#define E_IO_ERROR                 (mrb_class_obj_get(mrb, "IOError"))

#define mrb_io_ptr(s) ((struct RFile *)((s).value.p))

#define MakeOpenFile(mrb, obj, fp) do {\
  if (RFILE(obj)->fptr) {\
    rb_io_close(mrb, obj);\
    mrb_free(mrb, RFILE(obj)->fptr);\
    RFILE(obj)->fptr = 0;\
  }\
  fp = 0;\
  fp = RFILE(obj)->fptr = mrb_malloc(mrb, sizeof(struct mrb_io));\
  fp->f = fp->f2 = NULL;\
  fp->mode = 0;\
  fp->pid = 0;\
  fp->lineno = 0;\
  fp->path = mrb_nil_value();\
  fp->finalize = 0;\
} while (0)

#define GetOpenFile(mrb, obj, fp) rb_io_check_closed(mrb, (fp) = RFILE(obj)->fptr)
#define GetReadFile(fptr) ((fptr)->f)
#define GetWriteFile(fptr) (((fptr)->f2) ? (fptr)->f2 : (fptr)->f)

mrb_value mrb_open_file(mrb_state *mrb, int argc, mrb_value *argv, mrb_value io);
void fptr_finalize(mrb_state *mrb, struct mrb_io *fptr, int noraise);
mrb_value rb_io_initialize(mrb_state *mrb, int argc, mrb_value *argv, mrb_value io);
void mrb_io_check_initialized(mrb_state *mrb, struct mrb_io *fptr);
void rb_io_check_closed(mrb_state *mrb, struct mrb_io *fptr);

#if defined(__cplusplus)
} /* extern "C" { */
#endif
#endif /* MRUBY_IO_H */
