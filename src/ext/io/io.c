/*
** io.c - IO class
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#ifdef ENABLE_IO

#include "mruby/hash.h"
#include "mruby/khash.h"
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include "mruby/ext/io.h"
#include "error.h"

static mrb_value lineno;

#ifndef PIPE_BUF
#ifdef _POSIX_PIPE_BUF
#define PIPE_BUF _POSIX_PIPE_BUF
#else
#define PIPE_BUF 512 /* is this ok? */
#endif
#endif

#define io_seek(fptr, ofs, whence) fseeko(flush_before_seek(mrb, fptr)->f, ofs, whence)
#define io_tell(fptr) ftello(flush_before_seek(mrb, fptr)->f)

#if !HAVE_FSEEKO && !defined(fseeko)
#define fseeko fseek
#endif
#if !HAVE_FTELLO && !defined(ftello)
#define ftello ftell
#endif

#ifdef _STDIO_USES_IOSTREAM  /* GNU libc */
#  ifdef _IO_fpos_t
#    define READ_DATA_PENDING(fp) ((fp)->_IO_read_ptr != (fp)->_IO_read_end)
#    define READ_DATA_PENDING_COUNT(fp) ((fp)->_IO_read_end - (fp)->_IO_read_ptr)
#    define READ_DATA_PENDING_PTR(fp) ((fp)->_IO_read_ptr)
#  else
#    define READ_DATA_PENDING(fp) ((fp)->_gptr < (fp)->_egptr)
#    define READ_DATA_PENDING_COUNT(fp) ((fp)->_egptr - (fp)->_gptr)
#    define READ_DATA_PENDING_PTR(fp) ((fp)->_gptr)
#  endif
#elif defined(_LP64) && (defined(__sun__) || defined(__sun))
typedef struct _FILE64 {
  unsigned char        *_ptr;  /* next character from/to here in buffer */
  unsigned char        *_base; /* the buffer */
  unsigned char        *_end;  /* the end of the buffer */
  ssize_t      _cnt;           /* number of available characters in buffer */
  int          _file;          /* UNIX System file descriptor */
  unsigned int _flag;          /* the state of the stream */
  char         __fill[80];     /* filler to bring size to 128 bytes */
} FILE64;
#  define READ_DATA_PENDING(fp) (((FILE64*)(fp))->_cnt > 0)
#  define READ_DATA_PENDING_COUNT(fp) (((FILE64*)(fp))->_cnt)
#  define READ_DATA_PENDING_PTR(fp) ((char *)((FILE64*)(fp))->_ptr)
#elif defined(FILE_COUNT)
#  define READ_DATA_PENDING(fp) ((fp)->FILE_COUNT > 0)
#  define READ_DATA_PENDING_COUNT(fp) ((fp)->FILE_COUNT)
#elif defined(FILE_READEND)
#  define READ_DATA_PENDING(fp) ((fp)->FILE_READPTR < (fp)->FILE_READEND)
#  define READ_DATA_PENDING_COUNT(fp) ((fp)->FILE_READEND - (fp)->FILE_READPTR)
#elif defined(__BEOS__)
#  define READ_DATA_PENDING(fp) (fp->_state._eof == 0)
#elif defined(__VMS)
#  define READ_DATA_PENDING_COUNT(fp) ((unsigned int)(*(fp))->_cnt)
#  define READ_DATA_PENDING(fp)       (((unsigned int)(*(fp))->_cnt) > 0)
#  define READ_DATA_BUFFERED(fp) 0
#elif defined(__DragonFly__)
/* FILE is an incomplete struct type since DragonFly BSD 1.4.0 */
#  define READ_DATA_PENDING(fp) (((struct __FILE_public *)(fp))->_r > 0)
#  define READ_DATA_PENDING_COUNT(fp) (((struct __FILE_public *)(fp))->_r)
#else
/* requires systems own version of the ReadDataPending() */
extern int ReadDataPending();
#  define READ_DATA_PENDING(fp) (!feof(fp))
#  define READ_DATA_BUFFERED(fp) 0
#endif

#define READ_CHECK(mrb, fp) do {\
  if (!READ_DATA_PENDING(fp)) {\
    rb_io_check_closed(mrb, fptr);\
  }\
} while (0)

static int mrb_io_mode_flags(mrb_state *mrb,const char *mode);
static const char * mrb_io_modenum_to_modestr(mrb_state *mrb,int flags);
static int mrb_io_modestr_to_modenum(mrb_state *mrb,const char *mode);
static int mrb_io_oflags_fmode(int oflags);
static FILE * mrb_fdopen(mrb_state *mrb,int fd,const char *mode);
static int rb_sysopen(mrb_state *mrb, mrb_value fname,int flags,mode_t perm);
static struct mrb_io * flush_before_seek(mrb_state *mrb,struct mrb_io *fptr);
static void rb_io_check_readable(mrb_state *mrb,struct mrb_io *fptr);
static void rb_io_check_writable(mrb_state *mrb,struct mrb_io *fptr);
static void rb_io_fptr_cleanup(mrb_state *mrb,struct mrb_io *fptr,int noraise);
static mrb_value rb_io_close(mrb_state *mrb,mrb_value io);
static int appendlineOld(mrb_state *mrb,struct mrb_io *fptr,int delim,mrb_value *strp);
static int appendline(mrb_state *mrb,struct mrb_io *fptr,int delim,mrb_value *strp,long *lp);
static inline int swallow(mrb_state *mrb,struct mrb_io *fptr,int term);
static int rscheck(mrb_state *mrb,const char *rsptr,long rslen,mrb_value rs);
static mrb_value rb_io_getline_fast(mrb_state *mrb,struct mrb_io *fptr,unsigned char delim);
static mrb_value rb_io_getline_1(mrb_state *mrb,mrb_value rs,long limit,mrb_value io);
static mrb_value rb_io_getline(mrb_state *mrb,int argc,mrb_value *argv,mrb_value io);
static mrb_value rb_str_locktmp(mrb_value str);
static mrb_value rb_str_unlocktmp(mrb_value str);
static int rb_proc_exec(const char *pname);
static mrb_value pipe_open(mrb_state *mrb,mrb_value pstr,const char *pname,int modef);
static void prepare_getline_args(mrb_state *mrb,int argc,mrb_value *argv,mrb_value *rsp,long *limit,mrb_value io);
static mrb_value read_all(mrb_state *mrb,struct mrb_io *fptr,long siz,mrb_value str);
static mrb_value io_read(mrb_state *mrb,mrb_value io,int argc,mrb_value *argv);
static mrb_value io_write(mrb_state *mrb,mrb_value io,mrb_value str,int nosync);
static mrb_value mrb_io_print(mrb_state *, mrb_value);
static int wsplit_p(struct mrb_io *fptr);
static long read_buffered_data(char *ptr,long len,FILE *f);
static long io_fread(mrb_state *mrb,char *ptr,long len,struct mrb_io *fptr);
static long remain_size(mrb_state *mrb,struct mrb_io *fptr);
static long io_fwrite(mrb_state *mrb,mrb_value str,struct mrb_io *fptr);
static mrb_value io_alloc(mrb_state *mrb,struct RClass *klass);
static void io_fflush(mrb_state *mrb, struct mrb_io *fptr);

/* Imported from string.c */
extern const char* mrb_str_body(mrb_value, int *);


#include <stdarg.h>
void
mrb_warning(mrb_state *mrb, const char *fmt, ...)
{
  va_list args;
  char buf[128]; /* XXX */

  va_start(args, fmt);
  snprintf(buf, sizeof(buf), "warning: %s", fmt);
  printf(buf, args);
  va_end(args);
}

void
mrb_debug_io(mrb_state *mrb, struct mrb_io *v)
{
  if (!v) {
    printf("invalid mrb_io...\n");
    return;
  }

  printf("fd: %d, f: %d, f2: %d, mode: %d, pid: %d, lineno: %d, path: %s\n",
      v->fd, !(!v->f), !(!v->f2), v->mode, v->pid, v->lineno,
      mrb_string_value_ptr(mrb, v->path));
}

static int
mrb_io_mode_flags(mrb_state *mrb, const char *mode)
{
  int flags = 0;
  const char *m = mode;

  switch (*m++) {
    case 'r':
      flags |= FMODE_READABLE;
      break;
    case 'w':
      flags |= FMODE_WRITABLE | FMODE_CREATE;
      break;
    case 'a':
      flags |= FMODE_WRITABLE | FMODE_APPEND | FMODE_CREATE;
      break;
    default:
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal access mode %s", mode);
  }

  while (*m) {
    switch (*m++) {
      case 'b':
        flags |= FMODE_BINMODE;
        break;
      case '+':
        flags |= FMODE_READWRITE;
        break;
      case ':':
        /* XXX: PASSTHROUGH*/
      default:
        mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal access mode %s", mode);
    }
  }

  return flags;
}

#ifdef O_BINARY
#define MODE_BINARY(a, b) ((flags & O_BINARY) ? (b) : (a))
#else
#define MODE_BINARY(a, b) (a)
#endif

static const char *
mrb_io_modenum_to_modestr(mrb_state *mrb, int flags)
{
  int accmode = flags & O_ACCMODE;
  if (flags & O_APPEND) {
    if (accmode == O_WRONLY) {
      return MODE_BINARY("a", "ab");
    }
    if (accmode == O_RDWR) {
      return MODE_BINARY("a+", "ab+");
    }
  }
  switch (accmode) {
    case O_RDONLY:
      return MODE_BINARY("r", "rb");
    case O_WRONLY:
      return MODE_BINARY("w", "wb");
    case O_RDWR:
      return MODE_BINARY("r+", "rb+");
  }

  mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal access mode %o", flags);
  return NULL;
}

static int
mrb_io_modestr_to_modenum(mrb_state *mrb, const char *mode)
{
  int flags = 0;
  const char *m = mode;

  switch (*m++) {
    case 'r':
      flags |= O_RDONLY;
      break;
    case 'w':
      flags |= O_WRONLY | O_CREAT | O_TRUNC;
      break;
    case 'a':
      flags |= O_WRONLY | O_CREAT | O_APPEND;
      break;
    default:
      goto error;
  }

  while (*m) {
    switch (*m++) {
      case 'b':
#ifdef O_BINARY
        flags |= O_BINARY;
#endif
        break;
      case '+':
        flags = (flags & ~O_ACCMODE) | O_RDWR;
        break;
      case ':':
        /* PASSTHROUGH */
      default:
        goto error;
    }
  }
  return flags;
error:
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal access mode %s", mode);
  /* mrb_raise execute exit(1) */
  return 0; /* UNREACH */
}

static int
mrb_io_oflags_fmode(int oflags)
{
  int fmode = 0;

  switch (oflags & (O_RDONLY|O_WRONLY|O_RDWR)) {
    case O_RDONLY:
      fmode = FMODE_READABLE;
      break;
    case O_WRONLY:
      fmode = FMODE_WRITABLE;
      break;
    case O_RDWR:
      fmode = FMODE_READWRITE;
      break;
    }

    if (oflags & O_APPEND) {
      fmode |= FMODE_APPEND;
    }
    if (oflags & O_TRUNC) {
      fmode |= FMODE_TRUNC;
    }
    if (oflags & O_CREAT) {
      fmode |= FMODE_CREATE;
    }
#ifdef O_BINARY
    if (oflags & O_BINARY) {
      fmode |= FMODE_BINMODE;
    }
#endif

    return fmode;
}

static FILE *
mrb_fdopen(mrb_state *mrb, int fd, const char *mode)
{
  FILE *file;
  file = fdopen(fd, mode);
  if (!file) {
    mrb_sys_fail(mrb, "fdopen failed.");
  }
  return file;
}

mrb_value
rb_io_initialize(mrb_state *mrb, int argc, mrb_value *argv, mrb_value io)
{
  if (argc < 1 || argc > 2) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR,
        "wrong number of arguments (%d for 1..2)", argc);
    return mrb_nil_value();
  }

  mrb_value fnum, mode;
  struct mrb_io *fp;
  int fd, flags;

  fnum = argv[0];
  mode = argv[1];
  fd = mrb_fixnum(fnum);
  if (argc == 2) {
    if (mrb_fixnum_p(mode)) {
      flags = mrb_fixnum(mode);
    } else {
      flags = mrb_io_modestr_to_modenum(mrb, mrb_string_value_cstr(mrb, &mode));
    }
  } else {
    flags = O_RDONLY;
  }

  const char *m = mrb_io_modenum_to_modestr(mrb, flags);
  mrb_value path = mrb_str_new2(mrb, "fd ");
  mrb_value fnum_s = mrb_funcall(mrb, fnum, "to_s", 0);
  mrb_str_concat(mrb, path, fnum_s);

  MakeOpenFile(mrb, io, fp);
  fp->mode = mrb_io_mode_flags(mrb, m);
  fp->fd = fd;
  fp->f = mrb_fdopen(mrb, fd, m);
  fp->path = path;

  return io;
}

static int
rb_sysopen(mrb_state *mrb, mrb_value fname, int flags, mode_t perm)
{
  int fd;

  fd = open(RSTRING(fname)->ptr, flags, perm);
  if (fd < 0) {
    /* return -1; */
    mrb_sys_fail(mrb, "open failed.");
  }

  return fd;
}

static struct mrb_io *
flush_before_seek(mrb_state *mrb, struct mrb_io *fptr)
{
  if (fptr->mode & FMODE_WBUF) {
    io_fflush(mrb, fptr);
  }
  errno = 0;
  return fptr;
}

void
mrb_io_check_initialized(mrb_state *mrb, struct mrb_io *fptr)
{
  if (!fptr) {
    mrb_raise(mrb, E_IO_ERROR, "uninitialized stream");
  }
}

void
rb_io_check_closed(mrb_state *mrb, struct mrb_io *fptr)
{
  mrb_io_check_initialized(mrb, fptr);
  if (!fptr->f && !fptr->f2) {
    mrb_raise(mrb, E_IO_ERROR, "closed stream");
  }
}

static void
rb_io_check_readable(mrb_state *mrb, struct mrb_io *fptr)
{
  rb_io_check_closed(mrb, fptr);
  if (!(fptr->mode & FMODE_READABLE)) {
    mrb_raise(mrb, E_IO_ERROR, "not opended for reading");
  }

  fptr->mode |= FMODE_RBUF;
}

static void
rb_io_check_writable(mrb_state *mrb, struct mrb_io *fptr)
{
  rb_io_check_closed(mrb, fptr);
  if (!(fptr->mode & FMODE_WRITABLE)) {
    mrb_raise(mrb, E_IO_ERROR, "not opened for writing");
  }

  if ((fptr->mode & FMODE_RBUF) && !feof(fptr->f) && !fptr->f2) {
    io_seek(fptr, 0, SEEK_CUR);
  }
  if (!fptr->f2) {
    fptr->mode &= ~FMODE_RBUF;
  }
}

void
fptr_finalize(mrb_state *mrb, struct mrb_io *fptr, int noraise)
{
  int n1, n2, f1, f2;

  n1 = n2 = 0;
  f2 = -1;

  if (fptr == NULL) {
    return;
  }

  errno = 0; /* XXX */
  if (fptr->f2 && fptr->fd > 2) {
    f2 = fileno(fptr->f2);
    while (n2 = 0, fflush(fptr->f2) < 0) {
      n2 = errno;
      /* if (!rb_io_wait_writable(mrb, f2)) */ /* XXX */
      break;
      if (!fptr->f2)
        break;
    }

    if (fclose(fptr->f2) < 0 && n2 == 0) {
      n2 = errno;
    }
    fptr->f2 = 0;
  }

  if (fptr->f && fptr->fd > 2) {
    f1 = fileno(fptr->f);
    if ((f2 == -1) && (fptr->mode & FMODE_WBUF)) {
      while (n1 = 0, fflush(fptr->f) < 0) {
        n1 = errno;
        /* if (!rb_io_wait_writable(mrb, f1)) */ /* XXX */
        break;
        if (!fptr->f)
          break;
      }
    }
    if (fclose(fptr->f) < 0 && n1 == 0) {
      n1 = errno;
    }
    fptr->f = 0;
    if (n1 == EBADF && f1 == f2) {
      n1 = 0;
    }
  }

  if (!noraise && (n1 || n2)) {
    errno = (n1 ? n1 : n2);
    mrb_sys_fail(mrb, "fptr_finalize failed.");
  }
}

static void
rb_io_fptr_cleanup(mrb_state *mrb, struct mrb_io *fptr, int noraise)
{
  if (fptr->finalize) {
    (*fptr->finalize)(fptr, noraise);
  } else {
    fptr_finalize(mrb, fptr, noraise);
  }
}

static mrb_value
rb_io_close(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  int fd, fd2;
  int status;

  fptr = RFILE(io)->fptr;
  if (!fptr) {
    return mrb_nil_value();
  }

  if (fptr->f2) {
    fd = fileno(fptr->f2);
  } else {
    if (!fptr->f)
      return mrb_nil_value();

    fd2 = -1;
  }

  fd = fileno(fptr->f);
  rb_io_fptr_cleanup(mrb, fptr, FALSE);

  if (fptr->pid) {
    /* rb_syswait(fptr->pid); */ /* XXX */
    waitpid(fptr->pid, &status, 0);
    fptr->pid = 0;
  }

  return mrb_nil_value();
}

static int
appendlineOld(mrb_state *mrb, struct mrb_io *fptr, int delim, mrb_value *strp)
{
  FILE *f = fptr->f;
  mrb_value str = *strp;
  int c = EOF;
#ifndef READ_DATA_PENDING_PTR
  char buf[8192];
  char *bp = buf, *bpe = buf + sizeof buf - 3;
  int update = FALSE;
#endif

  do {
#ifdef READ_DATA_PENDING_PTR
    long pending = READ_DATA_PENDING_COUNT(f);
    if (pending > 0) {
      const char *p = READ_DATA_PENDING_PTR(f);
      const char *e = memchr(p, delim, pending);
      long last = 0, len = (c != EOF);
      if (e) pending = e - p + 1;
      len += pending;
      if (!mrb_nil_p(str)) {
        last = RSTRING(str)->len;
        mrb_str_resize(mrb, str, last + len);
      } else {
        *strp = str = mrb_str_buf_new(mrb, len);
        RSTRING(str)->len = len;
        RSTRING(str)->ptr[len] = '\0';
      }
      if (c != EOF) {
        RSTRING(str)->ptr[last++] = c;
      }
      fread(RSTRING(str)->ptr + last, 1, pending, f); /* must not fail */
      if (e)
        return delim;
    } else if (c != EOF) {
      if (!mrb_nil_p(str)) {
        char ch = c;
        mrb_str_buf_cat(mrb, str, &ch, 1);
      } else {
        *strp = str = mrb_str_buf_new(mrb, 1);
        RSTRING(str)->ptr[RSTRING(str)->len++] = c;
      }
    }
    rb_io_check_closed(mrb, fptr);
#else
    READ_CHECK(mrb, f);
#endif
    clearerr(f);
    /* TRAP_BEG(mrb); */
    c = getc(f);
    /* TRAP_END(mrb); */
    if (c == EOF) {
      if (ferror(f)) {
        clearerr(f);
        mrb_sys_fail(mrb, "appendlineOld failed");
        continue;
      }
#ifdef READ_DATA_PENDING_PTR
      return c;
#endif
    }
#ifndef READ_DATA_PENDING_PTR
    if (c == EOF || (*bp++ = c) == delim || bp == bpe) {
      int cnt = bp - buf;

      if (cnt > 0) {
        if (!mrb_nil_p(str))
          mrb_str_buf_cat(mrb, str, buf, cnt);
        else
          *strp = str = mrb_str_new(mrb, buf, cnt);
      }
      if (c == EOF) {
        if (update) {
          if (!mrb_nil_p(str))
            return (int)RSTRING(str)->ptr[RSTRING(str)->len-1];
        }
        return c;
      }
      bp = buf;
    }
    update = TRUE;
#endif
  } while (c != delim);

#ifdef READ_DATA_PENDING_PTR
  {
    char ch = c;
    if (!mrb_nil_p(str)) {
      mrb_str_cat(mrb, str, &ch, 1);
    }
    else {
      *strp = str = mrb_str_new(mrb, &ch, 1);
    }
  }
#endif

  return c;
}

static int
appendline(mrb_state *mrb, struct mrb_io *fptr, int delim, mrb_value *strp, long *lp)
{
  FILE *f = fptr->f;
  mrb_value str = *strp;
  int c = EOF;
#ifndef READ_DATA_PENDING_PTR
  long limit = *lp;
  char buf[8192];
  char *bp = buf, *bpe = buf + sizeof buf - 3;
  int update = FALSE;
#endif

  do {
#ifdef READ_DATA_PENDING_PTR
    long pending = READ_DATA_PENDING_COUNT(f);
    if (pending > 0) {
      const char *p = READ_DATA_PENDING_PTR(f);
      const char *e = memchr(p, delim, pending);
      long last = 0, len = (c != EOF);
      if (e) pending = e - p + 1;
      len += pending;
      if (!mrb_nil_p(str)) {
        last = RSTRING(str)->len;
        mrb_str_resize(mrb, str, last + len);
      } else {
        *strp = str = mrb_str_buf_new(mrb, len);
        RSTRING(str)->len = len;
        RSTRING(str)->ptr[len] = '\0';
      }
      if (c != EOF) {
        RSTRING(str)->ptr[last++] = c;
      }
      fread(RSTRING(str)->ptr + last, 1, pending, f); /* must not fail */
      if (e)
        return delim;
    } else if (c != EOF) {
      if (!mrb_nil_p(str)) {
        char ch = c;
        mrb_str_buf_cat(mrb, str, &ch, 1);
      } else {
        *strp = str = mrb_str_buf_new(mrb, 1);
        RSTRING(str)->ptr[RSTRING(str)->len++] = c;
      }
    }
    rb_io_check_closed(mrb, fptr);
#else
    READ_CHECK(mrb, f);
#endif
    clearerr(f);
    /* TRAP_BEG(mrb); */
    c = getc(f);
    /* TRAP_END(mrb); */
    if (c == EOF) {
      if (ferror(f)) {
        clearerr(f);
        mrb_sys_fail(mrb, "appendline failed");
        continue;
      }
#ifdef READ_DATA_PENDING_PTR
      return c;
#endif
    }
#ifndef READ_DATA_PENDING_PTR
    if (c == EOF || (*bp++ = c) == delim || bp == bpe || (bp==(buf+limit))) {
      int cnt = bp - buf;

      if (cnt > 0) {
        if (!mrb_nil_p(str))
          mrb_str_cat(mrb, str, buf, cnt);
        else
          *strp = str = mrb_str_new(mrb, buf, cnt);
      }
      if (c == EOF) {
        if (update) {
          if (!mrb_nil_p(str))
            return (int)RSTRING(str)->ptr[RSTRING(str)->len-1];
        }
        return c;
      }
      else {
        *lp = limit - (bp - buf);
        return c;
      }
      bp = buf;
    }
    update = TRUE;
#endif
  } while (c != delim);

#ifdef READ_DATA_PENDING_PTR
  {
    char ch = c;
    if (!mrb_nil_p(str)) {
      mrb_str_cat(mrb, str, &ch, 1);
    } else {
      *strp = str = mrb_str_new(mrb, &ch, 1);
    }
  }
#endif

  return c;
}

static inline int
swallow(mrb_state *mrb, struct mrb_io *fptr, int term)
{
  FILE *f = fptr->f;
  int c;
#ifdef READ_DATA_PENDING_PTR
  long cnt;
#endif

  do { 
#ifdef READ_DATA_PENDING_PTR
    while((cnt = READ_DATA_PENDING(f)) > 0) {
      char buf[1024];
      const char *p = READ_DATA_PENDING_PTR(f);
      int i;

      if (cnt > sizeof(buf))
        cnt = sizeof(buf);
      if (*p != term)
        return TRUE;

      i = cnt;
      while (--i && *++p == term);

      if (!fread(buf, 1, cnt - i, f)) {
        mrb_sys_fail(mrb, "swallow failed");
      }
    }

    rb_io_check_closed(mrb, fptr);
#else
    READ_CHECK(mrb, f);
#endif

    clearerr(f);
    /* TRAP_BEG(mrb); */
    c = getc(f);
    /* TRAP_END(mrb); */
    if (c != term) {
      ungetc(c, f);
      return TRUE;
    }
  } while (c != EOF);

  return FALSE;
}

static int
rscheck(mrb_state *mrb, const char *rsptr, long rslen, mrb_value rs)
{
  if (RSTRING(rs)->ptr != rsptr && RSTRING(rs)->len != rslen) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "rs modified");
  }
  return 1;
}

static mrb_value
rb_io_getline_fast(mrb_state *mrb, struct mrb_io *fptr, unsigned char delim)
{
  mrb_value str = mrb_nil_value();
  int c;

  while ((c = appendlineOld(mrb, fptr, delim, &str)) != EOF) {
    if (c == delim) {
      break;
    }
  }

  if (!mrb_nil_p(str)) {
    fptr->lineno++;
    lineno = mrb_fixnum_value(fptr->lineno);
  }

  return str;
}

static mrb_value
rb_io_getline_1(mrb_state *mrb, mrb_value rs, long limit, mrb_value io)
{
  mrb_value str = mrb_nil_value();
  struct mrb_io *fptr;
  int nolimit = 0;
  long svlimit = limit;

  GetOpenFile(mrb, io, fptr);
  rb_io_check_readable(mrb, fptr);
  if (mrb_nil_p(rs) && limit < 0) {
    str = read_all(mrb, fptr, 0, mrb_nil_value());
    if (RSTRING_LEN(str) == 0)
      return mrb_nil_value();
  } else if (limit == 0) {
    return mrb_str_new(mrb, 0, 0);
  } else if (RSTRING_LEN(rs) > 0 && limit < 0) {
    return rb_io_getline_fast(mrb, fptr, '\n');
  }

  int c, newline = -1;
  const char *rsptr = 0;
  long rslen = 0;
  int rspara = 0;
  int extra_limit = 16;
  const char *s, *p, *e, *pp;
  
  if (!mrb_nil_p(rs)) {
    rslen = RSTRING_LEN(rs);
    if (rslen == 0) {
      rsptr = "\n\n";
      rslen = 2;
      rspara = 1;
      swallow(mrb, fptr, '\n');
      rs = mrb_str_new_cstr(mrb, "");
    } else {
      rsptr = RSTRING_PTR(rs);
    }
    newline = (unsigned char) rsptr[rslen -1];
  }

  while ((c = appendline(mrb, fptr, newline, &str, &limit)) != EOF) {
    if (c == newline) {
      if (RSTRING_LEN(str) < rslen)
        continue;

      s = RSTRING_PTR(str);
      e = s + RSTRING_LEN(str);
      p = e - rslen;

      if (!rspara)
        rscheck(mrb, rsptr, rslen, rs);
      if (memcmp(p, rsptr, rslen) == 0)
        break;
    }

    if (limit == 0) {
      limit = svlimit;
      s = RSTRING_PTR(str);
      p = s + RSTRING_LEN(str);
      pp = p - 1;
      if (extra_limit && ((p - pp) < -1)) {
        limit = 1;
        extra_limit--;
      } else {
        nolimit = 1;
        break;
      }
    }
  } /* end of whiles */

  if (rspara) {
    if (c != EOF) {
      swallow(mrb, fptr, '\n');
    }
  }

  if (!mrb_nil_p(str)) {
    if (!nolimit) {
      fptr->lineno++;
      lineno = mrb_fixnum_value(fptr->lineno);
    }
  }

  return str;
}

static mrb_value
rb_io_getline(mrb_state *mrb, int argc, mrb_value *argv, mrb_value io)
{
  mrb_value rs;
  long limit;

  prepare_getline_args(mrb, argc, argv, &rs, &limit, io);
  return rb_io_getline_1(mrb, rs, limit, io);
}

static mrb_value
rb_str_locktmp(mrb_value str)
{
  return str;
}

static mrb_value
rb_str_unlocktmp(mrb_value str)
{
  return str;
}

#if !defined NOFILE
#define NOFILE 64
#endif

static int
rb_proc_exec(const char *pname)
{
  const char *s;
  s = pname;

  while (*s == ' ' || *s == '\t' || *s == '\n')
    s++;

  if (!*s) {
    errno = ENOENT;
    return -1;
  }

  /* XXX */
  execl("/bin/sh", "sh", "-c", pname, (char *)NULL); /* async-signal-safe */
  return -1;
}

/* XXX */
static mrb_value
pipe_open(mrb_state *mrb, mrb_value pstr, const char *pname, int modef)
{
  struct mrb_io *fptr;
  int pid, fd, write_fd;
  int pr[2], pw[2];
  volatile int doexec;

  if (!pname)
    pname = mrb_string_value_cstr(mrb, &pstr);

  doexec = (strcmp("-", pname) != 0);

  if (((modef & FMODE_READABLE) && pipe(pr) == -1)
      || ((modef & FMODE_WRITABLE) && pipe(pw) == -1))
    mrb_sys_fail(mrb, "pipe_open failed.");

  if (!doexec) {
    fflush(stdin);
    fflush(stdout);
    fflush(stderr);
  }

retry:
  switch ((pid = fork())) {
  case 0: /* child */
    if (modef & FMODE_READABLE) {
      close(pr[0]);
      if (pr[1] != 1) {
        dup2(pr[1], 1);
        close(pr[1]);
      }
    }
    if (modef & FMODE_WRITABLE) {
      close(pw[1]);
      if (pw[0] != 0) {
        dup2(pw[0], 0);
        close(pw[0]);
      }
    }

    if (doexec) {
      int fd;

      /* XXX: WARNING!! */
      for (fd = 3; fd < NOFILE; fd++) {
        close(fd);
      }
      rb_proc_exec(pname); /* XXX */
      fprintf(stderr, "command not found: %s\n", pname);
      _exit(127);
    }
    return mrb_nil_value();
  case -1: /* error */
    if (errno == EAGAIN) {
      goto retry;
    } else {
      int e = errno;
      if ((modef & FMODE_READABLE)) {
        close(pr[0]);
        close(pr[1]);
      }
      if ((modef & FMODE_WRITABLE)) {
        close(pw[0]);
        close(pw[1]);
      }

      errno = e;
      mrb_sys_fail(mrb, "pipe_open failed.");
    }
    break;
  default: /* parent */
    if (pid < 0) {
      mrb_sys_fail(mrb, "pipe_open failed.");
    } else {
      mrb_value port = io_alloc(mrb, mrb->io_class);

      MakeOpenFile(mrb, port, fptr);

      if ((modef & FMODE_READABLE) && (modef & FMODE_WRITABLE)) {
        close(pr[1]);
        fd = pr[0];
        close(pw[0]);
        write_fd = pw[1];
      } else if (modef & FMODE_READABLE) {
        close(pr[1]);
        fd = pr[0];
      } else {
        close(pw[0]);
        fd = pw[1];
      }

      fptr->fd = fd;
      fptr->mode = modef;
      fptr->mode |= FMODE_SYNC;
      if (modef & FMODE_READABLE) {
        close(pr[1]);
        fptr->f = mrb_fdopen(mrb, pr[0], "r");
      }
      if (modef & FMODE_WRITABLE) {
        FILE *f = mrb_fdopen(mrb, pw[1], "w");

        close(pw[0]);
        if (fptr->f)
          fptr->f2 = f;
        else 
          fptr->f = f;
      }
      fptr->pid = pid;

      return port;
    }
  }

  return mrb_nil_value();
}

static void
prepare_getline_args(mrb_state *mrb, int argc, mrb_value *argv, mrb_value *rsp, long *limit, mrb_value io)
{
  mrb_value rs = mrb_gv_get(mrb, mrb_intern(mrb, "$/"));
  mrb_value lim = mrb_nil_value();

  if (argc == 1) {
    mrb_value tmp = mrb_nil_value();

    if (mrb_type(argv[0]) == MRB_TT_FIXNUM) {
      lim = argv[0];
    } else {
      if (mrb_nil_p(argv[0]) || !mrb_nil_p(tmp = mrb_check_string_type(mrb, argv[0]))) {
        rs = tmp;
      } else {
        lim = argv[0];
      }
    }
  } else if (argc >= 2) {
    mrb_get_args(mrb, "oo", &rs, &lim);
    if (!mrb_nil_p(rs))
      mrb_string_value(mrb, &rs);
  }

  *rsp = rs;
  *limit = mrb_nil_p(lim) ? -1L : mrb_fixnum(lim);
}

static mrb_value
read_all(mrb_state *mrb, struct mrb_io *fptr, long siz, mrb_value str)
{
  long bytes = 0;
  long n;

  if (siz == 0)
    siz = BUFSIZ;

  if (mrb_nil_p(str)) {
    str = mrb_str_new(mrb, 0, siz);
  } else {
    mrb_str_resize(mrb, str, siz);
  }

  for (;;) {
    rb_str_locktmp(str);
    READ_CHECK(mrb, fptr->f);
    n = io_fread(mrb, RSTRING(str)->ptr + bytes, siz - bytes, fptr);
    rb_str_unlocktmp(str);
    if (n == 0 && bytes == 0) {
      if (!fptr->f)
        break;

      if (feof(fptr->f))
        break;

      if (!ferror(fptr->f))
        break;

      mrb_sys_fail(mrb, "read_all failed");
    }
    bytes += n;
    if (bytes < siz)
      break;
    siz += BUFSIZ;
    mrb_str_resize(mrb, str, siz);
  }
  if (bytes != siz)
    mrb_str_resize(mrb, str, bytes);

  return str;
}

static mrb_value
io_read(mrb_state *mrb, mrb_value io, int argc, mrb_value *argv)
{
  mrb_value length, str;
  long n, len;
  struct mrb_io *fptr;

  length = mrb_nil_value();
  str = mrb_nil_value();

  if (argc >= 1)
    length = argv[0];
  if (argc >= 2)
    str = argv[1];

  if (mrb_nil_p(length)) {
    if (!mrb_nil_p(str))
      mrb_string_value(mrb, &str);
    GetOpenFile(mrb, io, fptr);
    return read_all(mrb, fptr, remain_size(mrb, fptr), str);
  }

  len = mrb_fixnum(length);
  if (len < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative length given");
  }

  if (mrb_nil_p(str)) {
    str = mrb_str_new(mrb, 0, len);
  } else {
    mrb_string_value(mrb, &str);
    /* rb_str_modify(str); */ /* XXX */
    mrb_str_resize(mrb, str, len);
  }

  GetOpenFile(mrb, io, fptr);
  rb_io_check_readable(mrb, fptr);
  if (feof(fptr->f))
    return mrb_nil_value();

  if (len == 0)
    return str;

  rb_str_locktmp(str);
  READ_CHECK(mrb, fptr->f);
  if (RSTRING(str)->len != len) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "buffer string modified");
  }

  n = io_fread(mrb, RSTRING(str)->ptr, len, fptr);
  rb_str_unlocktmp(str);
  if (n == 0) {
    if (!fptr->f)
      return mrb_nil_value();

    if (feof(fptr->f)) {
      mrb_str_resize(mrb, str, 0);
      return mrb_nil_value();
    }

    if (len > 0) 
      mrb_sys_fail(mrb, "io_read failed.");
  }

  mrb_str_resize(mrb, str, n);
  RSTRING(str)->len = n;
  RSTRING(str)->ptr[n] = '\0';

  return str;
}

static mrb_value
io_write(mrb_state *mrb, mrb_value io, mrb_value str, int nosync)
{
  struct mrb_io *fptr;
  long n;

  if (mrb_type(str) != MRB_TT_STRING)
    str = mrb_obj_as_string(mrb, str);

  if (mrb_type(io) != MRB_TT_FILE) {
    return mrb_funcall(mrb, io, "write", 1, str);
  }

  if (RSTRING(str)->len == 0)
    return mrb_fixnum_value(0);

  GetOpenFile(mrb, io, fptr);
  rb_io_check_writable(mrb, fptr);

  n = io_fwrite(mrb, str, fptr);
  if (n == -1L)
    mrb_sys_fail(mrb, "io_write failed.");

  if (!(fptr->mode & FMODE_SYNC))
    fptr->mode |= FMODE_WBUF;

  return mrb_fixnum_value(n);
}

static mrb_value
mrb_io_print(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;
  int i;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc == 0)
    return mrb_nil_value();

  for (i = 0; i < argc; i++)
    io_write(mrb, klass, argv[i], 0);

  return mrb_nil_value();
}

#ifndef S_ISREG
#define S_ISREG(m) ((m & S_IFMT) == S_IFREG)
#endif

/* TODO: need implementation fcntl */
static int
wsplit_p(struct mrb_io *fptr)
{
  FILE *f = GetWriteFile(fptr);

  if (!(fptr->mode & FMODE_WSPLIT_INITIALIZED)) {
    struct stat buf;
    if (fstat(fileno(f), &buf) == 0 && !S_ISREG(buf.st_mode)) {
      fptr->mode |= FMODE_WSPLIT;
    }
    fptr->mode |= FMODE_WSPLIT_INITIALIZED;
  }

  return fptr->mode & FMODE_WSPLIT;
}

static long
read_buffered_data(char *ptr, long len, FILE *f)
{
  long n;

#ifdef READ_DATA_PENDING_COUNT
  n = READ_DATA_PENDING_COUNT(f);
  if (n <= 0) {
    return 0;
  } else if (n > len) {
    n = len;
  }
  return fread(ptr, 1, n, f);
#else
  int c;

  for (n = 0; n < len; n++) {
    if (!READ_DATA_PENDING(f))
      break;

    c = getc(f);

    if (c == EOF)
      break;

    *ptr++ = c;
  }
  return n;
#endif
}

static long
io_fread(mrb_state *mrb, char *ptr, long len, struct mrb_io *fptr)
{
  long n = len;
  int c;
  int saved_errno;

  while (n > 0) {
    c = read_buffered_data(ptr, n, fptr->f);
    if (c < 0)
      goto eof;
    if (c > 0) {
      ptr += c;
      n -= c;
      if (n <= 0)
        break;
    }

    rb_io_check_closed(mrb, fptr);
    clearerr(fptr->f);
    /* TRAP_BEG(mrb); */ /* XXX */
    c = getc(fptr->f);
    /* TRAP_END(mrb); */ /* XXX */
    if (c == EOF) {
eof:
      if (ferror(fptr->f)) {
        switch (errno) {
          case EINTR:
#if defined(ERESTART)
          case ERESTART:
#endif
            clearerr(fptr->f);
            continue;
          case EAGAIN:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
          case EWOULDBLOCK:
#endif
            if (len > n) {
              clearerr(fptr->f);
            }
            saved_errno = errno;
            mrb_warning(mrb, 
                "nonblocking IO#read is obsolete; use IO#readpartial or IO#sysread");
            errno = saved_errno;
        }
        if (len == n)
          return 0;
      }
      break;
    }
    *ptr++ = c;
    n--;
  }

  return len - n;
}

static long
remain_size(mrb_state *mrb, struct mrb_io *fptr)
{
  struct stat st;
  off_t siz = BUFSIZ;
  off_t pos;

  if (feof(fptr->f))
    return 0;

  if (fstat(fileno(fptr->f), &st) == 0 && S_ISREG(st.st_mode)) {
    pos = io_tell(fptr);
    if (st.st_size >= pos && pos >= 0) {
      siz = st.st_size - pos + 1;
      if (siz > LONG_MAX) {
        mrb_raise(mrb, E_IO_ERROR, "file too big for single read");
      }
    }
  }

  return (long)siz; /* XXX */
}

static long
io_fwrite(mrb_state *mrb, mrb_value str, struct mrb_io *fptr)
{
  long len, n, r, l, offset = 0;
  FILE *f = GetWriteFile(fptr);

  len = RSTRING(str)->len;
  n = len;
  if (n <= 0)
    return n;

  if (fptr->mode & FMODE_SYNC) {
    io_fflush(mrb, fptr);

    l = n;
    if (PIPE_BUF < l && wsplit_p(fptr)) {
      l = PIPE_BUF;
    }
    /* TRAP_BEG(mrb); */ /* XXX */
    r = write(fileno(f), RSTRING(str)->ptr + offset, l);
    /* TRAP_END(mrb); */ /* XXX */

    if (r == n)
      return len;
    if (0 <= r) {
      offset += r;
      n -= r;
      errno = EAGAIN;
    }

    return -1L;
  }

  do {
    r = fwrite(RSTRING(str)->ptr + offset, 1, n, f);
    offset += r;
    n -= r;
    if (errno != 0) {
      break;
    }

    if (ferror(f)) {
      return -1L;
    }
  } while (n > 0);

  return len - n;
}

static mrb_value
io_alloc(mrb_state *mrb, struct RClass *klass)
{
  struct RFile *io;
  io = (struct RFile *)mrb_obj_alloc(mrb, MRB_TT_FILE, klass); /* XXX */

  io->fptr = 0;
  return mrb_obj_value(io);
}

static void
io_fflush(mrb_state *mrb, struct mrb_io *fptr)
{
  int n;

  for (;;) {
    /* TRAP_BEG(mrb); */ /* XXX */
    n = fflush(GetWriteFile(fptr));
    /* TRAP_END(mrb); */ /* XXX */

    if (n != EOF)
      break;

    mrb_sys_fail(mrb, "io_fflush failed");
  }

  fptr->mode &= ~FMODE_WBUF;
}

mrb_value
mrb_io_s_open(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;
  mrb_value io;

  mrb_get_args(mrb, "*", &argv, &argc);
  io = io_alloc(mrb, mrb_class_ptr(klass));
  return rb_io_initialize(mrb, argc, argv, io);
}

mrb_value
mrb_io_s_sysopen(mrb_state *mrb, mrb_value klass)
{
  int argc;
  mrb_value *argv;
  int oflags, fd;
  mrb_value fname, vmode, vperm;
  mode_t perm;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc < 1 || argc > 3) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR,
        "wrong number of arguments (%d for 1..2)", argc);
    return mrb_nil_value();
  }

  fname = argv[0];
  vmode = argv[1];
  vperm = argv[2];

  if (mrb_nil_p(vmode)) {
    oflags = O_RDONLY;
  } else if (mrb_fixnum_p(vmode)) {
    oflags = mrb_fixnum(vmode);
  } else {
    oflags = mrb_io_modestr_to_modenum(mrb, mrb_string_value_cstr(mrb, &vmode));
  }

  if (mrb_nil_p(vperm)) {
    perm = 0666;
  } else {
    perm = (mode_t)mrb_fixnum(vperm);
  }

  fd = rb_sysopen(mrb, fname, oflags, perm);

  return mrb_fixnum_value(fd);
}

mrb_value
mrb_io_s_popen(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;
  int oflags;
  mrb_value pname, pmode, port;
  mrb_value io;

  mrb_get_args(mrb, "*", &argv, &argc);
  io = io_alloc(mrb, mrb_class_ptr(klass));

  if (argc < 1) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR,
        "wrong number of arguments (%d for 1..)", argc);
    return mrb_nil_value();
  }

  pname = argv[0];
  pmode = argv[1];

  if (mrb_nil_p(pmode)) {
    oflags = O_RDONLY;
  } else if (mrb_fixnum_p(pmode)) {
    oflags = mrb_fixnum(pmode);
  } else {
    oflags = mrb_io_modestr_to_modenum(mrb, mrb_string_value_cstr(mrb, &pmode));
  }

  mrb_string_value(mrb, &pname);
  port = pipe_open(mrb, pname, 0, mrb_io_oflags_fmode(oflags)); /* XXX */
  if (mrb_nil_p(port)) {
    /* XXX: NOT SUPPORTED yet */
    /*
    if (rb_block_given_p()) {
      rb_yield(mrb_nil_value());
      fflush(stdout);
      fflush(stderr);
      _exit(0);
    }
    */
    return mrb_nil_value();
  }

  mrb_basic(port)->c = mrb_class_ptr(klass);
  /* XXX: NOT SUPPORTED yet */
  /*
  if (rb_block_given_p()) {
    return rb_ensure(rb_yield, port, io_close, port);
  }
  */
  return port;
}

mrb_value
mrb_io_close(mrb_state *mrb, mrb_value klass)
{
  rb_io_check_closed(mrb, RFILE(klass)->fptr);
  rb_io_close(mrb, klass);
  return mrb_nil_value();
}

mrb_value
mrb_io_closed(mrb_state *mrb, mrb_value klass)
{
  struct mrb_io *fptr;

  fptr = RFILE(klass)->fptr;
  mrb_io_check_initialized(mrb, fptr);
  return (fptr->f || fptr->f2) ? mrb_false_value() : mrb_true_value();
}

mrb_value
mrb_io_read(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  return io_read(mrb, klass, argc, argv);
}

mrb_value
mrb_io_write(mrb_state *mrb, mrb_value klass)
{
  mrb_value str;

  mrb_get_args(mrb, "o", &str);
  return io_write(mrb, klass, str, 0);
}

mrb_value
mrb_io_flush(mrb_state *mrb, mrb_value self)
{
  struct mrb_io *fptr;

  GetOpenFile(mrb, self, fptr);
  io_fflush(mrb, fptr);
  return self;
}

mrb_value
mrb_io_sync(mrb_state *mrb, mrb_value klass)
{
  struct mrb_io *fptr;

  GetOpenFile(mrb, klass, fptr);
  if (fptr->mode & FMODE_SYNC) {
    return mrb_true_value();
  }

  return mrb_false_value();
}

mrb_value
mrb_io_set_sync(mrb_state *mrb, mrb_value klass)
{
  struct mrb_io *fptr;
  mrb_value mode;

  mrb_get_args(mrb, "o", &mode);
  GetOpenFile(mrb, klass, fptr);
  if (mrb_test(mode)) {
    fptr->mode |= FMODE_SYNC;
  } else {
    fptr->mode &= ~FMODE_SYNC;
  }
  return mode;
}

mrb_value
mrb_io_initialize(mrb_state *mrb, mrb_value io)
{
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  return rb_io_initialize(mrb, argc, argv, io);
}

mrb_value
mrb_io_to_io(mrb_state *mrb, mrb_value self)
{
  return self;
}

static struct timeval
time2timeval(mrb_state *mrb, mrb_value time)
{
  struct timeval t;

  switch (mrb_type(time)) {
  case MRB_TT_FIXNUM:
    t.tv_sec = mrb_fixnum(time);
    t.tv_usec = 0;
    break;

  case MRB_TT_FLOAT:
    t.tv_sec = mrb_float(time);
    t.tv_usec = (mrb_float(time) - t.tv_sec) * 1000000.0;
    break;

  default:
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument class");
  }

  return t;
}

static mrb_value
mrb_io_select(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;
  mrb_value read, write, except, timeout, list;
  struct timeval *tp, timerec;
  fd_set pset, rset, wset, eset;
  fd_set *rp, *wp, *ep;
  struct mrb_io *fptr;
  int pending = 0;
  mrb_value result;
  int max = 0;
  int interrupt_flag = 0;
  int i, n;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc < 1 || argc > 4) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR,
	      "wrong number of arguments (%d for 1..4)", argc);
    return mrb_nil_value();
  }

  timeout = mrb_nil_value();
  except = mrb_nil_value();
  write = mrb_nil_value();
  if (argc > 3)
    timeout = argv[3];
  if (argc > 2)
    except = argv[2];
  if (argc > 1)
    write = argv[1];
  read = argv[0];

  if (mrb_nil_p(timeout)) {
    tp = NULL;
  } else {
    timerec = time2timeval(mrb, timeout);
    tp = &timerec;
  }

  FD_ZERO(&pset);
  if (!mrb_nil_p(read)) {
    mrb_check_type(mrb, read, MRB_TT_ARRAY);
    rp = &rset;
    FD_ZERO(rp);
    for (i = 0; i < RARRAY_LEN(read); i++) {
      GetOpenFile(mrb, RARRAY_PTR(read)[i], fptr);
      FD_SET(fileno(fptr->f), rp);
      if (READ_DATA_PENDING(fptr->f)) {
        pending++;
        FD_SET(fileno(fptr->f), &pset);
      }
      if (max < fileno(fptr->f)) max = fileno(fptr->f);
    }
    if (pending) {
      timerec.tv_sec = timerec.tv_usec = 0;
      tp = &timerec;
    }
  } else {
    rp = NULL;
  }

  if (!mrb_nil_p(write)) {
    mrb_check_type(mrb, write, MRB_TT_ARRAY);
    wp = &wset;
    FD_ZERO(wp);
    for (i = 0; i < RARRAY_LEN(write); i++) {
      GetOpenFile(mrb, RARRAY_PTR(write)[i], fptr);
      FD_SET(fileno(fptr->f), wp);
      if (max < fileno(fptr->f))
        max = fileno(fptr->f);
      if (fptr->f2) {
        FD_SET(fileno(fptr->f2), wp);
        if (max < fileno(fptr->f2))
          max = fileno(fptr->f2);
      }
    }
  } else {
    wp = NULL;
  }

  if (!mrb_nil_p(except)) {
    mrb_check_type(mrb, except, MRB_TT_ARRAY);
    ep = &eset;
    FD_ZERO(ep);
    for (i = 0; i < RARRAY_LEN(except); i++) {
      GetOpenFile(mrb, RARRAY_PTR(except)[i], fptr);
      FD_SET(fileno(fptr->f), ep);
      if (max < fileno(fptr->f))
        max = fileno(fptr->f);
      if (fptr->f2) {
        FD_SET(fileno(fptr->f2), ep);
        if (max < fileno(fptr->f2))
          max = fileno(fptr->f2);
      }
    }
  } else {
    ep = NULL;
  }

  max++;

retry:
  n = select(max, rp, wp, ep, tp);
  if (n < 0) {
    if (errno != EINTR)
      mrb_sys_fail(mrb, "select failed");
    if (tp == NULL)
      goto retry;
    interrupt_flag = 1;
  }

  if (!pending && n == 0)
    return mrb_nil_value();

  result = mrb_ary_new_capa(mrb, 3);
  mrb_ary_push(mrb, result, rp? mrb_ary_new(mrb) : mrb_ary_new_capa(mrb, 0));
  mrb_ary_push(mrb, result, wp? mrb_ary_new(mrb) : mrb_ary_new_capa(mrb, 0));
  mrb_ary_push(mrb, result, ep? mrb_ary_new(mrb) : mrb_ary_new_capa(mrb, 0));

  if (interrupt_flag == 0) {
    if (rp) {
      list = RARRAY_PTR(result)[0];
      for (i = 0; i < RARRAY_LEN(read); i++) {
        GetOpenFile(mrb, RARRAY_PTR(read)[i], fptr);
	if (FD_ISSET(fileno(fptr->f), rp) ||
	    FD_ISSET(fileno(fptr->f), &pset)) {
	  mrb_ary_push(mrb, list, RARRAY_PTR(read)[i]);
	}
      }
    }

    if (wp) {
      list = RARRAY_PTR(result)[1];
      for (i = 0; i < RARRAY_LEN(write); i++) {
        GetOpenFile(mrb, RARRAY_PTR(write)[i], fptr);
	if (FD_ISSET(fileno(fptr->f), wp)) {
	  mrb_ary_push(mrb, list, RARRAY_PTR(write)[i]);
	} else if (fptr->f2 && FD_ISSET(fileno(fptr->f2), wp)) {
	  mrb_ary_push(mrb, list, RARRAY_PTR(write)[i]);
	}
      }
    }

    if (ep) {
      list = RARRAY_PTR(result)[2];
      for (i = 0; i < RARRAY_LEN(except); i++) {
        GetOpenFile(mrb, RARRAY_PTR(except)[i], fptr);
	if (FD_ISSET(fileno(fptr->f), ep)) {
	  mrb_ary_push(mrb, list, RARRAY_PTR(except)[i]);
	} else if (fptr->f2 && FD_ISSET(fileno(fptr->f2), wp)) {
	  mrb_ary_push(mrb, list, RARRAY_PTR(except)[i]);
	}
      }
    }
  }

  return result;
}

static mrb_value
mrb_io_getc(mrb_state *mrb, mrb_value klass)
{
  mrb_int argc = 1;
  mrb_value length = mrb_fixnum_value(1);
  mrb_value *argv = &length;

  return io_read(mrb, klass, argc, argv);
}

static mrb_value
mrb_io_gets(mrb_state *mrb, mrb_value klass)
{
  mrb_value str;
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  str = rb_io_getline(mrb, argc, argv, klass);
  if (mrb_nil_p(str)) {
    mrb_gv_set(mrb, mrb_intern(mrb, "$_"), mrb_nil_value());
  } else {
    mrb_gv_set(mrb, mrb_intern(mrb, "$_"), mrb_str_dup(mrb, str));
  }

  return str;
}

void
mrb_init_io(mrb_state *mrb)
{
  struct RClass *io;

  io = mrb->io_class = mrb_define_class(mrb, "IO", mrb->object_class);
  MRB_SET_INSTANCE_TT(io, MRB_TT_FILE);

  mrb_include_module(mrb, io, mrb_class_get(mrb, "Enumerable")); /* 15.2.20.3 */
  mrb_define_class_method(mrb, io, "open", mrb_io_s_open, ARGS_ANY()); /* 15.2.20.4.1 */
  mrb_define_class_method(mrb, io, "sysopen", mrb_io_s_sysopen, ARGS_ANY()); /* ??.?.??.?.? */
  mrb_define_class_method(mrb, io, "__popen__", mrb_io_s_popen, ARGS_ANY()); /* ??.?.??.?.? */
  mrb_define_class_method(mrb, io, "select", mrb_io_select, ARGS_ANY()); /* ?? */

  mrb_define_method(mrb, io, "close", mrb_io_close, ARGS_NONE()); /* 15.2.20.5.1 */
  mrb_define_method(mrb, io, "closed?", mrb_io_closed, ARGS_NONE()); /* 15.2.20.5.2 */
  mrb_define_method(mrb, io, "flush", mrb_io_flush, ARGS_NONE()); /* TBD */
  mrb_define_method(mrb, io, "print", mrb_io_print, ARGS_ANY()); /* 15.2.20.5.11 */
  mrb_define_method(mrb, io, "read", mrb_io_read, ARGS_ANY());  /* 15.2.20.5.14 */
  mrb_define_method(mrb, io, "sync", mrb_io_sync, ARGS_NONE()); /* 15.2.20.5.18 */
  mrb_define_method(mrb, io, "sync=", mrb_io_set_sync, ARGS_REQ(1));   /* 15.2.20.5.19 */
  mrb_define_method(mrb, io, "write", mrb_io_write, ARGS_REQ(1)); /* 15.2.20.5.20 */

  mrb_define_method(mrb, io, "initialize", mrb_io_initialize, ARGS_ANY());    /* 15.2.20.5.21 (x)*/
  mrb_define_method(mrb, io, "to_io", mrb_io_to_io, ARGS_NONE()); /* 15.2.20.5.22 (x) */

  mrb_define_method(mrb, io, "getc", mrb_io_getc, ARGS_NONE()); /* ??? */
  mrb_define_method(mrb, io, "gets", mrb_io_gets, ARGS_NONE()); /* ??? */

  /* TODO: ADD Kernel Module */
  /* mrb_define_method(mrb, mrb->kernel_module, "open", mrb_io_f_open, ARGS_ANY()); */

  mrb_gv_set(mrb, mrb_intern(mrb, "$/"), mrb_str_new_cstr(mrb, "\n"));
}
#endif /* ENABLE_IO */
