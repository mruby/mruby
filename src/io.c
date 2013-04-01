/*
** io.c - IO class
*/

#include "mruby.h"

#include "mruby/hash.h"
#include "mruby/data.h"
#include "mruby/khash.h"
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include "mruby/ext/io.h"
#include "error.h"

static int mrb_io_modestr_to_flags(mrb_state *mrb, const char *modestr);
static int mrb_io_modenum_to_flags(mrb_state *mrb, int modenum);
static int mrb_io_flags_to_modenum(mrb_state *mrb, int flags);

static int
mrb_io_modestr_to_flags(mrb_state *mrb, const char *mode)
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

static int
mrb_io_modenum_to_flags(mrb_state *mrb, int modenum)
{
  int flags = 0;

  switch (modenum & (O_RDONLY|O_WRONLY|O_RDWR)) {
    case O_RDONLY:
      flags = FMODE_READABLE;
      break;
    case O_WRONLY:
      flags = FMODE_WRITABLE;
      break;
    case O_RDWR:
      flags = FMODE_READWRITE;
      break;
  }

  if (modenum & O_APPEND) {
    flags |= FMODE_APPEND;
  }
  if (modenum & O_TRUNC) {
    flags |= FMODE_TRUNC;
  }
  if (modenum & O_CREAT) {
    flags |= FMODE_CREATE;
  }
#ifdef O_BINARY
  if (modenum & O_BINARY) {
    flags |= FMODE_BINMODE;
  }
#endif

  return flags;
}

static int
mrb_io_flags_to_modenum(mrb_state *mrb, int flags)
{
  int modenum = 0;

  switch(flags & (FMODE_READABLE|FMODE_WRITABLE|FMODE_READWRITE)) {
    case FMODE_READABLE:
      modenum = O_RDONLY;
      break;
    case FMODE_WRITABLE:
      modenum = O_WRONLY;
      break;
    case FMODE_READWRITE:
      modenum = O_RDWR;
      break;
  }

  if (flags & FMODE_APPEND) {
    modenum |= O_APPEND;
  }
  if (flags & FMODE_TRUNC) {
    modenum |= O_TRUNC;
  }
  if (flags & FMODE_CREATE) {
    modenum |= O_CREAT;
  }
#ifdef O_BINARY
  if (flags & FMODE_BINMODE) {
    modenum |= O_BINARY
  }
#endif

  return modenum;
}

static int
mrb_proc_exec(const char *pname)
{
  const char *s;
  s = pname;

  while (*s == ' ' || *s == '\t' || *s == '\n')
    s++;

  if (!*s) {
    errno = ENOENT;
    return -1;
  }

  execl("/bin/sh", "sh", "-c", pname, (char *)NULL);
  return -1;
}

static void
mrb_io_free(mrb_state *mrb, void *ptr)
{
  struct mrb_io *io = (struct mrb_io *)ptr;
  if (io != NULL) {
    fptr_finalize(mrb, io, TRUE);
    mrb_free(mrb, io);
  }
}

struct mrb_data_type mrb_io_type = { "IO", mrb_io_free };

static struct mrb_io*
mrb_io_alloc(mrb_state *mrb)
{
  struct mrb_io *fptr;

  fptr = (struct mrb_io *)mrb_malloc(mrb, sizeof(struct mrb_io));
  fptr->fd       = -1;
  fptr->fd2      = -1;
  fptr->pid      = 0;

  return fptr;
}

static int
io_open(mrb_state *mrb, mrb_value path, int flags, int perm)
{
  const char *pat;
  int modenum;

  pat = mrb_string_value_cstr(mrb, &path);
  modenum = mrb_io_flags_to_modenum(mrb, flags);

  return open(pat, modenum, perm);
}

#ifndef NOFILE
#define NOFILE 64
#endif

mrb_value
mrb_io_s_popen(mrb_state *mrb, mrb_value klass)
{
  mrb_value cmd, io;
  mrb_value mode = mrb_str_new_cstr(mrb, "r");
  mrb_value opt  = mrb_hash_new(mrb);

  struct mrb_io *fptr;
  const char *pname;
  int pid, flags, fd, write_fd = -1;
  int pr[2], pw[2];
  int doexec;

  mrb_get_args(mrb, "S|SH", &cmd, &mode, &opt);
  io = mrb_obj_value(mrb_data_object_alloc(mrb, mrb_class_ptr(klass), NULL, &mrb_io_type));

  pname = mrb_string_value_cstr(mrb, &cmd);
  flags = mrb_io_modestr_to_flags(mrb, mrb_string_value_cstr(mrb, &mode));

  doexec = (strcmp("-", pname) != 0);

  if (((flags & FMODE_READABLE) && pipe(pr) == -1)
      || ((flags & FMODE_WRITABLE) && pipe(pw) == -1)) {
    mrb_sys_fail(mrb, "pipe_open failed.");
    return mrb_nil_value();
  }

  if (!doexec) {
    // XXX
    fflush(stdin);
    fflush(stdout);
    fflush(stderr);
  }

retry:
  switch (pid = fork()) {
    case 0: /* child */
      if (flags & FMODE_READABLE) {
        close(pr[0]);
        if (pr[1] != 1) {
          dup2(pr[1], 1);
          close(pr[1]);
        }
      }
      if (flags & FMODE_WRITABLE) {
        close(pw[1]);
        if (pw[0] != 0) {
          dup2(pw[0], 0);
          close(pw[0]);
        }
      }

      if (doexec) {
        for (fd = 3; fd < NOFILE; fd++) {
          close(fd);
        }
        mrb_proc_exec(pname);
        mrb_raisef(mrb, E_IO_ERROR, "command not found: %s", pname);
        _exit(127);
      }
      return mrb_nil_value();
    case -1: /* error */
      if (errno == EAGAIN) {
        goto retry;
      } else {
        int e = errno;
        if (flags & FMODE_READABLE) {
          close(pr[0]);
          close(pr[1]);
        }
        if (flags & FMODE_WRITABLE) {
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
        return mrb_nil_value();
      } else {
        if ((flags & FMODE_READABLE) && (flags & FMODE_WRITABLE)) {
          close(pr[1]);
          fd = pr[0];
          close(pw[0]);
          write_fd = pw[1];
        } else if (flags & FMODE_READABLE) {
          close(pr[1]);
          fd = pr[0];
        } else {
          close(pw[0]);
          fd = pw[1];
        }

        mrb_iv_set(mrb, io, mrb_intern(mrb, "@buf"), mrb_str_new_cstr(mrb, ""));
        mrb_iv_set(mrb, io, mrb_intern(mrb, "@pos"), mrb_fixnum_value(0));

        fptr = mrb_io_alloc(mrb);
        fptr->fd   = fd;
        fptr->fd2  = write_fd;
        fptr->pid  = pid;

        DATA_TYPE(io) = &mrb_io_type;
        DATA_PTR(io)  = fptr;
        return io;
      }
  }

  return mrb_nil_value();
}

mrb_value
mrb_io_initialize(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  mrb_int fd, flags;
  mrb_value mode, opt;

  DATA_TYPE(io) = &mrb_io_type;
  DATA_PTR(io)  = NULL;

  mode = opt = mrb_nil_value();

  mrb_get_args(mrb, "i|So", &fd, &mode, &opt);
  if (mrb_nil_p(mode)) {
    mode = mrb_str_new_cstr(mrb, "r");
  }
  if (mrb_nil_p(opt)) {
    opt = mrb_hash_new(mrb);
  }

  flags   = mrb_io_modestr_to_flags(mrb, mrb_string_value_cstr(mrb, &mode));

  mrb_iv_set(mrb, io, mrb_intern(mrb, "@buf"), mrb_str_new_cstr(mrb, ""));
  mrb_iv_set(mrb, io, mrb_intern(mrb, "@pos"), mrb_fixnum_value(0));

  fptr = DATA_PTR(io);
  if (fptr == NULL) {
    fptr = mrb_io_alloc(mrb);
  }
  fptr->fd    = fd;

  DATA_PTR(io) = fptr;

  return io;
}

void
fptr_finalize(mrb_state *mrb, struct mrb_io *fptr, int noraise)
{
  int n = 0;

  if (fptr == NULL) {
    return;
  }

  if (fptr->fd > 2) {
    n = close(fptr->fd);
    if (n == 0) {
      fptr->fd = -1;
    }
  }
  if (fptr->fd2 > 2) {
    n = close(fptr->fd2);
    if (n == 0) {
      fptr->fd2 = -1;
    }
  }

  if (!noraise && n != 0) {
    mrb_sys_fail(mrb, "fptr_finalize failed.");
  }
}

mrb_value
mrb_io_bless(mrb_state *mrb, mrb_value io)
{
  if (mrb_type(io) != MRB_TT_DATA) {
    mrb_raise(mrb, E_TYPE_ERROR, "expected IO object");
    return mrb_nil_value();
  }

  DATA_TYPE(io) = &mrb_io_type;
  DATA_PTR(io)  = NULL;
  DATA_PTR(io)  = mrb_io_alloc(mrb);

  return io;
}

mrb_value
mrb_io_s_for_fd(mrb_state *mrb, mrb_value klass)
{
  mrb_value io = mrb_obj_value(mrb_data_object_alloc(mrb, mrb_class_ptr(klass), NULL, &mrb_io_type));

  return mrb_io_initialize(mrb, io);
}

mrb_value
mrb_io_s_sysopen(mrb_state *mrb, mrb_value klass)
{
  mrb_value path = mrb_nil_value();
  mrb_value mode = mrb_nil_value();
  mrb_int fd, flags, perm = -1;

  mrb_get_args(mrb, "S|Si", &path, &mode, &perm);
  if (mrb_nil_p(mode)) {
    mode = mrb_str_new_cstr(mrb, "r");
  }
  if (perm < 0) {
    perm = 0666;
  }

  flags = mrb_io_modestr_to_flags(mrb, mrb_string_value_cstr(mrb, &mode));
  fd = io_open(mrb, path, flags, perm);

  return mrb_fixnum_value(fd);
}

mrb_value
mrb_io_sysread(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  mrb_value buf = mrb_nil_value();
  int maxlen, ret;

  mrb_get_args(mrb, "i|S", &maxlen, &buf);
  if (maxlen < 0) {
    return mrb_nil_value();
  }

  if (mrb_nil_p(buf)) {
    buf = mrb_str_new(mrb, "", maxlen);
  }
  if (RSTRING_LEN(buf) != maxlen) {
    buf = mrb_str_resize(mrb, buf, maxlen);
  }

  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);
  ret = read(fptr->fd, RSTRING_PTR(buf), maxlen);
  switch (ret) {
    case 0: /* EOF */
      if (maxlen == 0) {
        buf = mrb_str_new_cstr(mrb, "");
      } else {
        mrb_raise(mrb, E_EOF_ERROR, "sysread failed: End of File");
        return mrb_nil_value();
      }
      break;
    case -1: /* Error */
      mrb_raise(mrb, E_IO_ERROR, "sysread failed");
      return mrb_nil_value();
      break;
    default:
      if (RSTRING_LEN(buf) != ret) {
        buf = mrb_str_resize(mrb, buf, ret);
      }
      break;
  }

  return buf;
}

mrb_value
mrb_io_sysseek(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  int pos, offset, whence = -1;

  mrb_get_args(mrb, "i|i", &offset, &whence);
  if (whence < 0) {
    whence = 0;
  }

  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);
  pos = lseek(fptr->fd, offset, whence);
  if (pos < 0) {
    mrb_raise(mrb, E_IO_ERROR, "sysseek faield");
    return mrb_nil_value();
  }

  return mrb_fixnum_value(pos);
}

mrb_value
mrb_io_syswrite(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  mrb_value str, buf;
  int length;

  mrb_get_args(mrb, "S", &str);
  if (mrb_type(str) != MRB_TT_STRING) {
    buf = mrb_funcall(mrb, str, "to_s", 0);
  } else {
    buf = str;
  }

  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);
  length = write(fptr->fd, RSTRING_PTR(buf), RSTRING_LEN(buf));

  return mrb_fixnum_value(length);
}

mrb_value
mrb_io_close(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);
  if (fptr && fptr->fd < 0) {
    mrb_raise(mrb, E_IO_ERROR, "closed stream.");
    return mrb_nil_value();
  }

  fptr_finalize(mrb, fptr, FALSE);
  return mrb_nil_value();
}

mrb_value
mrb_io_closed(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);

  if (fptr->fd >= 0) {
    return mrb_false_value();
  }

  return mrb_true_value();
}

mrb_value
mrb_io_pid(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);

  if (fptr->pid > 0) {
    return mrb_fixnum_value(fptr->pid);
  }

  return mrb_nil_value();
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
      fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(read)[i], &mrb_io_type);
      FD_SET(fptr->fd, rp);
      /* XXX: .....
      if (READ_DATA_PENDING(fptr->f)) {
        pending++;
        FD_SET(fileno(fptr->f), &pset);
      }
      */
      if (max < fptr->fd)
        max = fptr->fd;
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
      fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(write)[i], &mrb_io_type);
      FD_SET(fptr->fd, wp);
      if (max < fptr->fd)
        max = fptr->fd;
      if (fptr->fd2 >= 0) {
        FD_SET(fptr->fd2, wp);
        if (max < fptr->fd2)
          max = fptr->fd2;
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
      fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(except)[i], &mrb_io_type);
      FD_SET(fptr->fd, ep);
      if (max < fptr->fd)
        max = fptr->fd;
      if (fptr->fd2) {
        FD_SET(fptr->fd2, ep);
        if (max < fptr->fd2)
          max = fptr->fd2;
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
        fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(read)[i], &mrb_io_type);
        if (FD_ISSET(fptr->fd, rp) ||
            FD_ISSET(fptr->fd, &pset)) {
          mrb_ary_push(mrb, list, RARRAY_PTR(read)[i]);
        }
      }
    }

    if (wp) {
      list = RARRAY_PTR(result)[1];
      for (i = 0; i < RARRAY_LEN(write); i++) {
        fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(write)[i], &mrb_io_type);
        if (FD_ISSET(fptr->fd, wp)) {
          mrb_ary_push(mrb, list, RARRAY_PTR(write)[i]);
        } else if (fptr->fd2 && FD_ISSET(fptr->fd2, wp)) {
          mrb_ary_push(mrb, list, RARRAY_PTR(write)[i]);
        }
      }
    }

    if (ep) {
      list = RARRAY_PTR(result)[2];
      for (i = 0; i < RARRAY_LEN(except); i++) {
        fptr = (struct mrb_io *)mrb_get_datatype(mrb, RARRAY_PTR(except)[i], &mrb_io_type);
        if (FD_ISSET(fptr->fd, ep)) {
          mrb_ary_push(mrb, list, RARRAY_PTR(except)[i]);
        } else if (fptr->fd2 && FD_ISSET(fptr->fd2, wp)) {
          mrb_ary_push(mrb, list, RARRAY_PTR(except)[i]);
        }
      }
    }
  }

  return result;
}

mrb_value
mrb_io_fileno(mrb_state *mrb, mrb_value io)
{
  struct mrb_io *fptr;
  fptr = (struct mrb_io *)mrb_get_datatype(mrb, io, &mrb_io_type);
  return mrb_fixnum_value(fptr->fd);
}

void
mrb_init_io(mrb_state *mrb)
{
  struct RClass *io;

  io      = mrb_define_class(mrb, "IO", mrb->object_class);
  MRB_SET_INSTANCE_TT(io, MRB_TT_DATA);

  mrb_include_module(mrb, io, mrb_class_get(mrb, "Enumerable")); /* 15.2.20.3 */

  mrb_define_class_method(mrb, io, "_popen",  mrb_io_s_popen,   ARGS_ANY());
  mrb_define_class_method(mrb, io, "for_fd",  mrb_io_s_for_fd,  ARGS_REQ(1)|ARGS_OPT(2));
  mrb_define_class_method(mrb, io, "sysopen", mrb_io_s_sysopen, ARGS_ANY());

  mrb_define_method(mrb, io, "_bless",     mrb_io_bless,       ARGS_NONE());
  mrb_define_method(mrb, io, "initialize", mrb_io_initialize,  ARGS_ANY());    /* 15.2.20.5.21 (x)*/
  mrb_define_method(mrb, io, "sysread",    mrb_io_sysread,     ARGS_ANY());
  mrb_define_method(mrb, io, "sysseek",    mrb_io_sysseek,     ARGS_REQ(1));
  mrb_define_method(mrb, io, "syswrite",   mrb_io_syswrite,    ARGS_REQ(1));
  mrb_define_method(mrb, io, "close",      mrb_io_close,       ARGS_NONE());   /* 15.2.20.5.1 */
  mrb_define_method(mrb, io, "closed?",    mrb_io_closed,      ARGS_NONE());   /* 15.2.20.5.2 */
  mrb_define_method(mrb, io, "pid",        mrb_io_pid,         ARGS_NONE());   /* 15.2.20.5.2 */
  mrb_define_method(mrb, io, "fileno",     mrb_io_fileno,      ARGS_NONE());

  mrb_gv_set(mrb, mrb_intern(mrb, "$/"), mrb_str_new_cstr(mrb, "\n"));
}
