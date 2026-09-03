/*
** file_test.c - FileTest class
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>
#include <mruby/io.h>
#include <mruby/error.h>
#include <mruby/internal.h>
#include "io_hal.h"

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

extern struct mrb_data_type mrb_io_type;

static int
mrb_stat0(mrb_state *mrb, mrb_value obj, mrb_io_stat *st, int do_lstat)
{
  if (mrb_obj_is_kind_of(mrb, obj, mrb_class_get_id(mrb, MRB_SYM(IO)))) {
    struct mrb_io *fptr;
    fptr = (struct mrb_io*)mrb_data_get_ptr(mrb, obj, &mrb_io_type);

    if (fptr && fptr->fd >= 0) {
      return mrb_hal_io_fstat(mrb, fptr->fd, st);
    }

    mrb_raise(mrb, E_IO_ERROR, "closed stream");
    return -1;
  }
  else {
    char *path = mrb_locale_from_utf8(RSTRING_CSTR(mrb, obj), -1);
    int ret;
    if (do_lstat) {
      ret = mrb_hal_io_lstat(mrb, path, st);
    }
    else {
      ret = mrb_hal_io_stat(mrb, path, st);
    }
    mrb_locale_free(path);
    return ret;
  }
}

static int
mrb_stat(mrb_state *mrb, mrb_value obj, mrb_io_stat *st)
{
  return mrb_stat0(mrb, obj, st, 0);
}

#ifdef MRB_HAL_IO_HAS_STAT_SYMLINK
static int
mrb_lstat(mrb_state *mrb, mrb_value obj, mrb_io_stat *st)
{
  return mrb_stat0(mrb, obj, st, 1);
}
#endif

/*
 * call-seq:
 *   File.directory?(file_name)   ->  true or false
 *   FileTest.directory?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is a directory, or a symlink that points at a directory, and `false`
 * otherwise.
 *
 *    File.directory?(".")   #=> true
 *    FileTest.directory?(".")   #=> true
 */

static mrb_value
mrb_filetest_s_directory_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();
  if (MRB_IO_S_ISDIR(st.st_mode))
    return mrb_true_value();

  return mrb_false_value();
}

/* Whether a stat can name a FIFO, a symbolic link or a socket is the port's
   to say, in its io_hal_features.h; a predicate the port cannot answer is
   unimplemented, and named as such so `respond_to?` can answer false */

#ifdef MRB_HAL_IO_HAS_STAT_FIFO
/*
 * call-seq:
 *   File.pipe?(file_name)   ->  true or false
 *   FileTest.pipe?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is a pipe.
 *
 *   File.pipe?("/dev/stdin")   #=> true
 *   FileTest.pipe?("/dev/stdin")   #=> true
 */

static mrb_value
mrb_filetest_s_pipe_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();
  if (MRB_IO_S_ISFIFO(st.st_mode))
    return mrb_true_value();

  return mrb_false_value();
}
#else
# define mrb_filetest_s_pipe_p mrb_notimplement_m
#endif

#ifdef MRB_HAL_IO_HAS_STAT_SYMLINK
/*
 * call-seq:
 *   File.symlink?(file_name)   ->  true or false
 *   FileTest.symlink?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is a symbolic link.
 *
 *   File.symlink?("link-to-test")   #=> true
 *   FileTest.symlink?("link-to-test")   #=> true
 */

static mrb_value
mrb_filetest_s_symlink_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_lstat(mrb, obj, &st) == -1)
    return mrb_false_value();
  if (MRB_IO_S_ISLNK(st.st_mode))
    return mrb_true_value();

  return mrb_false_value();
}
#else
# define mrb_filetest_s_symlink_p mrb_notimplement_m
#endif

#ifdef MRB_HAL_IO_HAS_STAT_SOCKET
/*
 * call-seq:
 *   File.socket?(file_name)   ->  true or false
 *   FileTest.socket?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is a socket.
 *
 *   File.socket?("/tmp/.X11-unix/X0")   #=> true
 *   FileTest.socket?("/tmp/.X11-unix/X0")   #=> true
 */

static mrb_value
mrb_filetest_s_socket_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();
  if (MRB_IO_S_ISSOCK(st.st_mode))
    return mrb_true_value();

  return mrb_false_value();
}
#else
# define mrb_filetest_s_socket_p mrb_notimplement_m
#endif

/*
 * call-seq:
 *    File.exist?(file_name)    ->  true or false
 *    File.exists?(file_name)   ->  true or false
 *    FileTest.exist?(file_name)    ->  true or false
 *    FileTest.exists?(file_name)   ->  true or false
 *
 * Returns `true` if the named file exists.
 *
 *   File.exist?("config.h")      #=> true
 *   File.exist?("no_such_file")  #=> false
 *   FileTest.exist?("config.h")      #=> true
 *   FileTest.exist?("no_such_file")  #=> false
 */

static mrb_value
mrb_filetest_s_exist_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();

  return mrb_true_value();
}

/*
 * call-seq:
 *    File.file?(file_name)   -> true or false
 *    FileTest.file?(file_name)   -> true or false
 *
 * Returns `true` if the named file exists and is a regular file.
 *
 *   File.file?("testfile")   #=> true
 *   FileTest.file?("testfile")   #=> true
 */

static mrb_value
mrb_filetest_s_file_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();
  if (MRB_IO_S_ISREG(st.st_mode))
    return mrb_true_value();

  return mrb_false_value();
}

/*
 * call-seq:
 *    File.zero?(file_name)   -> true or false
 *    FileTest.zero?(file_name)   -> true or false
 *
 * Returns `true` if the named file exists and has a zero size.
 *
 *   File.zero?("testfile")   #=> false
 *   FileTest.zero?("testfile")   #=> false
 */

static mrb_value
mrb_filetest_s_zero_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_false_value();
  if (st.st_size == 0)
    return mrb_true_value();

  return mrb_false_value();
}

/*
 * call-seq:
 *    File.size(file_name)   -> integer
 *    FileTest.size(file_name)   -> integer
 *
 * Returns the size of `file_name`.
 *
 * `file_name` can be an IO object.
 *
 *   File.size("testfile")   #=> 66
 *   FileTest.size("testfile")   #=> 66
 */

static mrb_value
mrb_filetest_s_size(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    mrb_sys_fail(mrb, "mrb_stat");

  return mrb_int_value(mrb, st.st_size);
}

/*
 * call-seq:
 *    File.size?(file_name)   -> Integer or nil
 *    FileTest.size?(file_name)   -> Integer or nil
 *
 * Returns `nil` if `file_name` doesn't exist or has zero size, the size of the
 * file otherwise.
 *
 *   File.size?("testfile")   #=> 66
 *   FileTest.size?("testfile")   #=> 66
 */

static mrb_value
mrb_filetest_s_size_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0)
    return mrb_nil_value();
  if (st.st_size == 0)
    return mrb_nil_value();

  return mrb_int_value(mrb, st.st_size);
}

void
mrb_init_file_test(mrb_state *mrb)
{
  struct RClass *f;

  f = mrb_define_module_id(mrb, MRB_SYM(FileTest));

  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(directory), mrb_filetest_s_directory_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(exist),     mrb_filetest_s_exist_p,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(exists),    mrb_filetest_s_exist_p,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(file),      mrb_filetest_s_file_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(pipe),      mrb_filetest_s_pipe_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM(size),        mrb_filetest_s_size,        MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(size),      mrb_filetest_s_size_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(socket),    mrb_filetest_s_socket_p,    MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(symlink),   mrb_filetest_s_symlink_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(zero),      mrb_filetest_s_zero_p,      MRB_ARGS_REQ(1));

  // Also register the same methods on File class
  struct RClass *file = mrb_class_get_id(mrb, MRB_SYM(File));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(directory), mrb_filetest_s_directory_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(exist),     mrb_filetest_s_exist_p,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(exists),    mrb_filetest_s_exist_p,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(file),      mrb_filetest_s_file_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(pipe),      mrb_filetest_s_pipe_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(size),        mrb_filetest_s_size,        MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(size),      mrb_filetest_s_size_p,      MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(socket),    mrb_filetest_s_socket_p,    MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(symlink),   mrb_filetest_s_symlink_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(zero),      mrb_filetest_s_zero_p,      MRB_ARGS_REQ(1));
}
