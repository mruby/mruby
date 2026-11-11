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
    char *path = mrb_locale_from_utf8(RSTRING_CSTR(mrb, mrb_io_path(mrb, obj)), -1);
    int ret;
    if (path == NULL) {
      errno = EINVAL;
      return -1;
    }
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

/* File.__atime(file_name), File.__mtime and File.__ctime: whole seconds
   since the Epoch, as File#atime and the others read them, which
   File.atime and the others turn into a Time */
static mrb_value
file_s_time(mrb_state *mrb, int which)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0) {
    mrb_sys_fail(mrb, mrb_string_p(obj) ? RSTRING_CSTR(mrb, obj) : NULL);
  }
  switch (which) {
    case 'a': return mrb_int_value(mrb, st.st_atime);
    case 'm': return mrb_int_value(mrb, st.st_mtime);
    default:  return mrb_int_value(mrb, st.st_ctime);
  }
}

static mrb_value
mrb_file_s_atime(mrb_state *mrb, mrb_value klass)
{
  return file_s_time(mrb, 'a');
}

static mrb_value
mrb_file_s_mtime(mrb_state *mrb, mrb_value klass)
{
  return file_s_time(mrb, 'm');
}

static mrb_value
mrb_file_s_ctime(mrb_state *mrb, mrb_value klass)
{
  return file_s_time(mrb, 'c');
}

/* Whether the process may access the named file in the way asked; false
   for a file that is not there */
static mrb_value
filetest_access(mrb_state *mrb, int mode)
{
  mrb_value obj = mrb_get_arg1(mrb);
  char *path = mrb_locale_from_utf8(RSTRING_CSTR(mrb, mrb_io_path(mrb, obj)), -1);
  int ret;

  if (path == NULL) return mrb_false_value();
  ret = mrb_hal_io_access(mrb, path, mode);
  mrb_locale_free(path);
  return mrb_bool_value(ret == 0);
}

/*
 * call-seq:
 *   File.readable?(file_name)   ->  true or false
 *   FileTest.readable?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is readable by the effective user and
 * group of this process.
 *
 *   File.readable?("/etc/passwd")   #=> true
 */

static mrb_value
mrb_filetest_s_readable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_R);
}

/*
 * call-seq:
 *   File.writable?(file_name)   ->  true or false
 *   FileTest.writable?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is writable by the effective user and
 * group of this process.
 *
 *   File.writable?("/etc/passwd")   #=> false
 */

static mrb_value
mrb_filetest_s_writable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_W);
}

/*
 * call-seq:
 *   File.executable?(file_name)   ->  true or false
 *   FileTest.executable?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is executable (or, for a directory,
 * searchable) by the effective user and group of this process. On
 * Windows a file is executable by its extension.
 *
 *   File.executable?("/bin/sh")   #=> true
 */

static mrb_value
mrb_filetest_s_executable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_X);
}

/* the access a _real? predicate asks: by the real IDs */
static mrb_value
mrb_filetest_s_readable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_R | MRB_IO_ACCESS_REAL);
}

/*
 * call-seq:
 *   File.readable_real?(file_name)     ->  true or false
 *   File.writable_real?(file_name)     ->  true or false
 *   File.executable_real?(file_name)   ->  true or false
 *
 * As File.readable? and the others, by the real user and group of this
 * process rather than the effective ones.
 */
static mrb_value
mrb_filetest_s_writable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_W | MRB_IO_ACCESS_REAL);
}

static mrb_value
mrb_filetest_s_executable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access(mrb, MRB_IO_ACCESS_X | MRB_IO_ACCESS_REAL);
}

/* whether the named file is there and `mode & mask` equals `bits` */
static mrb_value
filetest_mode(mrb_state *mrb, mrb_int mask, mrb_int bits)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0) return mrb_false_value();
  return mrb_bool_value((st.st_mode & mask) == bits);
}

/*
 * call-seq:
 *   File.blockdev?(file_name)   ->  true or false
 *   File.chardev?(file_name)    ->  true or false
 *
 * Returns `true` if the named file is a block device, or a character
 * device.
 *
 *   File.chardev?("/dev/null")   #=> true
 */
static mrb_value
mrb_filetest_s_blockdev_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_IFMT, MRB_IO_S_IFBLK);
}

static mrb_value
mrb_filetest_s_chardev_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_IFMT, MRB_IO_S_IFCHR);
}

/*
 * call-seq:
 *   File.setuid?(file_name)   ->  true or false
 *   File.setgid?(file_name)   ->  true or false
 *   File.sticky?(file_name)   ->  true or false
 *
 * Returns `true` if the named file has the setuid, setgid or sticky bit
 * set.
 *
 *   File.sticky?("/tmp")   #=> true
 */
static mrb_value
mrb_filetest_s_setuid_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISUID, MRB_IO_S_ISUID);
}

static mrb_value
mrb_filetest_s_setgid_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISGID, MRB_IO_S_ISGID);
}

static mrb_value
mrb_filetest_s_sticky_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISVTX, MRB_IO_S_ISVTX);
}

/* the permission bits of the named file when `bit` is among them, else
   nil, as File.world_readable? and File.world_writable? answer */
static mrb_value
filetest_world(mrb_state *mrb, mrb_int bit)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0 || !(st.st_mode & bit)) return mrb_nil_value();
  return mrb_int_value(mrb, st.st_mode & 0777);
}

/*
 * call-seq:
 *   File.world_readable?(file_name)   ->  integer or nil
 *   File.world_writable?(file_name)   ->  integer or nil
 *
 * If the named file is readable (writable) by others, returns its
 * permission bits, and nil otherwise.
 *
 *   File.world_readable?("/etc/passwd")   #=> 420
 */
static mrb_value
mrb_filetest_s_world_readable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_world(mrb, MRB_IO_S_IROTH);
}

static mrb_value
mrb_filetest_s_world_writable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_world(mrb, MRB_IO_S_IWOTH);
}

#ifdef MRB_HAL_IO_HAS_OWNER
static mrb_value
filetest_owner(mrb_state *mrb, int which)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (mrb_stat(mrb, obj, &st) < 0) return mrb_false_value();
  return mrb_bool_value((mrb_hal_io_owner(mrb, &st) & which) != 0);
}

/*
 * call-seq:
 *   File.owned?(file_name)      ->  true or false
 *   File.grpowned?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is owned by the effective user of this
 * process, or its group is the effective group or one of its supplementary
 * groups.
 */
static mrb_value
mrb_filetest_s_owned_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_owner(mrb, MRB_IO_OWNER_USER);
}

static mrb_value
mrb_filetest_s_grpowned_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_owner(mrb, MRB_IO_OWNER_GROUP);
}
#endif

#ifdef MRB_HAL_IO_HAS_STAT_INODE
/*
 * call-seq:
 *   File.identical?(file_1, file_2)   ->  true or false
 *
 * Returns `true` if the two name the same file, as a link or a symbolic
 * link does, and both are there.
 */
static mrb_value
mrb_filetest_s_identical_p(mrb_state *mrb, mrb_value klass)
{
  mrb_value a, b;
  mrb_io_stat sa, sb;

  mrb_get_args(mrb, "oo", &a, &b);
  if (mrb_stat(mrb, a, &sa) < 0 || mrb_stat(mrb, b, &sb) < 0) return mrb_false_value();
  return mrb_bool_value(sa.st_dev == sb.st_dev && sa.st_ino == sb.st_ino);
}
#endif

/*
 * call-seq:
 *   File.ftype(file_name)   ->  string
 *
 * Returns the type of the named file, without following a symbolic link:
 * "file", "directory", "characterSpecial", "blockSpecial", "fifo", "link",
 * "socket" or "unknown". Raises a SystemCallError when it is not there.
 *
 *   File.ftype("/dev/null")   #=> "characterSpecial"
 */
static mrb_value
mrb_file_s_ftype(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);
  const char *t;

#ifdef MRB_HAL_IO_HAS_STAT_SYMLINK
  if (mrb_lstat(mrb, obj, &st) < 0)
#else
  if (mrb_stat(mrb, obj, &st) < 0)
#endif
  {
    mrb_sys_fail(mrb, RSTRING_CSTR(mrb, mrb_io_path(mrb, obj)));
  }
  switch (st.st_mode & MRB_IO_S_IFMT) {
    case MRB_IO_S_IFREG:  t = "file"; break;
    case MRB_IO_S_IFDIR:  t = "directory"; break;
    case MRB_IO_S_IFCHR:  t = "characterSpecial"; break;
    case MRB_IO_S_IFBLK:  t = "blockSpecial"; break;
    case MRB_IO_S_IFIFO:  t = "fifo"; break;
    case MRB_IO_S_IFLNK:  t = "link"; break;
    case MRB_IO_S_IFSOCK: t = "socket"; break;
    default:              t = "unknown"; break;
  }
  return mrb_str_new_cstr(mrb, t);
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
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(readable),   mrb_filetest_s_readable_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(writable),   mrb_filetest_s_writable_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(executable), mrb_filetest_s_executable_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(blockdev), mrb_filetest_s_blockdev_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(chardev), mrb_filetest_s_chardev_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(empty), mrb_filetest_s_zero_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(readable_real), mrb_filetest_s_readable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(writable_real), mrb_filetest_s_writable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(executable_real), mrb_filetest_s_executable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(setuid), mrb_filetest_s_setuid_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(setgid), mrb_filetest_s_setgid_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(sticky), mrb_filetest_s_sticky_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(world_readable), mrb_filetest_s_world_readable_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(world_writable), mrb_filetest_s_world_writable_p, MRB_ARGS_REQ(1));
#ifdef MRB_HAL_IO_HAS_OWNER
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(owned), mrb_filetest_s_owned_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(grpowned), mrb_filetest_s_grpowned_p, MRB_ARGS_REQ(1));
#endif
#ifdef MRB_HAL_IO_HAS_STAT_INODE
  mrb_define_class_method_id(mrb, f, MRB_SYM_Q(identical), mrb_filetest_s_identical_p, MRB_ARGS_REQ(2));
#endif

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
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(readable),   mrb_filetest_s_readable_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(writable),   mrb_filetest_s_writable_p,   MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(executable), mrb_filetest_s_executable_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(blockdev), mrb_filetest_s_blockdev_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(chardev), mrb_filetest_s_chardev_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(empty), mrb_filetest_s_zero_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(readable_real), mrb_filetest_s_readable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(writable_real), mrb_filetest_s_writable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(executable_real), mrb_filetest_s_executable_real_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(setuid), mrb_filetest_s_setuid_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(setgid), mrb_filetest_s_setgid_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(sticky), mrb_filetest_s_sticky_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(world_readable), mrb_filetest_s_world_readable_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(world_writable), mrb_filetest_s_world_writable_p, MRB_ARGS_REQ(1));
#ifdef MRB_HAL_IO_HAS_OWNER
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(owned), mrb_filetest_s_owned_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(grpowned), mrb_filetest_s_grpowned_p, MRB_ARGS_REQ(1));
#endif
#ifdef MRB_HAL_IO_HAS_STAT_INODE
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(identical), mrb_filetest_s_identical_p, MRB_ARGS_REQ(2));
#endif
  mrb_define_class_method_id(mrb, file, MRB_SYM(ftype), mrb_file_s_ftype, MRB_ARGS_REQ(1));

  mrb_define_class_method_id(mrb, file, MRB_SYM(__atime),     mrb_file_s_atime,           MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(__mtime),     mrb_file_s_mtime,           MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(__ctime),     mrb_file_s_ctime,           MRB_ARGS_REQ(1));
}
