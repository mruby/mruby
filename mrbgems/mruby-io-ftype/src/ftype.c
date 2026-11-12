/*
** ftype.c - File.ftype and the FileTest predicates mruby-io leaves out
**
** Written by Akira Matsuda (mruby/mruby#7817).
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/error.h>
#include <mruby/io.h>
#include "io_hal.h"

#include <errno.h>
#include <stdlib.h>

/* the status of the file an IO or a path argument names; with do_lstat,
   a symbolic link's own */
static int
ftype_stat(mrb_state *mrb, mrb_value obj, mrb_io_stat *st, mrb_bool do_lstat)
{
  if (mrb_obj_is_kind_of(mrb, obj, mrb_class_get_id(mrb, MRB_SYM(IO)))) {
    return mrb_hal_io_fstat(mrb, mrb_io_fileno(mrb, obj), st);
  }

  char *path = mrb_locale_from_utf8(RSTRING_CSTR(mrb, mrb_io_path(mrb, obj)), -1);
  int ret;

  if (path == NULL) {
    errno = EINVAL;
    return -1;
  }
  ret = do_lstat ? mrb_hal_io_lstat(mrb, path, st) : mrb_hal_io_stat(mrb, path, st);
  mrb_locale_free(path);
  return ret;
}

/* whether the named file is there and `mode & mask` equals `bits` */
static mrb_value
filetest_mode(mrb_state *mrb, mrb_int mask, mrb_int bits)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (ftype_stat(mrb, obj, &st, FALSE) < 0) return mrb_false_value();
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
filetest_s_blockdev_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_IFMT, MRB_IO_S_IFBLK);
}

static mrb_value
filetest_s_chardev_p(mrb_state *mrb, mrb_value klass)
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
filetest_s_setuid_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISUID, MRB_IO_S_ISUID);
}

static mrb_value
filetest_s_setgid_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISGID, MRB_IO_S_ISGID);
}

static mrb_value
filetest_s_sticky_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_mode(mrb, MRB_IO_S_ISVTX, MRB_IO_S_ISVTX);
}

/*
 * call-seq:
 *   File.empty?(file_name)   ->  true or false
 *
 * Returns `true` if the named file is there and has no bytes, as
 * File.zero? does.
 */
static mrb_value
filetest_s_empty_p(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (ftype_stat(mrb, obj, &st, FALSE) < 0) return mrb_false_value();
  return mrb_bool_value(st.st_size == 0);
}

/* the access a _real? predicate asks: by the real IDs */
static mrb_value
filetest_access_real(mrb_state *mrb, int mode)
{
  mrb_value obj = mrb_get_arg1(mrb);
  char *path = mrb_locale_from_utf8(RSTRING_CSTR(mrb, mrb_io_path(mrb, obj)), -1);
  int ret;

  if (path == NULL) return mrb_false_value();
  ret = mrb_hal_io_access(mrb, path, mode | MRB_IO_ACCESS_REAL);
  mrb_locale_free(path);
  return mrb_bool_value(ret == 0);
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
filetest_s_readable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access_real(mrb, MRB_IO_ACCESS_R);
}

static mrb_value
filetest_s_writable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access_real(mrb, MRB_IO_ACCESS_W);
}

static mrb_value
filetest_s_executable_real_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_access_real(mrb, MRB_IO_ACCESS_X);
}

/* the permission bits of the named file when `bit` is among them, else
   nil, as File.world_readable? and File.world_writable? answer */
static mrb_value
filetest_world(mrb_state *mrb, mrb_int bit)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (ftype_stat(mrb, obj, &st, FALSE) < 0 || !(st.st_mode & bit)) return mrb_nil_value();
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
filetest_s_world_readable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_world(mrb, MRB_IO_S_IROTH);
}

static mrb_value
filetest_s_world_writable_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_world(mrb, MRB_IO_S_IWOTH);
}

#ifdef MRB_HAL_IO_HAS_OWNER
static mrb_value
filetest_owner(mrb_state *mrb, int which)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);

  if (ftype_stat(mrb, obj, &st, FALSE) < 0) return mrb_false_value();
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
filetest_s_owned_p(mrb_state *mrb, mrb_value klass)
{
  return filetest_owner(mrb, MRB_IO_OWNER_USER);
}

static mrb_value
filetest_s_grpowned_p(mrb_state *mrb, mrb_value klass)
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
filetest_s_identical_p(mrb_state *mrb, mrb_value klass)
{
  mrb_value a, b;
  mrb_io_stat sa, sb;

  mrb_get_args(mrb, "oo", &a, &b);
  if (ftype_stat(mrb, a, &sa, FALSE) < 0 || ftype_stat(mrb, b, &sb, FALSE) < 0) {
    return mrb_false_value();
  }
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
file_s_ftype(mrb_state *mrb, mrb_value klass)
{
  mrb_io_stat st;
  mrb_value obj = mrb_get_arg1(mrb);
  const char *t;

#ifdef MRB_HAL_IO_HAS_STAT_SYMLINK
  if (ftype_stat(mrb, obj, &st, TRUE) < 0)
#else
  if (ftype_stat(mrb, obj, &st, FALSE) < 0)
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

/* the predicates, which FileTest and File both answer */
static const mrb_mt_entry filetest_rom_entries[] = {
  MRB_MT_ENTRY(filetest_s_blockdev_p,        MRB_SYM_Q(blockdev),        MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_chardev_p,         MRB_SYM_Q(chardev),         MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_empty_p,           MRB_SYM_Q(empty),           MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_readable_real_p,   MRB_SYM_Q(readable_real),   MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_writable_real_p,   MRB_SYM_Q(writable_real),   MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_executable_real_p, MRB_SYM_Q(executable_real), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_setuid_p,          MRB_SYM_Q(setuid),          MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_setgid_p,          MRB_SYM_Q(setgid),          MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_sticky_p,          MRB_SYM_Q(sticky),          MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_world_readable_p,  MRB_SYM_Q(world_readable),  MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_world_writable_p,  MRB_SYM_Q(world_writable),  MRB_ARGS_REQ(1)),
#ifdef MRB_HAL_IO_HAS_OWNER
  MRB_MT_ENTRY(filetest_s_owned_p,           MRB_SYM_Q(owned),           MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(filetest_s_grpowned_p,        MRB_SYM_Q(grpowned),        MRB_ARGS_REQ(1)),
#endif
#ifdef MRB_HAL_IO_HAS_STAT_INODE
  MRB_MT_ENTRY(filetest_s_identical_p,       MRB_SYM_Q(identical),       MRB_ARGS_REQ(2)),
#endif
};

void
mrb_mruby_io_ftype_gem_init(mrb_state *mrb)
{
  struct RClass *ft = mrb_module_get_id(mrb, MRB_SYM(FileTest));
  struct RClass *file = mrb_class_get_id(mrb, MRB_SYM(File));

  MRB_MT_INIT_ROM(mrb, mrb_singleton_class_ptr(mrb, mrb_obj_value(ft)), filetest_rom_entries);
  MRB_MT_INIT_ROM(mrb, mrb_singleton_class_ptr(mrb, mrb_obj_value(file)), filetest_rom_entries);
  mrb_define_class_method_id(mrb, file, MRB_SYM(ftype), file_s_ftype, MRB_ARGS_REQ(1));
}

void
mrb_mruby_io_ftype_gem_final(mrb_state *mrb)
{
}
