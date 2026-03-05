/*
** dir.c - Dir
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/string.h>
#include <mruby/presym.h>
#include "dir_hal.h"

#include <string.h>
#include <errno.h>

#define E_IO_ERROR mrb_exc_get_id(mrb, MRB_SYM(IOError))

struct mrb_dir {
  mrb_dir_handle *handle;
};

static void
mrb_dir_free(mrb_state *mrb, void *ptr)
{
  struct mrb_dir *mdir = (struct mrb_dir*)ptr;

  if (mdir->handle) {
    mrb_hal_dir_close(mrb, mdir->handle);
    mdir->handle = NULL;
  }
  mrb_free(mrb, mdir);
}

static struct mrb_data_type mrb_dir_type = { "DIR", mrb_dir_free };

/*
 * call-seq:
 *   dir.close -> nil
 *
 * Closes the directory stream. Any further attempts to access dir will
 * raise an IOError.
 *
 *   d = Dir.new("testdir")
 *   d.close   #=> nil
 */
static mrb_value
mrb_dir_close(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  mdir = (struct mrb_dir*)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->handle) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  if (mrb_hal_dir_close(mrb, mdir->handle) == -1) {
    mrb_sys_fail(mrb, "closedir");
  }
  mdir->handle = NULL;
  return mrb_nil_value();
}

/*
 * call-seq:
 *   Dir.new(string) -> aDir
 *
 * Returns a new directory object for the named directory.
 *
 *   d = Dir.new("testdir")
 */
static mrb_value
mrb_dir_init(mrb_state *mrb, mrb_value self)
{
  mrb_dir_handle *handle;
  struct mrb_dir *mdir;
  const char *path;

  mdir = (struct mrb_dir*)DATA_PTR(self);
  if (mdir) {
    mrb_dir_free(mrb, mdir);
  }
  DATA_TYPE(self) = &mrb_dir_type;
  DATA_PTR(self) = NULL;

  mdir = (struct mrb_dir*)mrb_malloc(mrb, sizeof(*mdir));
  mdir->handle = NULL;
  DATA_PTR(self) = mdir;

  mrb_get_args(mrb, "z", &path);
  if ((handle = mrb_hal_dir_open(mrb, path)) == NULL) {
    mrb_sys_fail(mrb, path);
  }
  mdir->handle = handle;
  return self;
}

/*
 * call-seq:
 *   Dir.delete(string) -> 0
 *
 * Deletes the named directory. Raises a subclass of SystemCallError if the
 * directory isn't empty.
 *
 *   Dir.delete("testdir")
 */
static mrb_value
mrb_dir_delete(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);
  if (mrb_hal_dir_rmdir(mrb, path) == -1) {
    mrb_sys_fail(mrb, path);
  }
  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   Dir.exist?(file_name) -> true or false
 *
 * Returns true if the named file is a directory, false otherwise.
 *
 *   Dir.exist?(".")   #=> true
 *   Dir.exist?("foo") #=> false
 */
static mrb_value
mrb_dir_existp(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);
  if (mrb_hal_dir_is_directory(mrb, path)) {
    return mrb_true_value();
  }
  else {
    return mrb_false_value();
  }
}

/*
 * call-seq:
 *   Dir.getwd -> string
 *   Dir.pwd   -> string
 *
 * Returns the path to the current working directory of this process as a string.
 *
 *   Dir.getwd   #=> "/usr/local"
 */
static mrb_value
mrb_dir_getwd(mrb_state *mrb, mrb_value klass)
{
  mrb_int size = 64;

  mrb_value path = mrb_str_buf_new(mrb, size);
  while (mrb_hal_dir_getcwd(mrb, RSTRING_PTR(path), (size_t)size) == -1) {
    int e = errno;
    if (e != ERANGE) {
      mrb_sys_fail(mrb, "getcwd(2)");
    }
    size *= 2;
    mrb_str_resize(mrb, path, size);
  }
  mrb_str_resize(mrb, path, strlen(RSTRING_PTR(path)));
  return path;
}

/*
 * call-seq:
 *   Dir.mkdir(string [, integer]) -> 0
 *
 * Makes a new directory named by string, with permissions specified by the
 * optional parameter integer. The permissions may be modified by the value
 * of File.umask, and are ignored on NT. Raises a SystemCallError if the
 * directory cannot be created.
 *
 *   Dir.mkdir("testdir")         #=> 0
 *   Dir.mkdir("testdir", 0755)   #=> 0
 */
static mrb_value
mrb_dir_mkdir(mrb_state *mrb, mrb_value klass)
{
  mrb_int mode;
  const char *path;

  mode = 0777;
  mrb_get_args(mrb, "z|i", &path, &mode);
  if (mrb_hal_dir_mkdir(mrb, path, (int)mode) == -1) {
    mrb_sys_fail(mrb, path);
  }
  return mrb_fixnum_value(0);
}

/* Helper for Dir.chdir - internal method to change directory */
static mrb_value
mrb_dir_chdir(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);
  if (mrb_hal_dir_chdir(mrb, path) == -1) {
    mrb_sys_fail(mrb, path);
  }
  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   Dir.chroot(string) -> 0
 *
 * Changes this process's idea of the file system root. Only a privileged
 * process may make this call. Not available on all platforms.
 *
 *   Dir.chroot("/production/secure/root")
 */
static mrb_value
mrb_dir_chroot(mrb_state *mrb, mrb_value self)
{
  const char *path;
  int res;

  mrb_get_args(mrb, "z", &path);
  res = mrb_hal_dir_chroot(mrb, path);
  if (res == -1) {
    if (errno == ENOSYS) {
      mrb_raise(mrb, E_NOTIMP_ERROR, "chroot() unreliable on your system");
    }
    mrb_sys_fail(mrb, path);
  }

  return mrb_fixnum_value(res);
}

static mrb_bool
skip_name_p(const char *name)
{
  if (name[0] != '.') return FALSE;
  if (name[1] == '\0') return TRUE;
  if (name[1] != '.') return FALSE;
  if (name[2] == '\0') return TRUE;
  return FALSE;
}

/*
 * call-seq:
 *   Dir.empty?(path_name) -> true or false
 *
 * Returns true if the named directory is empty, false otherwise.
 *
 *   Dir.empty?(".")   #=> false
 *   Dir.empty?("/tmp") #=> false
 */
static mrb_value
mrb_dir_empty(mrb_state *mrb, mrb_value self)
{
  mrb_dir_handle *handle;
  const char *name;
  const char *path;
  mrb_value result = mrb_true_value();

  mrb_get_args(mrb, "z", &path);
  if ((handle = mrb_hal_dir_open(mrb, path)) == NULL) {
    mrb_sys_fail(mrb, path);
  }
  while ((name = mrb_hal_dir_read(mrb, handle)) != NULL) {
    if (!skip_name_p(name)) {
      result = mrb_false_value();
      break;
    }
  }
  mrb_hal_dir_close(mrb, handle);
  return result;
}

/*
 * call-seq:
 *   dir.read -> string or nil
 *
 * Reads the next entry from dir and returns it as a string. Returns nil
 * at the end of the stream.
 *
 *   d = Dir.new("testdir")
 *   d.read   #=> "."
 *   d.read   #=> ".."
 *   d.read   #=> "config.h"
 */
static mrb_value
mrb_dir_read(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  const char *name;

  mdir = (struct mrb_dir*)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->handle) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  name = mrb_hal_dir_read(mrb, mdir->handle);
  if (name != NULL) {
    return mrb_str_new_cstr(mrb, name);
  }
  else {
    return mrb_nil_value();
  }
}

/*
 * call-seq:
 *   dir.rewind -> dir
 *
 * Repositions dir to the beginning of the stream.
 *
 *   d = Dir.new("testdir")
 *   d.read     #=> "."
 *   d.rewind   #=> #<Dir:testdir>
 *   d.read     #=> "."
 */
static mrb_value
mrb_dir_rewind(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;

  mdir = (struct mrb_dir*)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->handle) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  mrb_hal_dir_rewind(mrb, mdir->handle);
  return self;
}

/*
 * call-seq:
 *   dir.seek(integer) -> dir
 *
 * Seeks to a particular location in dir. integer must be a value returned
 * by Dir#tell.
 *
 *   d = Dir.new("testdir")
 *   pos = d.tell      #=> 0
 *   d.read            #=> "."
 *   d.seek(pos)       #=> #<Dir:testdir>
 *   d.read            #=> "."
 */
static mrb_value
mrb_dir_seek(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  mrb_int pos;

  mdir = (struct mrb_dir*)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->handle) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  mrb_get_args(mrb, "i", &pos);
  if (mrb_hal_dir_seek(mrb, mdir->handle, (long)pos) == -1) {
    if (errno == ENOSYS) {
      mrb_raise(mrb, E_NOTIMP_ERROR, "dirseek() unreliable on your system");
    }
  }
  return self;
}

/*
 * call-seq:
 *   dir.tell -> integer
 *   dir.pos  -> integer
 *
 * Returns the current position in dir.
 *
 *   d = Dir.new("testdir")
 *   d.tell   #=> 0
 *   d.read   #=> "."
 *   d.tell   #=> 1
 */
static mrb_value
mrb_dir_tell(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  long pos;

  mdir = (struct mrb_dir*)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->handle) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  pos = mrb_hal_dir_tell(mrb, mdir->handle);
  if (pos == -1) {
    if (errno == ENOSYS) {
      mrb_raise(mrb, E_NOTIMP_ERROR, "dirtell() unreliable on your system");
    }
  }
  return mrb_fixnum_value((mrb_int)pos);
}

/*
 * call-seq:
 *   Dir.entries(dirname) -> array
 *
 * Returns an array containing all of the filenames in the given directory.
 * Will raise a SystemCallError if the named directory doesn't exist.
 */
static mrb_value
mrb_dir_entries(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);

  mrb_dir_handle *handle = mrb_hal_dir_open(mrb, path);
  if (handle == NULL) {
    mrb_sys_fail(mrb, path);
  }

  mrb_value ary = mrb_ary_new(mrb);
  const char *name;
  while ((name = mrb_hal_dir_read(mrb, handle)) != NULL) {
    mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, name));
  }

  mrb_hal_dir_close(mrb, handle);
  return ary;
}

/*
 * call-seq:
 *   Dir.children(dirname) -> array
 *
 * Returns an array containing all of the filenames except for "." and ".."
 * in the given directory. Will raise a SystemCallError if the named
 * directory doesn't exist.
 */
static mrb_value
mrb_dir_children(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);

  mrb_dir_handle *handle = mrb_hal_dir_open(mrb, path);
  if (handle == NULL) {
    mrb_sys_fail(mrb, path);
  }

  mrb_value ary = mrb_ary_new(mrb);
  const char *name;
  while ((name = mrb_hal_dir_read(mrb, handle)) != NULL) {
    if (!skip_name_p(name)) {
      mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, name));
    }
  }

  mrb_hal_dir_close(mrb, handle);
  return ary;
}

void
mrb_mruby_dir_gem_init(mrb_state *mrb)
{
  struct RClass *d;

  mrb_hal_dir_init(mrb);

  d = mrb_define_class_id(mrb, MRB_SYM(Dir), mrb->object_class);
  MRB_SET_INSTANCE_TT(d, MRB_TT_DATA);
  mrb_define_class_method_id(mrb, d, MRB_SYM(delete),  mrb_dir_delete, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM_Q(exist), mrb_dir_existp, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM(getwd),   mrb_dir_getwd,  MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, d, MRB_SYM(mkdir),   mrb_dir_mkdir,  MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM(_chdir),  mrb_dir_chdir,  MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM(chroot),  mrb_dir_chroot, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM_Q(empty), mrb_dir_empty, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM(entries), mrb_dir_entries, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, d, MRB_SYM(children), mrb_dir_children, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, d, MRB_SYM(close),      mrb_dir_close,  MRB_ARGS_NONE());
  mrb_define_method_id(mrb, d, MRB_SYM(initialize), mrb_dir_init,   MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, d, MRB_SYM(read),       mrb_dir_read,   MRB_ARGS_NONE());
  mrb_define_method_id(mrb, d, MRB_SYM(rewind),     mrb_dir_rewind, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, d, MRB_SYM(seek),       mrb_dir_seek,   MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, d, MRB_SYM(tell),       mrb_dir_tell,   MRB_ARGS_NONE());

  mrb_define_class_id(mrb, MRB_SYM(IOError), E_STANDARD_ERROR);
}

void
mrb_mruby_dir_gem_final(mrb_state *mrb)
{
  mrb_hal_dir_final(mrb);
}
