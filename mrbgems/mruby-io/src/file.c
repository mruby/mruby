/*
** file.c - File class
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>
#include <mruby/ext/io.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include "io_hal.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* Undefine system macros that conflict with mrb_io_stat field names */
#ifdef st_atime
#undef st_atime
#endif
#ifdef st_mtime
#undef st_mtime
#endif
#ifdef st_ctime
#undef st_ctime
#endif

#if defined(_WIN32)
  #include <windows.h>
  #include <io.h>
  #define NULL_FILE "NUL"
  #define UNLINK _unlink
  #define GETCWD _getcwd
  #define CHMOD(a, b) 0
  #define MAXPATHLEN 1024
 #if !defined(PATH_MAX)
  #define PATH_MAX _MAX_PATH
 #endif
  #define realpath(N,R) _fullpath((R),(N),_MAX_PATH)
  #include <direct.h>
#else
  #define NULL_FILE "/dev/null"
  #include <unistd.h>
  #define UNLINK unlink
  #define GETCWD getcwd
  #define CHMOD(a, b) chmod(a,b)
  #include <sys/file.h>
#ifndef __DJGPP__
  #include <libgen.h>
#endif
  #include <sys/param.h>
  #include <pwd.h>
#endif

#define FILE_SEPARATOR "/"

#if defined(_WIN32)
  #define PATH_SEPARATOR ";"
  #define FILE_ALT_SEPARATOR "\\"
  #define VOLUME_SEPARATOR ":"
  #define DIRSEP_P(ch) (((ch) == '/') | ((ch) == '\\'))
  #define VOLSEP_P(ch) ((ch) == ':')
  #define UNC_PATH_P(path) (DIRSEP_P((path)[0]) && DIRSEP_P((path)[1]))
  #define DRIVE_LETTER_P(path) (((size_t)(((path)[0]) | 0x20) - 'a' <= (size_t)'z' - 'a') && (path)[1] == ':')
  #define DRIVE_EQUAL_P(x, y) (((x)[0] | 0x20) == ((y)[0] | 0x20))
#else
  #define PATH_SEPARATOR ":"
  #define DIRSEP_P(ch) ((ch) == '/')
#endif

/* Use HAL lock constants */
#ifndef LOCK_SH
#define LOCK_SH MRB_IO_LOCK_SH
#endif
#ifndef LOCK_EX
#define LOCK_EX MRB_IO_LOCK_EX
#endif
#ifndef LOCK_NB
#define LOCK_NB MRB_IO_LOCK_NB
#endif
#ifndef LOCK_UN
#define LOCK_UN MRB_IO_LOCK_UN
#endif

#if !defined(_WIN32) || defined(MRB_MINGW32_LEGACY)
typedef struct stat         mrb_stat;
# define mrb_stat(path, sb) stat(path, sb)
# define mrb_fstat(fd, sb)  fstat(fd, sb)
#elif defined MRB_INT32
typedef struct _stat32      mrb_stat;
# define mrb_stat(path, sb) _stat32(path, sb)
# define mrb_fstat(fd, sb)  _fstat32(fd, sb)
#else
typedef struct _stat64      mrb_stat;
# define mrb_stat(path, sb) _stat64(path, sb)
# define mrb_fstat(fd, sb)  _fstat64(fd, sb)
#endif

#ifdef _WIN32
static int
flock(int fd, int operation)
{
  HANDLE h = (HANDLE)_get_osfhandle(fd);
  DWORD flags;
  flags = ((operation & LOCK_NB) ? LOCKFILE_FAIL_IMMEDIATELY : 0)
          | ((operation & LOCK_SH) ? LOCKFILE_EXCLUSIVE_LOCK : 0);
  static const OVERLAPPED zero_ov = {0};
  OVERLAPPED ov = zero_ov;
  return LockFileEx(h, flags, 0, 0xffffffff, 0xffffffff, &ov) ? 0 : -1;
}
#endif

/*
 * call-seq:
 *   File.umask([mask]) -> integer
 *
 * Returns the current umask value for this process. If the optional
 * `mask` argument is given, set the umask to that value and return
 * the previous value.
 *
 *   File.umask(0006)   #=> 18
 *   File.umask         #=> 6
 */
static mrb_value
mrb_file_s_umask(mrb_state *mrb, mrb_value klass)
{
  mrb_int mask;
  uint32_t omask;

  if (mrb_get_args(mrb, "|i", &mask) == 0) {
    omask = mrb_hal_io_umask(mrb, -1);
  }
  else {
    omask = mrb_hal_io_umask(mrb, (int32_t)mask);
  }
  return mrb_fixnum_value(omask);
}

/*
 * call-seq:
 *   File.delete(file_name, ...) -> integer
 *   File.unlink(file_name, ...) -> integer
 *
 * Deletes the named file(s). Returns the number of files deleted.
 *
 *   File.delete("a.txt", "b.txt") #=> 2
 */
static mrb_value
mrb_file_s_unlink(mrb_state *mrb, mrb_value obj)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  for (int i = 0; i < argc; i++) {
    mrb_value pathv = argv[i];
    mrb_ensure_string_type(mrb, pathv);
    const char *utf8_path = RSTRING_CSTR(mrb, pathv);
    char *path = mrb_locale_from_utf8(utf8_path, -1);
    if (mrb_hal_io_unlink(mrb, path) < 0) {
      mrb_locale_free(path);
      mrb_sys_fail(mrb, utf8_path);
    }
    mrb_locale_free(path);
  }
  return mrb_fixnum_value(argc);
}

/*
 * call-seq:
 *   File.rename(old_name, new_name) -> 0
 *
 * Renames the given file to the new name.
 *
 *   File.rename("a.txt", "b.txt") #=> 0
 */
static mrb_value
mrb_file_s_rename(mrb_state *mrb, mrb_value obj)
{
  mrb_value from, to;

  mrb_get_args(mrb, "SS", &from, &to);
  char *src = mrb_locale_from_utf8(RSTRING_CSTR(mrb, from), -1);
  char *dst = mrb_locale_from_utf8(RSTRING_CSTR(mrb, to), -1);
  if (mrb_hal_io_rename(mrb, src, dst) < 0) {
#if defined(_WIN32)
    /* Windows retry: try chmod+unlink+rename if initial rename fails */
    if (mrb_hal_io_chmod(mrb, dst, 0666) == 0 &&
        mrb_hal_io_unlink(mrb, dst) == 0 &&
        mrb_hal_io_rename(mrb, src, dst) == 0) {
      mrb_locale_free(src);
      mrb_locale_free(dst);
      return mrb_fixnum_value(0);
    }
#endif
    mrb_locale_free(src);
    mrb_locale_free(dst);
    mrb_sys_fail(mrb, RSTRING_CSTR(mrb, mrb_format(mrb, "(%v, %v)", from, to)));
    return mrb_fixnum_value(-1); /* not reached */
  }
  mrb_locale_free(src);
  mrb_locale_free(dst);
  return mrb_fixnum_value(0);
}

#define SKIP_DIRSEP(p) for (; DIRSEP_P(*(p)); (p)++)
#define NEXT_DIRSEP(p) for (; *(p) != '\0' && !DIRSEP_P(*(p)); (p)++)

static const char*
scan_dirname(const char *path, mrb_int level)
{
  const char *p = path + strlen(path);
  if (level < 1) return p;
  for (; p > path && DIRSEP_P(p[-1]); p--)
    ;
  for (; level > 0; level--) {
    for (; p > path && !DIRSEP_P(p[-1]); p--)
      ;
    for (; p > path && DIRSEP_P(p[-1]); p--)
      ;
  }
  return p > path ? p : path;
}

/*
 * call-seq:
 *   File.dirname(file_name) -> string
 *
 * Returns the directory part of a file name.
 *
 *   File.dirname("/usr/bin/ruby") #=> "/usr/bin"
 */
static mrb_value
mrb_file_dirname(mrb_state *mrb, mrb_value klass)
{
  const char *path;
  mrb_int level = 1;
  mrb_get_args(mrb, "z|i", &path, &level);

  if (level < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative level: %i", level);
  }

  const char *p = path;
#ifdef _WIN32
  if (UNC_PATH_P(p)) {
    p += 2;
    SKIP_DIRSEP(p);
    path = p - 2; /* if consecutive, point to the trailing slash */
    NEXT_DIRSEP(p);
    const char *o = p;
    SKIP_DIRSEP(p);
    if (*p == '\0') {
      p = o;
    }
    else {
      NEXT_DIRSEP(p);
      p = scan_dirname(p, level);
    }
    return mrb_str_new(mrb, path, p - path);
  }
  else if (ISALPHA(p[0]) && p[1] == ':') {
    p += 2;
    const char *o = p;
    SKIP_DIRSEP(p);
    p = scan_dirname(p, level);
    mrb_value s = mrb_str_new(mrb, path, p - path);
    if (p == o) {
      mrb_str_cat_lit(mrb, s, ".");
    }
    return s;
  }
#endif
  SKIP_DIRSEP(p);
  if (p > path) {
    path = p - 1; /* if consecutive, point to the trailing slash */
  }
  p = scan_dirname(p, level);
  return (p == path) ? mrb_str_new_lit(mrb, ".") : mrb_str_new(mrb, path, p - path);
}

/*
 * call-seq:
 *   File.basename(file_name, [suffix]) -> string
 *
 * Returns the last component of the file name.
 *
 *   File.basename("/usr/bin/ruby") #=> "ruby"
 *   File.basename("/usr/bin/ruby.exe", ".exe") #=> "ruby"
 */
static mrb_value
mrb_file_basename(mrb_state *mrb, mrb_value klass)
{
#if defined(_WIN32)
  char bname[_MAX_DIR];
  char extname[_MAX_EXT];
  char *path;
  const char *suffix = NULL;

  mrb_get_args(mrb, "z|z", &path, &suffix);
  size_t ridx = strlen(path);
  if (ridx > 0) {
    ridx--;
    while (ridx > 0 && (path[ridx] == '/' || path[ridx] == '\\')) {
      path[ridx] = '\0';
      ridx--;
    }
    if (ridx == 0 && path[0] == '/') {
      mrb_value result = mrb_str_new_cstr(mrb, path);
      if (suffix && *suffix) {
        mrb_int blen = RSTRING_LEN(result);
        mrb_int slen = strlen(suffix);
        if (blen > slen && memcmp(RSTRING_PTR(result) + blen - slen, suffix, slen) == 0) {
          mrb_str_resize(mrb, result, blen - slen);
        }
      }
      return result;
    }
  }
  _splitpath((const char*)path, NULL, NULL, bname, extname);
  mrb_value buffer = mrb_str_new_cstr(mrb, bname);
  mrb_str_cat_cstr(mrb, buffer, extname);
  if (suffix && *suffix) {
    mrb_int blen = RSTRING_LEN(buffer);
    mrb_int slen = strlen(suffix);
    if (blen > slen && memcmp(RSTRING_PTR(buffer) + blen - slen, suffix, slen) == 0) {
      mrb_str_resize(mrb, buffer, blen - slen);
    }
  }
  return buffer;
#else
  char *path;
  const char *suffix = NULL;

  mrb_get_args(mrb, "z|z", &path, &suffix);

  // Copy path to a local buffer to avoid modifying the original string
  size_t len = strlen(path);
  if (len == 0) {
    return mrb_str_new_lit(mrb, ".");
  }

  // Remove trailing slashes (except when path is only "/")
  while (len > 1 && path[len - 1] == '/') {
    len--;
  }

  // Find the last path separator
  ssize_t base = len - 1;
  while (base >= 0 && path[base] != '/') {
    base--;
  }
  base++; // move to the first character after '/'

  // If path is all slashes, return "/"
  if ((size_t)base == len) {
    return mrb_str_new_lit(mrb, "/");
  }

  mrb_value result = mrb_str_new(mrb, path + base, len - base);

  // Suffix removal (CRuby compatible)
  if (suffix && *suffix) {
    mrb_int blen = RSTRING_LEN(result);
    mrb_int slen = strlen(suffix);
    if (blen > slen && memcmp(RSTRING_PTR(result) + blen - slen, suffix, slen) == 0) {
      mrb_str_resize(mrb, result, blen - slen);
    }
  }

  return result;
#endif
}

/*
 * call-seq:
 *   File.realpath(pathname, [dir_string]) -> string
 *
 * Returns the real (absolute) path of `pathname` in the actual
 * filesystem.
 *
 *   File.realpath("../../bin/ruby") #=> "/usr/bin/ruby"
 */
static mrb_value
mrb_file_realpath(mrb_state *mrb, mrb_value klass)
{
  mrb_value pathname, dir_string;

  if (mrb_get_args(mrb, "S|S", &pathname, &dir_string) == 2) {
    mrb_value s = mrb_str_dup(mrb, dir_string);
    s = mrb_str_cat_cstr(mrb, s, FILE_SEPARATOR);
    s = mrb_str_append(mrb, s, pathname);
    pathname = s;
  }
  char *cpath = mrb_locale_from_utf8(RSTRING_CSTR(mrb, pathname), -1);
  mrb_value result = mrb_str_new_capa(mrb, PATH_MAX);
  if (mrb_hal_io_realpath(mrb, cpath, RSTRING_PTR(result)) == NULL) {
    mrb_locale_free(cpath);
    mrb_sys_fail(mrb, RSTRING_CSTR(mrb, pathname));
    return result;              /* not reached */
  }
  mrb_locale_free(cpath);
  mrb_str_resize(mrb, result, strlen(RSTRING_PTR(result)));
  return result;
}

static const char*
path_getwd(mrb_state *mrb)
{
  char buf[MAXPATHLEN];

  if (mrb_hal_io_getcwd(mrb, buf, MAXPATHLEN) == NULL) {
    mrb_sys_fail(mrb, "getcwd(2)");
  }
  char *utf8 = mrb_utf8_from_locale(buf, -1);
  mrb_value path = mrb_str_new_cstr(mrb, utf8);
  mrb_utf8_free(utf8);
  return RSTRING_CSTR(mrb, path);
}

static mrb_bool
path_absolute_p(const char *path)
{
#ifdef _WIN32
  return UNC_PATH_P(path) ||
         (ISALPHA(path[0]) && VOLSEP_P(path[1]) && DIRSEP_P(path[2]));
#else
  return DIRSEP_P(path[0]);
#endif
}

static void
path_parse(mrb_state *mrb, mrb_value ary, const char *path, int ai)
{
#ifdef _WIN32
  if (DRIVE_LETTER_P(path)) {
    mrb_ary_set(mrb, ary, 0, mrb_str_new(mrb, path, 2));
    path += 2;
    if (DIRSEP_P(*path)) {
      ARY_SET_LEN(mrb_ary_ptr(ary), 1);
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  else if (UNC_PATH_P(path)) {
    path += 2;
    SKIP_DIRSEP(path);
    const char *path0 = path;
    NEXT_DIRSEP(path);
    mrb_value prefix = mrb_str_new_lit(mrb, "//");
    mrb_str_cat(mrb, prefix, path0, path - path0);
    ARY_SET_LEN(mrb_ary_ptr(ary), 0);
    mrb_ary_push(mrb, ary, prefix);
    mrb_gc_arena_restore(mrb, ai);
  }
  else
#endif /* _WIN32 */
  {
    if (RARRAY_LEN(ary) == 0) {
      mrb_ary_set(mrb, ary, 0, mrb_nil_value());
    }
    else if (DIRSEP_P(*path)) {
      ARY_SET_LEN(mrb_ary_ptr(ary), 1);
    }
  }

  for (;;) {
    SKIP_DIRSEP(path);
    const char *path0 = path;
    NEXT_DIRSEP(path);
    ptrdiff_t len = path - path0;
    if (len == 0) {
      break;
    }
    else if (len == 1 && path0[0] == '.') {
      /* do nothing */
    }
    else if (len == 2 && path0[0] == '.' && path0[1] == '.') {
      if (RARRAY_LEN(ary) >= 2) {
        mrb_ary_pop(mrb, ary);
      }
    }
    else {
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, path0, path - path0));
      mrb_gc_arena_restore(mrb, ai);
    }
  }
}

// This function decomposes path into an array based on basedir and workdir.
// The array consists of the root prefix at ary[0], zero or more directories, and finally file names.
// The root prefix is nil for non-Windows, or the drive name or UNC host name for Windows.
static mrb_value
path_split(mrb_state *mrb, const char *path, const char *basedir, const char *workdir)
{
  mrb_value ary = mrb_ary_new(mrb);
  int ai = mrb_gc_arena_save(mrb);

  if (workdir) {
    path_parse(mrb, ary, workdir, ai);
  }

  if (basedir) {
    path_parse(mrb, ary, basedir, ai);
  }

  path_parse(mrb, ary, path, ai);

  return ary;
}

static const char*
path_gethome(mrb_state *mrb, const char **pathp)
{
  mrb_assert(pathp && *pathp && **pathp == '~');

  const char *home;
  mrb_value path;

  const char *username = ++*pathp;
  NEXT_DIRSEP(*pathp);
  ptrdiff_t len = *pathp - username;

  if (len == 0) {
    home = mrb_hal_io_gethome(mrb, NULL);
    if (home == NULL) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "couldn't find HOME environment -- expanding '~'");
    }
    if (!path_absolute_p(home)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "non-absolute home");
    }
  }
  else {
    const char *uname = RSTRING_CSTR(mrb, mrb_str_new(mrb, username, (mrb_int)len));
    home = mrb_hal_io_gethome(mrb, uname);
    if (home == NULL) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "user %s doesn't exist", uname);
    }
    if (!path_absolute_p(home)) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "non-absolute home of ~%s", uname);
    }
  }
  char *home_utf8 = mrb_utf8_from_locale(home, -1);
  path = mrb_str_new_cstr(mrb, home_utf8);
  mrb_utf8_free(home_utf8);

  SKIP_DIRSEP(*pathp);
  return RSTRING_CSTR(mrb, path);
}

static mrb_value
path_expand(mrb_state *mrb, const char *path, const char *base, mrb_bool tilda)
{
  mrb_value ary;

  // split path components as array and normalization
  if (tilda && path[0] == '~') {
    base = path_gethome(mrb, &path);
    ary = path_split(mrb, path, base, NULL);
  }
  else if (path_absolute_p(path)) {
    ary = path_split(mrb, path, NULL, NULL);
  }
  else {
    const char *wd = NULL;
    if (tilda && base[0] == '~') {
      wd = path_gethome(mrb, &base);
    }
#ifndef _WIN32
    else if (!path_absolute_p(base)) {
      wd = path_getwd(mrb);
    }
#else
    else if (DRIVE_LETTER_P(path)) {
      if (DRIVE_LETTER_P(base) && DRIVE_EQUAL_P(path, base) && DIRSEP_P(base[2])) {
        wd = NULL;
      }
      else {
        wd = path_getwd(mrb);
        if (UNC_PATH_P(base) || (DRIVE_LETTER_P(base) && !DRIVE_EQUAL_P(path, base))) {
          base = NULL;
        }
        if (!DRIVE_EQUAL_P(path, wd)) {
          wd = NULL;
        }
      }
    }
    else if (UNC_PATH_P(base)) {
      wd = NULL;
    }
    else {
      wd = path_getwd(mrb);
    }
#endif /* _WIN32 */
    ary = path_split(mrb, path, base, wd);
  }

  // join path components as string
  mrb_value ret;
  mrb_assert(RARRAY_LEN(ary) >= 1);
#ifndef _WIN32
  mrb_assert(mrb_nil_p(RARRAY_PTR(ary)[0]));
  ret = mrb_str_new(mrb, NULL, 0);
#else
  mrb_assert(mrb_string_p(RARRAY_PTR(ary)[0]));
  ret = RARRAY_PTR(ary)[0];
#endif
  if (RARRAY_LEN(ary) == 1) {
#ifdef _WIN32
    mrb_assert(mrb_string_p(ret));
    mrb_assert(RSTRING_LEN(ret) >= 2); // drive letter or UNC prefix
    if (!DIRSEP_P(RSTRING_PTR(ret)[0]))
#endif
    {
      mrb_str_cat_lit(mrb, ret, "/");
    }
  }
  else {
    for (int i = 1; i < RARRAY_LEN(ary); i++) {
      mrb_str_cat_lit(mrb, ret, "/");
      mrb_assert(mrb_string_p(RARRAY_PTR(ary)[i]));
      mrb_str_cat_str(mrb, ret, RARRAY_PTR(ary)[i]);
    }
  }

  return ret;
}

static mrb_value
mrb_file_expand_path(mrb_state *mrb, mrb_value self)
{
  const char *path;
  const char *default_dir = ".";
  mrb_get_args(mrb, "z|z", &path, &default_dir);
  return path_expand(mrb, path, default_dir, TRUE);
}

/*
 * call-seq:
 *   File.absolute_path(file_name, [dir_string]) -> string
 *
 * Converts a pathname to an absolute pathname.
 *
 *   File.absolute_path("~oracle/bin/oracle") #=> "/home/oracle/bin/oracle"
 */
static mrb_value
mrb_file_absolute_path(mrb_state *mrb, mrb_value self)
{
  const char *path;
  const char *default_dir = ".";
  mrb_get_args(mrb, "z|z", &path, &default_dir);
  return path_expand(mrb, path, default_dir, FALSE);
}

/*
 * call-seq:
 *   File.absolute_path?(file_name) -> true or false
 *
 * Returns `true` if the file name is an absolute path, `false` otherwise.
 *
 *   File.absolute_path?("/usr/bin/ruby") #=> true
 *   File.absolute_path?("bin/ruby")      #=> false
 */
static mrb_value
mrb_file_absolute_path_p(mrb_state *mrb, mrb_value klass)
{
  mrb_value path = mrb_get_arg1(mrb);
  mrb_ensure_string_type(mrb, path);
  return mrb_bool_value(path_absolute_p(RSTRING_CSTR(mrb, path)));
}

#define TIME_OVERFLOW_P(a) (sizeof(time_t) >= sizeof(mrb_int) && ((a) > MRB_INT_MAX || (a) < MRB_INT_MIN))
#define TIME_T_UINT (~(time_t)0 > 0)
#if defined(MRB_USE_BITINT)
#define TIME_BIGTIME(mrb, a)                                                   \
  return (TIME_T_UINT ? mrb_bint_new_uint64((mrb), (uint64_t)(a))              \
               : mrb_bint_new_int64(mrb, (int64_t)(a)))
#elif !defined(MRB_NO_FLOAT)
#define TIME_BIGTIME(mrb,a) return mrb_float_value((mrb), (mrb_float)(a))
#else
#define TIME_BIGTIME(mrb, a) mrb_raise(mrb, E_IO_ERROR, #a " overflow")
#endif

static mrb_value
mrb_file_atime(mrb_state *mrb, mrb_value self)
{
  int fd = mrb_io_fileno(mrb, self);
  mrb_io_stat st;

  mrb->c->ci->mid = 0;
  if (mrb_hal_io_fstat(mrb, fd, &st) == -1)
    mrb_sys_fail(mrb, "atime");
  if (TIME_OVERFLOW_P(st.st_atime)) {
    TIME_BIGTIME(mrb, st.st_atime);
  }
  return mrb_int_value(mrb, (mrb_int)st.st_atime);
}

static mrb_value
mrb_file_ctime(mrb_state *mrb, mrb_value self)
{
  int fd = mrb_io_fileno(mrb, self);
  mrb_io_stat st;

  mrb->c->ci->mid = 0;
  if (mrb_hal_io_fstat(mrb, fd, &st) == -1)
    mrb_sys_fail(mrb, "ctime");
  if (TIME_OVERFLOW_P(st.st_ctime)) {
    TIME_BIGTIME(mrb, st.st_ctime);
  }
  return mrb_int_value(mrb, (mrb_int)st.st_ctime);
}

static mrb_value
mrb_file_mtime(mrb_state *mrb, mrb_value self)
{
  int fd = mrb_io_fileno(mrb, self);
  mrb_io_stat st;

  mrb->c->ci->mid = 0;
  if (mrb_hal_io_fstat(mrb, fd, &st) == -1)
    mrb_sys_fail(mrb, "mtime");
  if (TIME_OVERFLOW_P(st.st_mtime)) {
    TIME_BIGTIME(mrb, st.st_mtime);
  }
  return mrb_int_value(mrb, (mrb_int)st.st_mtime);
}

/*
 * call-seq:
 *   file.flock(locking_constant) -> 0 or false
 *
 * Locks or unlocks a file according to `locking_constant`.
 * See `File::LOCK_*` for locking constants.
 *
 *   f = File.new("testfile")
 *   f.flock(File::LOCK_EX)  #=> 0
 *   f.flock(File::LOCK_UN)  #=> 0
 */
static mrb_value
mrb_file_flock(mrb_state *mrb, mrb_value self)
{
#if defined(sun)
  mrb_raise(mrb, E_NOTIMP_ERROR, "flock is not supported on Illumos/Solaris/Windows");
#else
  mrb_int operation;

  mrb_get_args(mrb, "i", &operation);
  int fd = mrb_io_fileno(mrb, self);

  while (mrb_hal_io_flock(mrb, fd, (int)operation) == -1) {
    switch (errno) {
      case EINTR:
        /* retry */
        break;
      case EAGAIN:      /* NetBSD */
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
      case EWOULDBLOCK: /* FreeBSD OpenBSD Linux */
#endif
        if (operation & MRB_IO_LOCK_NB) {
          return mrb_false_value();
        }
        /* FALLTHRU - should not happen */
      default:
        mrb_sys_fail(mrb, "flock");
        break;
    }
  }
#endif
  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   file.size -> integer
 *
 * Returns the size of `file` in bytes.
 *
 *   File.new("testfile").size #=> 66
 */
static mrb_value
mrb_file_size(mrb_state *mrb, mrb_value self)
{
  mrb_io_stat st;
  int fd = mrb_io_fileno(mrb, self);
  if (mrb_hal_io_fstat(mrb, fd, &st) == -1) {
    mrb_sys_fail(mrb, "fstat");
  }

  if (sizeof(st.st_size) >= sizeof(mrb_int) && st.st_size > MRB_INT_MAX) {
#ifdef MRB_NO_FLOAT
    mrb_raise(mrb, E_RUNTIME_ERROR, "File#size too large for MRB_NO_FLOAT");
#else
    return mrb_float_value(mrb, (mrb_float)st.st_size);
#endif
  }

  return mrb_int_value(mrb, (mrb_int)st.st_size);
}

static int
mrb_ftruncate(mrb_state *mrb, int fd, mrb_int length)
{
  return mrb_hal_io_ftruncate(mrb, fd, (int64_t)length);
}

/*
 * call-seq:
 *   file.truncate(integer) -> 0
 *
 * Truncates a file to a maximum of `integer` bytes.
 *
 *   f = File.new("out", "w")
 *   f.write("1234567890")   #=> 10
 *   f.truncate(5)         #=> 0
 *   f.size                #=> 5
 */
static mrb_value
mrb_file_truncate(mrb_state *mrb, mrb_value self)
{
  mrb_value lenv = mrb_get_arg1(mrb);
  int fd = mrb_io_fileno(mrb, self);
  mrb_int length = mrb_as_int(mrb, lenv);
  if (mrb_ftruncate(mrb, fd, length) != 0) {
    mrb_sys_fail(mrb, "ftruncate");
  }

  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   File.symlink(old_name, new_name) -> 0
 *
 * Creates a symbolic link `new_name` for the file `old_name`.
 *
 *   File.symlink("testfile", "link-to-test") #=> 0
 */
static mrb_value
mrb_file_s_symlink(mrb_state *mrb, mrb_value klass)
{
  mrb_value from, to;

  mrb_get_args(mrb, "SS", &from, &to);
  char *src = mrb_locale_from_utf8(RSTRING_CSTR(mrb, from), -1);
  char *dst = mrb_locale_from_utf8(RSTRING_CSTR(mrb, to), -1);
  if (mrb_hal_io_symlink(mrb, src, dst) == -1) {
    mrb_locale_free(src);
    mrb_locale_free(dst);
    mrb_sys_fail(mrb, RSTRING_CSTR(mrb, mrb_format(mrb, "(%v, %v)", from, to)));
  }
  mrb_locale_free(src);
  mrb_locale_free(dst);
  return mrb_fixnum_value(0);
}

/*
 * call-seq:
 *   File.chmod(mode_int, file_name, ...) -> integer
 *
 * Changes permission bits on the named file(s) to the bit pattern
 * represented by `mode_int`.
 *
 *   File.chmod(0644, "testfile", "out") #=> 2
 */
static mrb_value
mrb_file_s_chmod(mrb_state *mrb, mrb_value klass)
{
  mrb_int mode;
  mrb_int argc;
  const mrb_value *filenames;
  int ai = mrb_gc_arena_save(mrb);

  mrb_get_args(mrb, "i*", &mode, &filenames, &argc);
  for (int i = 0; i < argc; i++) {
    mrb_ensure_string_type(mrb, filenames[i]);
    const char *utf8_path = RSTRING_CSTR(mrb, filenames[i]);
    char *path = mrb_locale_from_utf8(utf8_path, -1);
    if (mrb_hal_io_chmod(mrb, path, (uint32_t)mode) == -1) {
      mrb_locale_free(path);
      mrb_sys_fail(mrb, utf8_path);
    }
    mrb_locale_free(path);
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_fixnum_value(argc);
}

/*
 * call-seq:
 *   File.readlink(link_name) -> string
 *
 * Returns the name of the file referenced by the given link.
 *
 *   File.symlink("testfile", "link-to-test") #=> 0
 *   File.readlink("link-to-test")          #=> "testfile"
 */
static mrb_value
mrb_file_s_readlink(mrb_state *mrb, mrb_value klass)
{
  const char *path;
  size_t bufsize = 100;

  mrb_get_args(mrb, "z", &path);

  char *tmp = mrb_locale_from_utf8(path, -1);
  char *buf = (char*)mrb_malloc(mrb, bufsize);

  int64_t rc;
  while ((rc = mrb_hal_io_readlink(mrb, tmp, buf, bufsize)) == (int64_t)bufsize) {
    bufsize += 100;
    buf = (char*)mrb_realloc(mrb, buf, bufsize);
  }
  mrb_locale_free(tmp);
  if (rc == -1) {
    mrb_free(mrb, buf);
    mrb_sys_fail(mrb, path);
  }
  tmp = mrb_utf8_from_locale(buf, -1);

  mrb_value ret = mrb_str_new(mrb, tmp, rc);
  mrb_utf8_free(tmp);
  mrb_free(mrb, buf);

  return ret;
}

/*
 * call-seq:
 *   File.extname(path) -> string
 *
 * Returns the extension (the portion of file name in path starting from the
 * last period). If path is a dotfile, or starts with a period, then the starting
 * dot is not dealt with the start of the extension.
 *
 *   File.extname("test.rb")         #=> ".rb"
 *   File.extname("a/b/d/test.rb")   #=> ".rb"
 *   File.extname("test")            #=> ""
 *   File.extname(".profile")        #=> ""
 */
static mrb_value
mrb_file_extname(mrb_state *mrb, mrb_value klass)
{
  char *path;
  mrb_get_args(mrb, "z", &path);

  size_t len = strlen(path);
  if (len == 0) {
    return mrb_str_new_lit(mrb, "");
  }

  // Remove trailing slashes to find the actual filename
  while (len > 1 && path[len - 1] == '/') {
    len--;
  }

  // Find the last path separator to get basename
  ssize_t base_start = len - 1;
  while (base_start >= 0 && path[base_start] != '/') {
    base_start--;
  }
  base_start++; // move to first character after '/'

  // If the result is only slashes, no extension
  if ((size_t)base_start == len) {
    return mrb_str_new_lit(mrb, "");
  }

  // Look for the last '.' in the basename
  ssize_t dot_pos = -1;
  for (size_t i = base_start; i < len; i++) {
    if (path[i] == '.') {
      dot_pos = i;
    }
  }

  // No dot found, or dot is the first character (dotfile)
  if (dot_pos == -1 || dot_pos == (ssize_t)base_start) {
    return mrb_str_new_lit(mrb, "");
  }

  // Return extension from dot to end
  return mrb_str_new(mrb, path + dot_pos, len - dot_pos);
}

/*
 * call-seq:
 *   File.path(path)  -> string
 *
 * Returns the string representation of the path
 *
 *   File.path("/dev/null")          #=> "/dev/null"
 *   File.path(Pathname.new("/tmp")) #=> "/tmp"
 */
static mrb_value
mrb_file_path(mrb_state *mrb, mrb_value klass)
{
  mrb_value filename;
  mrb_get_args(mrb, "S", &filename);
  return filename;
}

// Forward declaration for recursive join processing
static mrb_value mrb_file_join_process_args(mrb_state *mrb, const mrb_value *argv, mrb_int argc);

static mrb_value
mrb_file_join_process_args(mrb_state *mrb, const mrb_value *argv, mrb_int argc)
{
  mrb_value result = mrb_ary_new_capa(mrb, argc);

  for (mrb_int i = 0; i < argc; i++) {
    mrb_value arg = argv[i];

    if (mrb_array_p(arg)) {
      // Check for recursive arrays using mruby's built-in detection
      if (MRB_RECURSIVE_UNARY_P(mrb, MRB_SYM(join), arg)) {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "recursive array");
      }

      // Recursively process the array
      mrb_value nested = mrb_file_join_process_args(mrb, RARRAY_PTR(arg), RARRAY_LEN(arg));

      // Append nested results to our result
      mrb_int nested_len = RARRAY_LEN(nested);
      for (mrb_int k = 0; k < nested_len; k++) {
        mrb_ary_push(mrb, result, RARRAY_PTR(nested)[k]);
      }
    }
    else {
      // Convert to string (raises TypeError if not convertible)
      mrb_ensure_string_type(mrb, arg);
      mrb_ary_push(mrb, result, arg);
    }
  }

  return result;
}

/*
 * call-seq:
 *   File.join(string, ...) -> string
 *
 * Returns a new string formed by joining the strings using the operating
 * system's path separator (File::SEPARATOR).
 *
 *   File.join("usr", "mail", "gumby")   #=> "usr/mail/gumby"
 *   File.join("usr", "mail", "gumby")   #=> "usr\\mail\\gumby" (on Windows)
 */
static mrb_value
mrb_file_join(mrb_state *mrb, mrb_value klass)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);

  // Handle empty case
  if (argc == 0) {
    return mrb_str_new_lit(mrb, "");
  }

  // Process arguments and flatten arrays
  mrb_value names = mrb_file_join_process_args(mrb, argv, argc);

  mrb_int names_len = RARRAY_LEN(names);
  if (names_len == 0) {
    return mrb_str_new_lit(mrb, "");
  }

  // Handle single element case
  if (names_len == 1) {
    return RARRAY_PTR(names)[0];
  }

  // Start building the result
  mrb_value first = RARRAY_PTR(names)[0];
  mrb_value result = mrb_str_dup(mrb, first);

  // Remove trailing separator from first component
  const char *sep = FILE_SEPARATOR;
  mrb_int sep_len = strlen(sep);
  if (RSTRING_LEN(result) > 0 &&
      RSTRING_LEN(result) >= sep_len &&
      memcmp(RSTRING_PTR(result) + RSTRING_LEN(result) - sep_len, sep, sep_len) == 0) {
    mrb_str_resize(mrb, result, RSTRING_LEN(result) - sep_len);
  }

  // Process middle components
  for (mrb_int i = 1; i < names_len - 1; i++) {
    mrb_value component = RARRAY_PTR(names)[i];
    const char *comp_str = RSTRING_PTR(component);
    mrb_int comp_len = RSTRING_LEN(component);

    // Skip empty components
    if (comp_len == 0) continue;

    // Remove leading separator
    if (comp_len >= sep_len && memcmp(comp_str, sep, sep_len) == 0) {
      comp_str += sep_len;
      comp_len -= sep_len;
    }

    // Remove trailing separator
    if (comp_len >= sep_len && memcmp(comp_str + comp_len - sep_len, sep, sep_len) == 0) {
      comp_len -= sep_len;
    }

    // Add separator and component if not empty
    if (comp_len > 0) {
      mrb_str_cat_cstr(mrb, result, sep);
      mrb_str_cat(mrb, result, comp_str, comp_len);
    }
  }

  // Process last component
  if (names_len > 1) {
    mrb_value last = RARRAY_PTR(names)[names_len - 1];
    const char *last_str = RSTRING_PTR(last);
    mrb_int last_len = RSTRING_LEN(last);

    // Remove leading separator from last component
    if (last_len >= sep_len && memcmp(last_str, sep, sep_len) == 0) {
      last_str += sep_len;
      last_len -= sep_len;
    }

    // Add separator and last component
    mrb_str_cat_cstr(mrb, result, sep);
    mrb_str_cat(mrb, result, last_str, last_len);
  }

  return result;
}

void
mrb_init_file(mrb_state *mrb)
{
  struct RClass *io   = mrb_class_get_id(mrb, MRB_SYM(IO));
  struct RClass *file = mrb_define_class_id(mrb, MRB_SYM(File), io);
  MRB_SET_INSTANCE_TT(file, MRB_TT_CDATA);
  mrb_define_class_method_id(mrb, file, MRB_SYM(umask),  mrb_file_s_umask, MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(delete), mrb_file_s_unlink, MRB_ARGS_ANY());
  mrb_define_class_method_id(mrb, file, MRB_SYM(unlink), mrb_file_s_unlink, MRB_ARGS_ANY());
  mrb_define_class_method_id(mrb, file, MRB_SYM(rename), mrb_file_s_rename, MRB_ARGS_REQ(2));
  mrb_define_class_method_id(mrb, file, MRB_SYM(symlink), mrb_file_s_symlink, MRB_ARGS_REQ(2));
  mrb_define_class_method_id(mrb, file, MRB_SYM(chmod), mrb_file_s_chmod, MRB_ARGS_REQ(1) | MRB_ARGS_REST());
  mrb_define_class_method_id(mrb, file, MRB_SYM(readlink), mrb_file_s_readlink, MRB_ARGS_REQ(1));

  mrb_define_class_method_id(mrb, file, MRB_SYM(dirname),   mrb_file_dirname,    MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(basename),  mrb_file_basename,   MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(extname),   mrb_file_extname,    MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(join),      mrb_file_join,       MRB_ARGS_ANY());
  mrb_define_class_method_id(mrb, file, MRB_SYM(path),      mrb_file_path,       MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(realpath),  mrb_file_realpath,   MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(absolute_path), mrb_file_absolute_path, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM_Q(absolute_path), mrb_file_absolute_path_p, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, file, MRB_SYM(expand_path),  mrb_file_expand_path, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  mrb_define_method_id(mrb, file, MRB_SYM(flock), mrb_file_flock, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, file, MRB_SYM(_atime), mrb_file_atime, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, file, MRB_SYM(_ctime), mrb_file_ctime, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, file, MRB_SYM(_mtime), mrb_file_mtime, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, file, MRB_SYM(size), mrb_file_size, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, file, MRB_SYM(truncate), mrb_file_truncate, MRB_ARGS_REQ(1));

  struct RClass *cnst = mrb_define_module_under_id(mrb, file, MRB_SYM(Constants));
  mrb_define_const_id(mrb, cnst, MRB_SYM(LOCK_SH), mrb_fixnum_value(LOCK_SH));
  mrb_define_const_id(mrb, cnst, MRB_SYM(LOCK_EX), mrb_fixnum_value(LOCK_EX));
  mrb_define_const_id(mrb, cnst, MRB_SYM(LOCK_UN), mrb_fixnum_value(LOCK_UN));
  mrb_define_const_id(mrb, cnst, MRB_SYM(LOCK_NB), mrb_fixnum_value(LOCK_NB));
  mrb_define_const_id(mrb, cnst, MRB_SYM(SEPARATOR), mrb_str_new_cstr(mrb, FILE_SEPARATOR));
  mrb_define_const_id(mrb, cnst, MRB_SYM(PATH_SEPARATOR), mrb_str_new_cstr(mrb, PATH_SEPARATOR));
#if defined(_WIN32)
  mrb_define_const_id(mrb, cnst, MRB_SYM(ALT_SEPARATOR), mrb_str_new_cstr(mrb, FILE_ALT_SEPARATOR));
#else
  mrb_define_const_id(mrb, cnst, MRB_SYM(ALT_SEPARATOR), mrb_nil_value());
#endif
  mrb_define_const_id(mrb, cnst, MRB_SYM(NULL), mrb_str_new_cstr(mrb, NULL_FILE));

  mrb_define_const_id(mrb, cnst, MRB_SYM(RDONLY), mrb_fixnum_value(MRB_O_RDONLY));
  mrb_define_const_id(mrb, cnst, MRB_SYM(WRONLY), mrb_fixnum_value(MRB_O_WRONLY));
  mrb_define_const_id(mrb, cnst, MRB_SYM(RDWR), mrb_fixnum_value(MRB_O_RDWR));
  mrb_define_const_id(mrb, cnst, MRB_SYM(APPEND), mrb_fixnum_value(MRB_O_APPEND));
  mrb_define_const_id(mrb, cnst, MRB_SYM(CREAT), mrb_fixnum_value(MRB_O_CREAT));
  mrb_define_const_id(mrb, cnst, MRB_SYM(EXCL), mrb_fixnum_value(MRB_O_EXCL));
  mrb_define_const_id(mrb, cnst, MRB_SYM(TRUNC), mrb_fixnum_value(MRB_O_TRUNC));
  mrb_define_const_id(mrb, cnst, MRB_SYM(NONBLOCK), mrb_fixnum_value(MRB_O_NONBLOCK));
  mrb_define_const_id(mrb, cnst, MRB_SYM(NOCTTY), mrb_fixnum_value(MRB_O_NOCTTY));
  mrb_define_const_id(mrb, cnst, MRB_SYM(BINARY), mrb_fixnum_value(MRB_O_BINARY));
  mrb_define_const_id(mrb, cnst, MRB_SYM(SHARE_DELETE), mrb_fixnum_value(MRB_O_SHARE_DELETE));
  mrb_define_const_id(mrb, cnst, MRB_SYM(SYNC), mrb_fixnum_value(MRB_O_SYNC));
  mrb_define_const_id(mrb, cnst, MRB_SYM(DSYNC), mrb_fixnum_value(MRB_O_DSYNC));
  mrb_define_const_id(mrb, cnst, MRB_SYM(RSYNC), mrb_fixnum_value(MRB_O_RSYNC));
  mrb_define_const_id(mrb, cnst, MRB_SYM(NOFOLLOW), mrb_fixnum_value(MRB_O_NOFOLLOW));
  mrb_define_const_id(mrb, cnst, MRB_SYM(NOATIME), mrb_fixnum_value(MRB_O_NOATIME));
  mrb_define_const_id(mrb, cnst, MRB_SYM(DIRECT), mrb_fixnum_value(MRB_O_DIRECT));
  mrb_define_const_id(mrb, cnst, MRB_SYM(TMPFILE), mrb_fixnum_value(MRB_O_TMPFILE));
}
