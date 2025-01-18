/*
** print.c - Kernel#p, Kernel#print
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include <string.h>
#if defined(_WIN32)
# include <windows.h>
# include <io.h>
#ifdef _MSC_VER
# define isatty(x) _isatty(x)
# define fileno(x) _fileno(x)
#endif
#else
# include <unistd.h>
#endif

#ifndef MRB_NO_STDIO
static void
printcstr(mrb_state *mrb, const char *str, size_t len, FILE *stream)
{
#if defined(_WIN32)
  if (isatty(fileno(stream))) {
    DWORD written;
    int wlen = MultiByteToWideChar(CP_UTF8, 0, str, (int)len, NULL, 0);
    wchar_t* utf16 = (wchar_t*)mrb_malloc(mrb, (wlen+1) * sizeof(wchar_t));
    if (MultiByteToWideChar(CP_UTF8, 0, str, (int)len, utf16, wlen) > 0) {
      utf16[wlen] = 0;
      WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE),
                    utf16, (DWORD)wlen, &written, NULL);
    }
    mrb_free(mrb, utf16);
    return;
  }
#endif
  fwrite(str, (size_t)len, 1, stream);
}

static void
printstr(mrb_state *mrb, mrb_value obj, FILE *stream)
{
  if (!mrb_string_p(obj)) {
    obj = mrb_obj_as_string(mrb, obj);
  }
  printcstr(mrb, RSTRING_PTR(obj), RSTRING_LEN(obj), stream);
}

static void
printstrln(mrb_state *mrb, mrb_value obj, FILE *stream)
{
  printstr(mrb, obj, stream);
  printcstr(mrb, "\n", 1, stdout);
}

void
mrb_core_init_printabort(mrb_state *mrb)
{
  static const char *str = "Failed mruby core initialization";
  printcstr(mrb, str, strlen(str), stdout);
}

#ifndef HAVE_MRUBY_IO_GEM
mrb_value
mrb_print_m(mrb_state *mrb, mrb_value self)
{
  mrb_int argc;
  mrb_value *argv;

  mrb_get_args(mrb, "*", &argv, &argc);
  for (mrb_int i=0; i<argc; i++) {
    printstr(mrb, mrb_obj_as_string(mrb, argv[i]), stdout);
  }
  if (isatty(fileno(stdout))) fflush(stdout);
  return mrb_nil_value();
}
#endif

MRB_API void
mrb_p(mrb_state *mrb, mrb_value obj)
{
  if (mrb_type(obj) == MRB_TT_EXCEPTION && mrb_obj_ptr(obj) == mrb->nomem_err) {
    static const char *str = "Out of memory\n";
    printcstr(mrb, str, strlen(str), stdout);
  }
  else {
    printstrln(mrb, mrb_inspect(mrb, obj), stdout);
  }
  if (isatty(fileno(stdout))) fflush(stdout);
}


MRB_API void
mrb_show_version(mrb_state *mrb)
{
  printstrln(mrb, mrb_const_get(mrb, mrb_obj_value(mrb->object_class), MRB_SYM(MRUBY_DESCRIPTION)), stdout);
}

MRB_API void
mrb_show_copyright(mrb_state *mrb)
{
  printstrln(mrb, mrb_const_get(mrb, mrb_obj_value(mrb->object_class), MRB_SYM(MRUBY_COPYRIGHT)), stdout);
}

#else
void
mrb_core_init_printabort(mrb_state *mrb)
{
}

mrb_value
mrb_print_m(mrb_state *mrb, mrb_value self)
{
  return mrb_nil_value();
}

MRB_API void
mrb_p(mrb_state *mrb, mrb_value obj)
{
}

MRB_API void
mrb_show_version(mrb_state *mrb)
{
}

MRB_API void
mrb_show_copyright(mrb_state *mrb)
{
}
#endif
