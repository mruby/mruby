

#include <ctype.h>
#include <string.h>
#include <stddef.h>

#include "mruby.h"
#include "mruby/array.h"
#include "mruby/string.h"

extern mrb_value mrb_ary_join(mrb_state *mrb, mrb_value ary, mrb_value sep);

/*
 * Simple message formatting.
 *  Formats " Some strings {N} and {NN} ". N means digit.
 *  This function substitutes N-th array element for {N}.
 *  It returns the substituted format string which type is MRB_TT_STRING.
 *  It is used Array.join("") for building the formatted string.
 */
mrb_value
mrb_format_message(mrb_state *mrb, const char *format, mrb_value array)
{
  const char *p;
  mrb_value message;

  p = format;

  if (mrb_array_p(array)) {
    mrb_value ary = mrb_ary_new_capa(mrb, 4);
    const char *p_start = p;
    size_t len;

    do {
      if (*p == '{') {
	const char *p_end = p;
        int n = 0;
        p++;
        while (*p != '}' && *p != '\0') {
          if (isdigit(*p)) {
            n = n * 10 + *p++ - '0';
          }
          else {
            break;
          }
        }
	if (*p == '}') {
          mrb_value v = mrb_ary_ref(mrb, array, n);
          if (!mrb_nil_p(v)) {
            ptrdiff_t size = p_end - p_start;
            if (size != 0) {
              mrb_ary_push(mrb, ary, mrb_str_new(mrb, p_start, size));
            }
            p_start = p + 1;
            mrb_ary_push(mrb, ary, mrb_ary_ref(mrb, array, n));
          }
	}
      }
    } while(*++p != '\0');
    len = strlen(p_start);
    if (len != 0) {
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, p_start, len));
    }

    message = mrb_ary_join(mrb, ary, mrb_str_new(mrb,NULL,0));
  }
  else {
    message = mrb_str_new_cstr(mrb, format);
  }

  return message;
}
