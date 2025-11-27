/*
** strftime.c - Time#strftime
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/time.h>
#include <mruby/class.h>
#include <mruby/presym.h>
#include <time.h>
#include <string.h>

#define INITIAL_BUFFER_SIZE 64
#define MAX_BUFFER_SIZE 4096

/*
 * call-seq:
 *   time.strftime(format) -> string
 *
 * Formats time according to the directives in the given format string.
 *
 * The format string may contain NUL bytes, which will be preserved in
 * the output string.
 *
 * Common format directives:
 *   %Y - Year with century (e.g., 2023)
 *   %m - Month of the year (01-12)
 *   %d - Day of the month (01-31)
 *   %H - Hour of the day, 24-hour clock (00-23)
 *   %M - Minute of the hour (00-59)
 *   %S - Second of the minute (00-60)
 *   %% - Literal % character
 *
 * See your system's strftime(3) documentation for a complete list.
 *
 *   t = Time.new(2023, 12, 25, 10, 30, 45)
 *   t.strftime("%Y-%m-%d %H:%M:%S")  #=> "2023-12-25 10:30:45"
 *   t.strftime("%A, %B %d, %Y")      #=> "Monday, December 25, 2023"
 */
static mrb_value
mrb_time_strftime(mrb_state *mrb, mrb_value self)
{
  const char *format;
  mrb_int format_len;

  mrb_get_args(mrb, "s", &format, &format_len);
  struct tm *tm = mrb_time_get_tm(mrb, self);

  mrb_value result = mrb_str_new(mrb, NULL, 0);
  const char *fmt_ptr = format;
  mrb_int remaining = format_len;

  /* Process format string in segments, handling NUL bytes */
  while (remaining > 0) {
    const char *nul_pos = (const char *)memchr(fmt_ptr, '\0', (size_t)remaining);
    mrb_int segment_len = nul_pos ? (nul_pos - fmt_ptr) : remaining;

    /* Process this segment (up to NUL or end of string) */
    if (segment_len > 0) {
      char *segment;
      size_t buf_size;
      char *buf;
      size_t n;

      /* Create null-terminated copy of this segment */
      segment = (char *)mrb_malloc(mrb, (size_t)segment_len + 1);
      memcpy(segment, fmt_ptr, (size_t)segment_len);
      segment[segment_len] = '\0';

#ifdef _MSC_VER
      /* Check for GNU extension %-flag which crashes on MSVC */
      /* Scan for %- patterns in the format string */
      for (const char *p = segment; *p != '\0'; p++) {
        if (p[0] == '%' && p[1] == '-') {
          mrb_free(mrb, segment);
          mrb_raisef(mrb, E_ARGUMENT_ERROR,
                     "strftime format flag '%-' not supported on this platform (use '%%#' on Windows)");
        }
      }
#endif

      /* Allocate buffer for formatted output */
      buf_size = INITIAL_BUFFER_SIZE;
      buf = (char *)mrb_malloc(mrb, buf_size);

      /* Try formatting; grow buffer if needed */
      while (1) {
        n = strftime(buf, buf_size, segment, tm);

        /*
         * strftime returns 0 if:
         * 1. Buffer is too small (retry with larger buffer)
         * 2. Format produces empty result (stop retrying)
         * We distinguish by checking buffer size limit.
         */
        if (n > 0 || buf_size >= MAX_BUFFER_SIZE) {
          break;
        }

        /* Double buffer size and retry */
        buf_size *= 2;
        buf = (char *)mrb_realloc(mrb, buf, buf_size);
      }

      /* Append formatted output to result */
      mrb_str_cat(mrb, result, buf, n);

      mrb_free(mrb, buf);
      mrb_free(mrb, segment);
    }

    /* If there was a NUL, append it to result and advance past it */
    if (nul_pos) {
      mrb_str_cat(mrb, result, "\0", 1);
      fmt_ptr = nul_pos + 1;
      remaining -= segment_len + 1;
    }
    else {
      break;
    }
  }

  return result;
}

void
mrb_mruby_strftime_gem_init(mrb_state *mrb)
{
  struct RClass *time_class;

  time_class = mrb_class_get_id(mrb, MRB_SYM(Time));
  mrb_define_method_id(mrb, time_class, MRB_SYM(strftime), mrb_time_strftime, MRB_ARGS_REQ(1));
}

void
mrb_mruby_strftime_gem_final(mrb_state *mrb)
{
}
