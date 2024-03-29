#include <mruby.h>

#ifndef MRB_NO_FLOAT
/*
 * strtod implementation.
 * author: Yasuhiro Matsumoto (@mattn)
 * license: public domain
 */

/*
The original code can be found in https://github.com/mattn/strtod

I modified the routine for mruby:

 * renamed the function `vim_strtod` -> `mrb_read_float`
 * simplified the code
 * changed the API

My modifications in this file are also placed in the public domain.

Matz (Yukihiro Matsumoto)
*/

#include <string.h>
#include <math.h>

MRB_API mrb_bool
mrb_read_float(const char *str, char **endp, double *fp)
{
  double d = 0.0;
  int sign;
  const char *p, *a;

  a = p = str;
  while (ISSPACE(*p))
    p++;

  /* decimal part */
  sign = 1;
  if (*p == '-') {
    sign = -1;
    p++;
  }
  else if (*p == '+')
    p++;
  if (ISDIGIT(*p)) {
    d = (double)(*p++ - '0');
    while (*p && ISDIGIT(*p)) {
      d = d * 10.0 + (double)(*p - '0');
      p++;
    }
    a = p;
  }
  else if (*p != '.')
    goto done;
  d *= sign;

  /* fraction part */
  if (*p == '.') {
    double f = 0.0;
    double base = 0.1;
    p++;

    if (ISDIGIT(*p)) {
      while (*p && ISDIGIT(*p)) {
        f += base * (*p - '0');
        base /= 10.0;
        p++;
      }
    }
    d += f * sign;
    a = p;
  }

  /* exponential part */
  if ((*p == 'E') || (*p == 'e')) {
    int e = 0;

    p++;
    sign = 1;
    if (*p == '-') {
      sign = -1;
      p++;
    }
    else if (*p == '+')
      p++;

    if (ISDIGIT(*p)) {
      while (*p == '0')
        p++;
      if (*p == '\0') --p;
      e = (int)(*p++ - '0');
      for (; *p && ISDIGIT(*p); p++) {
        if (e < 10000)
          e = e * 10 + (*p - '0');
      }
      e *= sign;
    }
    else if (!ISDIGIT(*(a-1))) {
      return FALSE;
    }
    else if (*p == 0)
      goto done;
    d *= pow(10.0, (double)e);
    a = p;
  }
  else if (p > str && !ISDIGIT(*(p-1))) {
    goto done;
  }

done:
  *fp = d;
  if (endp) *endp = (char*)a;
  if (str == a) return FALSE;
  return TRUE;
}
#endif
