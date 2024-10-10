#include <mruby.h>

#ifndef MRB_NO_FLOAT

#include <string.h>
#include <math.h>

MRB_API mrb_bool
mrb_read_float(const char *str, char **endp, double *fp)
{
  const char *p = str;
  const char *a = p;
  double res = 0.0, frac = 0.0, div = 1.0;
  int sign = 1;
  int digits = 0;

  // Skip whitespace
  while (ISSPACE((unsigned char)*p)) p++;

  // Handle sign
  if (*p == '-') { sign = -1; p++; }
  else if (*p == '+') p++;

  // Parse integer part
  while (ISDIGIT(*p)) {
    res = res * 10.0 + (*p - '0');
    digits++;
    a = ++p;
  }

  // Parse fractional part
  if (*p == '.') {
    p++;
    while (ISDIGIT(*p)) {
      frac = frac * 10.0 + (*p++ - '0');
      div *= 10.0;
      digits++;
    }
    a = p;
  }

  // If no digits were found, return 0
  if (digits == 0) {
    if (endp) *endp = (char*)str;
    *fp = 0.0;
    return FALSE;
  }

  // Combine integer and fractional parts
  res += frac / div;
  res *= sign;

  // Handle exponent
  if ((*p | 32) == 'e') {
    int e = 0;
    sign = 1;
    p++;
    if (*p == '-') { sign = -1; p++; }
    else if (*p == '+') p++;

    // If no digits follow 'e', ignore the exponent part
    if (!ISDIGIT(*p)) goto done;

    while (ISDIGIT(*p)) {
      if (e < 10000)  // 10000 is big enough to get Infinity
        e = e * 10 + (*p - '0');
      p++;
    }
    res *= pow(10.0, sign * e);
    a = p;
  }

  // Set endp
 done:
  if (endp) *endp = (char*)a;
  *fp = res;

  // strtod(3) stores ERANGE to errno for overflow/underflow
  // mruby does not require those checks
#if 0
  // Check for underflow after applying the exponent
  if (res != 0.0 && fabs(res) < DBL_MIN) {
    return FALSE;
  }

  // Check if the result is infinity or NaN
  if (isinf(res) || isnan(res)) {
    return FALSE;
  }
#endif
  return TRUE;
}

#endif
