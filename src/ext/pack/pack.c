/*
 ** pack.c - Array#pack, String#unpack
 **
 ** See Copyright Notice in mruby.h
 */

#include "mruby.h"

#ifdef ENABLE_PACK
//#define PACK_DEBUG 1

#include "error.h"
#include "mruby/variable.h"
#include "mruby/string.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include "encoding.h"

#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#ifdef PACK_DEBUG
  #include <stdarg.h>
  #define dprintf(s,...)   printf("%s:%d " s, __FILE__, __LINE__,__VA_ARGS__)
#else
  #define dprintf(...)     ((void)0)
#endif

static const char toofew_err_msg[] = "too few arguments";
static const char uu_table[] =
"`!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
static const char b64_table[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char natstr[] = "sSiIlL";
static const char endstr[] = "sSiIlLqQ";


static void
encodes(mrb_state *mrb, mrb_value str, const char *s, long len, int type, int tail_lf)
{
  char buff[4096];
  long i = 0;
  const char *trans = type == 'u' ? uu_table : b64_table;
  int padding;

  if (type == 'u') {
    buff[i++] = (char)len + ' ';
    padding = '`';
  }
  else {
    padding = '=';
  }
  while (len >= 3) {
    while (len >= 3 && sizeof(buff)-i >= 4) {
      buff[i++] = trans[077 & (*s >> 2)];
      buff[i++] = trans[077 & (((*s << 4) & 060) | ((s[1] >> 4) & 017))];
      buff[i++] = trans[077 & (((s[1] << 2) & 074) | ((s[2] >> 6) & 03))];
      buff[i++] = trans[077 & s[2]];
      s += 3;
      len -= 3;
    }
    if (sizeof(buff)-i < 4) {
      mrb_str_buf_cat(mrb, str, buff, i);
      i = 0;
    }
  }

  if (len == 2) {
    buff[i++] = trans[077 & (*s >> 2)];
    buff[i++] = trans[077 & (((*s << 4) & 060) | ((s[1] >> 4) & 017))];
    buff[i++] = trans[077 & (((s[1] << 2) & 074) | (('\0' >> 6) & 03))];
    buff[i++] = padding;
  }
  else if (len == 1) {
    buff[i++] = trans[077 & (*s >> 2)];
    buff[i++] = trans[077 & (((*s << 4) & 060) | (('\0' >> 4) & 017))];
    buff[i++] = padding;
    buff[i++] = padding;
  }
  if (tail_lf) buff[i++] = '\n';
  mrb_str_buf_cat(mrb, str, buff, i);
}


static mrb_value
mrb_pack_pack(mrb_state *mrb, mrb_value ary)
{
  static const char nul10[] = "\0\0\0\0\0\0\0\0\0\0";
  const char *p, *pend;
  char ch, type;
  mrb_value result, from, fmt;
  long items, len, idx, plen;
  const char *ptr;

  mrb_get_args(mrb, "S", &fmt);

  p = RSTRING_PTR(fmt);
  pend = RSTRING_END(fmt);

  result = mrb_str_buf_new(mrb, 0);
  items = RARRAY_LEN(ary);
  idx = 0;
#define TOO_FEW (mrb_raise(mrb, E_ARGUMENT_ERROR, toofew_err_msg), mrb_nil_value())
#define THISFROM (items > 0 ? RARRAY_PTR(ary)[idx] : TOO_FEW)
#define NEXTFROM (items-- > 0 ? RARRAY_PTR(ary)[idx++] : TOO_FEW)

  while (p < pend) {
    int explicit_endian = 0;
    if (RSTRING_PTR(fmt) + RSTRING_LEN(fmt) != pend) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "format string modified");
    }
    type = *p++;  /* get data type */

    if (ISSPACE(type)) continue;
    if (type == '#') {
      while ((p < pend) && (*p != '\n')) {
        p++;
      }
      continue;
    }

    { /* modifiers */
modifiers:
      switch (*p) {
        case '_':
        case '!':
          if (strchr(natstr, type)) {
            p++;
          } else {
            mrb_raise(mrb, E_ARGUMENT_ERROR, "'%c' allowed only after types %s", *p, natstr);
          }
          goto modifiers;
        case '<':
        case '>':
          if (!strchr(endstr, type)) {
            mrb_raise(mrb, E_ARGUMENT_ERROR, "'%c' allowed only after types %s", *p, endstr);
          }
          if (explicit_endian) {
            mrb_raise(mrb, E_RANGE_ERROR, "Can't use both '<' and '>'");
          }
          explicit_endian = *p++;
          goto modifiers;
      }
    } /* end of modifiers */

    /* set data length */
    if (*p == '*') {
      len = strchr("@Xxu", type) ? 0 : (strchr("PMm", type) ? 1 : items);
      p++;
    } else if (ISDIGIT(*p)) {
      errno = 0;
      len = strtoul(p, (char**)&p, 10);
      if (errno) {
        mrb_raise(mrb, E_RANGE_ERROR, "pack length too big");
      }
    } else {
      len = 1;
    }

    switch (type) {
      case 'C':		/* unsigned char */
        while (len-- > 0) {
          from = NEXTFROM;
          ch = (unsigned char)mrb_fixnum(from);
          mrb_str_buf_cat(mrb, result, &ch, sizeof(char));
        }
        break;

      case 'H':		/* hex string (high nibble first) */
        from = NEXTFROM;
        if (mrb_nil_p(from)) {
          ptr = "";
          plen = 0;
        } else {
          mrb_string_value(mrb, &from);
          ptr = RSTRING_PTR(from);
          plen = RSTRING_LEN(from);
        }

        if (p[-1] == '*')
          len = plen;
        {
          int byte = 0;
          long i, j = 0;

          if (len > plen) {
            j = (len + 1) / 2 - (plen + 1) / 2;
            len = plen;
          }
          for (i=0; i++ < len; ptr++) {
            if (ISALPHA(*ptr))
              byte |= ((*ptr & 15) + 9) & 15;
            else
              byte |= *ptr & 15;
            if (i & 1)
              byte <<= 4;
            else {
              char c = byte & 0xff;
              mrb_str_buf_cat(mrb, result, &c, 1);
              byte = 0;
            }
          }
          if (len & 1) {
            char c = byte & 0xff;
            mrb_str_buf_cat(mrb, result, &c, 1);
          }
          len = j;
          goto grow;
        }
        break;

      case 'm':		/* base64 encoded string */
        from = NEXTFROM;
        mrb_string_value(mrb, &from);
        ptr = RSTRING_PTR(from);
        plen = RSTRING_LEN(from);

        if (len == 0 && type == 'm') {
          encodes(mrb, result, ptr, plen, type, 0);
          ptr += plen;
          break;
        }
        if (len <= 2)
          len = 45;
        else
          len = len / 3 * 3;
        while (plen > 0) {
          long todo;

          if (plen > len)
            todo = len;
          else
            todo = plen;
          encodes(mrb, result, ptr, todo, type, 1);
          plen -= todo;
          ptr += todo;
        }
        break;

      case 'x':		/* null byte */
      grow:
        while (len >= 10) {
          mrb_str_buf_cat(mrb, result, nul10, 10);
          len -= 10;
        }
        mrb_str_buf_cat(mrb, result, nul10, len);
        break;

      default:
        break;

    }

  }

  return result;
}

#define PACK_LENGTH_ADJUST_SIZE(sz) do {	\
    tmp_len = 0;				\
    if (len > (long)((send-s)/(sz))) {		\
        if (!star) {				\
	    tmp_len = len-(send-s)/(sz);		\
        }					\
	len = (send-s)/(sz);			\
    }						\
} while (0)

#define PACK_ITEM_ADJUST() do { \
    if (tmp_len > 0 && !block_p) \
      mrb_ary_set(mrb, ary, RARRAY_LEN(ary)+tmp_len-1, mrb_nil_value()); \
} while (0)

static mrb_value
mrb_pack_unpack(mrb_state *mrb, mrb_value str)
{
  static const char hexdigits[] = "0123456789abcdef";
  char *s, *send;
  char *p, *pend;
  mrb_value ary, fmt, block;
  char type;
  long len;
  int star;
  int block_p = mrb_block_given_p();

#define UNPACK_PUSH(item) do {\
  mrb_value item_val = (item);\
  if (block_p) {\
    mrb_yield(mrb, item_val, block);\
  } else {\
    mrb_ary_push(mrb, ary, item_val);\
  }\
} while (0)

  mrb_get_args(mrb, "S&", &fmt, &block);

  s = RSTRING_PTR(str);
  send = s + RSTRING_LEN(str);
  p = RSTRING_PTR(fmt);
  pend = p + RSTRING_LEN(fmt);

  ary = block_p ? mrb_nil_value() : mrb_ary_new(mrb);
  while (p < pend) {
    int explicit_endian = 0;
    type = *p++;

    if (ISSPACE(type)) continue;
    if (type == '#') {
      while ((p < pend) && (*p != '\n')) {
        p++;
      }
      continue;
    }

    star = 0;
    {
modifiers:
      switch (*p) {
        case '_':
        case '!':

          if (strchr(natstr, type)) {
            p++;
          }
          else {
            mrb_raise(mrb, E_ARGUMENT_ERROR, "'%c' allowed only after types %s", *p, natstr);
          }
          goto modifiers;

        case '<':
        case '>':
          if (!strchr(endstr, type)) {
            mrb_raise(mrb, E_ARGUMENT_ERROR, "'%c' allowed only after types %s", *p, endstr);
          }
          if (explicit_endian) {
            mrb_raise(mrb, E_RANGE_ERROR, "Can't use both '<' and '>'");
          }
          explicit_endian = *p++;
          goto modifiers;
      }
    }

    if (p >= pend) {
      len = 1;
    } else if (*p == '*') {
      star = 1;
      len = send - s;
      p++;
    } else if (ISDIGIT(*p)) {
      errno = 0;
      len = strtoul(p, (char**)&p, 10);
      if (errno) {
        mrb_raise(mrb, E_RANGE_ERROR, "pack length too big");
      }
    } else {
      len = (type != '@');
    }

    switch (type) {
      case '%':
        mrb_raise(mrb, E_ARGUMENT_ERROR, "%% is not supported");
        break;

      case 'C':
        {
          unsigned char u8;

          if (p[-1] == '*' || len > send - s)
            len = send - s;
	  while (len-- > 0) {
	    u8 = *s++;
            UNPACK_PUSH(mrb_fixnum_value(u8));
	  }
        }
        break;

      case 'H':
        {
          mrb_value bitstr;
          char *t;
          int bits;
          long i;

          if (p[-1] == '*' || len > (send - s) * 2)
            len = (send - s) * 2;
          bits = 0;
          UNPACK_PUSH(bitstr = mrb_str_new(mrb, 0, len));
          t = RSTRING_PTR(bitstr);
          for (i=0; i<len; i++) {
            if (i & 1)
              bits <<= 4;
            else
              bits = *s++;
            *t++ = hexdigits[(bits >> 4) & 15];
          }
        }
        break;

      case 'm':
        {
          mrb_value buf = mrb_str_dup(mrb, str);
          char *ptr = RSTRING_PTR(buf);
          int a = -1,b = -1,c = 0,d = 0;
          static signed char b64_xtable[256];

          if (b64_xtable['/'] <= 0) {
            int i;

            for (i = 0; i < 256; i++) {
              b64_xtable[i] = -1;
            }
            for (i = 0; i < 64; i++) {
              b64_xtable[(unsigned char)b64_table[i]] = i;
            }
          }
          if (len == 0) {
            while (s < send) {
              a = b = c = d = -1;
              a = b64_xtable[(unsigned char)*s++];
              if (s >= send || a == -1) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
              b = b64_xtable[(unsigned char)*s++];
              if (s >= send || b == -1) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
              if (*s == '=') {
                if (s + 2 == send && *(s + 1) == '=') break;
                mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
              }
              c = b64_xtable[(unsigned char)*s++];
              if (s >= send || c == -1) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
              if (s + 1 == send && *s == '=') break;
              d = b64_xtable[(unsigned char)*s++];
              if (d == -1) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
              *ptr++ = a << 2 | b >> 4;
              *ptr++ = b << 4 | c >> 2;
              *ptr++ = c << 6 | d;
            }
            if (c == -1) {
              *ptr++ = a << 2 | b >> 4;
              if (b & 0xf) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
            }
            else if (d == -1) {
              *ptr++ = a << 2 | b >> 4;
              *ptr++ = b << 4 | c >> 2;
              if (c & 0x3) mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid base64");
            }
          }
          else {
            while (s < send) {
              a = b = c = d = -1;
              while ((a = b64_xtable[(unsigned char)*s]) == -1 && s < send) {s++;}
              if (s >= send) break;
              s++;
              while ((b = b64_xtable[(unsigned char)*s]) == -1 && s < send) {s++;}
              if (s >= send) break;
              s++;
              while ((c = b64_xtable[(unsigned char)*s]) == -1 && s < send) {if (*s == '=') break; s++;}
              if (*s == '=' || s >= send) break;
              s++;
              while ((d = b64_xtable[(unsigned char)*s]) == -1 && s < send) {if (*s == '=') break; s++;}
              if (*s == '=' || s >= send) break;
              s++;
              *ptr++ = a << 2 | b >> 4;
              *ptr++ = b << 4 | c >> 2;
              *ptr++ = c << 6 | d;
            }
            if (a != -1 && b != -1) {
              if (c == -1 && *s == '=')
                *ptr++ = a << 2 | b >> 4;
              else if (c != -1 && *s == '=') {
                *ptr++ = a << 2 | b >> 4;
                *ptr++ = b << 4 | c >> 2;
              }
            }
          }
          buf = mrb_str_new(mrb, RSTRING_PTR(buf), ptr - RSTRING_PTR(buf));
          UNPACK_PUSH(buf);
        }
        break;


      default:
        break;
    }

  }

return ary;
}

void
mrb_init_pack(mrb_state *mrb)
{
  struct RClass *cArray, *cString;

  cArray = mrb->array_class;
  cString = mrb->string_class;

  mrb_define_method(mrb, cArray, "pack", mrb_pack_pack, ARGS_REQ(1));
  mrb_define_method(mrb, cString, "unpack", mrb_pack_unpack, ARGS_REQ(1));
}
#endif /* ENABLE_PACK */
