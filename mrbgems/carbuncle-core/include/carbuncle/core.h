#ifndef CARBUNCLE_CORE_H
#define CARBUNCLE_CORE_H

#define SUPPORT_FILEFORMAT_TTF 1

#include <mruby.h>
#include <mruby/data.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DATA_GET_DISPOSABLE_PTR(mrb, obj, type, cast) (cast *)mrb_check_disposed(mrb, obj, type);

void
mrb_carbuncle_raise(mrb_state *mrb, const char *name, const char *msg);

struct RClass *
mrb_carbuncle_define_data_class(mrb_state *mrb, const char *name, struct RClass *super);

void
mrb_carbuncle_check_frozen(mrb_state *mrb, mrb_value obj);

void
mrb_carbuncle_arg_error(mrb_state *mrb, const char *options, mrb_int argc);

void *
mrb_check_disposed(mrb_state *mrb, mrb_value obj, const struct mrb_data_type *type);

void
mrb_carbuncle_check_file(mrb_state *mrb, const char *filename);

struct RClass *
mrb_carbuncle_class_get(mrb_state *mrb, const char *name);

struct RClass *
mrb_carbuncle_module_get(mrb_state *mrb, const char *name);

struct RClass *
mrb_carbuncle_get(mrb_state *mrb);

static inline int
carbuncle_utf8_encode(char *out, uint32_t utf)
{
  if (utf <= 0x7F) {
    // Plain ASCII
    out[0] = (char) utf;
    out[1] = 0;
    return 1;
  }
  else if (utf <= 0x07FF) {
    // 2-byte unicode
    out[0] = (char) (((utf >> 6) & 0x1F) | 0xC0);
    out[1] = (char) (((utf >> 0) & 0x3F) | 0x80);
    out[2] = 0;
    return 2;
  }
  else if (utf <= 0xFFFF) {
    // 3-byte unicode
    out[0] = (char) (((utf >> 12) & 0x0F) | 0xE0);
    out[1] = (char) (((utf >>  6) & 0x3F) | 0x80);
    out[2] = (char) (((utf >>  0) & 0x3F) | 0x80);
    out[3] = 0;
    return 3;
  }
  else if (utf <= 0x10FFFF) {
    // 4-byte unicode
    out[0] = (char) (((utf >> 18) & 0x07) | 0xF0);
    out[1] = (char) (((utf >> 12) & 0x3F) | 0x80);
    out[2] = (char) (((utf >>  6) & 0x3F) | 0x80);
    out[3] = (char) (((utf >>  0) & 0x3F) | 0x80);
    out[4] = 0;
    return 4;
  }
  else { 
    // error - use replacement character
    out[0] = (char) 0xEF;  
    out[1] = (char) 0xBF;
    out[2] = (char) 0xBD;
    out[3] = 0;
    return 0;
  }
}

#ifdef __cplusplus
}
#endif

#endif
