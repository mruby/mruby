#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/string.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < 2) {
        return 0;
    }
    mrb_state *mrb = mrb_open();
    if (!mrb) {
        return 0;
    }

    uint8_t fmt_len = Data[0];
    if (fmt_len > size - 1) {
        fmt_len = size - 1;
    }
    
    mrb_value fmt = mrb_str_new(mrb, (const char *)(Data + 1), fmt_len);
    mrb_value str = mrb_str_new(mrb, (const char *)(Data + 1 + fmt_len), size - 1 - fmt_len);
    
    /* Target String#unpack */
    mrb_funcall(mrb, str, "unpack", 1, fmt);

    /* Target Array#pack (using the result of unpack if it's an array) */
    /* Or just pack the original string as an array of bytes */
    mrb_value ary = mrb_ary_new_capa(mrb, size);
    for (size_t i = 0; i < size; i++) {
        mrb_ary_push(mrb, ary, mrb_fixnum_value(Data[i]));
    }
    mrb_funcall(mrb, ary, "pack", 1, fmt);

    mrb_close(mrb);
    return 0;
}
