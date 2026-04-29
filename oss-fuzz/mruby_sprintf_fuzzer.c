#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/variable.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < 1) {
        return 0;
    }
    mrb_state *mrb = mrb_open();
    if (!mrb) {
        return 0;
    }

    /* Use the first byte as a format string length */
    uint8_t fmt_len = Data[0];
    if (fmt_len > size - 1) {
        fmt_len = size - 1;
    }
    
    mrb_value fmt = mrb_str_new(mrb, (const char *)(Data + 1), fmt_len);
    
    /* provide some arguments of different types to satisfy various format specifiers */
    mrb_value args[5];
    args[0] = mrb_fixnum_value(123);
    args[1] = mrb_float_value(mrb, 3.14);
    args[2] = mrb_str_new_cstr(mrb, "fuzz");
    args[3] = mrb_symbol_value(mrb_intern_cstr(mrb, "symbol"));
    args[4] = mrb_ary_new(mrb);
    
    /* Call sprintf via mrb_funcall */
    /* We don't use all args every time, but it doesn't hurt to pass them */
    mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "sprintf", 6, fmt, args[0], args[1], args[2], args[3], args[4]);

    mrb_close(mrb);
    return 0;
}
