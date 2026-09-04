#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < 3) {
        return 0;
    }
    mrb_state *mrb = mrb_open();
    if (!mrb) {
        return 0;
    }

    /* Use first byte for flags */
    uint8_t flags_byte = Data[0];
    mrb_value flags = mrb_fixnum_value(flags_byte & 0x07); // i, m, x flags

    /* Split remaining data into pattern and string */
    uint8_t pattern_len = Data[1];
    if (pattern_len > size - 2) {
        pattern_len = size - 2;
    }

    mrb_value pattern = mrb_str_new(mrb, (const char *)(Data + 2), pattern_len);
    mrb_value text = mrb_str_new(mrb, (const char *)(Data + 2 + pattern_len), size - 2 - pattern_len);

    /* Target Regexp.new(pattern, flags) */
    struct RClass *regexp_class_ptr = mrb_class_get(mrb, "Regexp");
    if (regexp_class_ptr) {
        mrb_value regexp_class = mrb_obj_value(regexp_class_ptr);
        mrb_value regexp = mrb_funcall(mrb, regexp_class, "new", 2, pattern, flags);

        /* Target Regexp#match(text) */
        if (!mrb_nil_p(regexp)) {
            mrb_funcall(mrb, regexp, "match", 1, text);
        }
    }

    mrb_close(mrb);
    return 0;
}
