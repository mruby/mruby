#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/data.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < 2) {
        return 0;
    }
    mrb_state *mrb = mrb_open();
    if (!mrb) {
        return 0;
    }

    uint8_t selector = Data[0] % 4;
    const uint8_t *D = Data + 1;
    size_t S = size - 1;

    if (selector == 0) {
        /* Target Set */
        struct RClass *set_class_ptr = mrb_class_get(mrb, "Set");
        if (set_class_ptr) {
            mrb_value set_class = mrb_obj_value(set_class_ptr);
            mrb_value ary = mrb_ary_new(mrb);
            for (size_t i = 0; i < S / 4 && i < 10; i++) {
                mrb_ary_push(mrb, ary, mrb_str_new(mrb, (const char *)(D + i*4), 4));
            }
            mrb_funcall(mrb, set_class, "new", 1, ary);
        }
    } else if (selector == 1) {
        /* Target Time */
        struct RClass *time_class_ptr = mrb_class_get(mrb, "Time");
        if (time_class_ptr) {
            mrb_value time_class = mrb_obj_value(time_class_ptr);
            if (S >= 8) {
                mrb_int sec = (mrb_int)D[0] | ((mrb_int)D[1] << 8) | ((mrb_int)D[2] << 16) | ((mrb_int)D[3] << 24);
                mrb_funcall(mrb, time_class, "at", 1, mrb_fixnum_value(sec));
            }
            mrb_funcall(mrb, time_class, "now", 0);
        }
    } else if (selector == 2) {
        /* Target Random */
        struct RClass *random_class_ptr = mrb_class_get(mrb, "Random");
        if (random_class_ptr) {
            mrb_value random_class = mrb_obj_value(random_class_ptr);
            if (S >= 4) {
                mrb_int seed = (mrb_int)D[0] | ((mrb_int)D[1] << 8);
                mrb_value rnd = mrb_funcall(mrb, random_class, "new", 1, mrb_fixnum_value(seed));
                mrb_funcall(mrb, rnd, "rand", 1, mrb_fixnum_value(100));
            }
        }
    } else if (selector == 3) {
        /* Target Range and Array-ext */
        if (S >= 4) {
            mrb_int start = (mrb_int)D[0];
            mrb_int end = (mrb_int)D[1];
            mrb_value range = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Range", 2, mrb_fixnum_value(start), mrb_fixnum_value(end));
            mrb_funcall(mrb, range, "to_a", 0);
        }
    }

    mrb_close(mrb);
    return 0;
}
