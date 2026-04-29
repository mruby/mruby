#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include <mruby/variable.h>

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
        /* Integer / BigInt */
        if (S < 3) goto done;
        uint8_t base = D[0] % 37;
        if (base < 2 && base != 0) base = 10;
        uint8_t op = D[1] % 5;
        uint16_t split = D[2]; // Simplified split
        if (S > 3) split = split % (S - 3);
        else split = 0;

        mrb_value s1 = mrb_str_new(mrb, (const char *)(D + 3), split);
        mrb_value s2 = mrb_str_new(mrb, (const char *)(D + 3 + split), S - 3 - split);

        mrb_value b1 = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Integer", 2, s1, mrb_fixnum_value(base));
        mrb_value b2 = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Integer", 2, s2, mrb_fixnum_value(base));

        if (!mrb_nil_p(b1) && !mrb_nil_p(b2)) {
            switch (op) {
                case 0: mrb_funcall(mrb, b1, "+", 1, b2); break;
                case 1: mrb_funcall(mrb, b1, "-", 1, b2); break;
                case 2: mrb_funcall(mrb, b1, "*", 1, b2); break;
                case 3: 
                    if (mrb_test(mrb_funcall(mrb, b2, "!=", 1, mrb_fixnum_value(0)))) {
                        mrb_funcall(mrb, b1, "/", 1, b2);
                    }
                    break;
                case 4: 
                    if (mrb_test(mrb_funcall(mrb, b2, "<", 1, mrb_fixnum_value(1000)))) {
                       mrb_funcall(mrb, b1, "**", 1, b2);
                    }
                    break;
            }
        }
    } else if (selector == 1) {
        /* Rational */
        if (S < 4) goto done;
        mrb_int n = (mrb_int)D[0] | ((mrb_int)D[1] << 8);
        mrb_int d = (mrb_int)D[2] | ((mrb_int)D[3] << 8);
        mrb_value r = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Rational", 2, mrb_fixnum_value(n), mrb_fixnum_value(d));
        if (S >= 8) {
            mrb_int n2 = (mrb_int)D[4] | ((mrb_int)D[5] << 8);
            mrb_int d2 = (mrb_int)D[6] | ((mrb_int)D[7] << 8);
            mrb_value r2 = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Rational", 2, mrb_fixnum_value(n2), mrb_fixnum_value(d2));
            mrb_funcall(mrb, r, "+", 1, r2);
        }
    } else if (selector == 2) {
        /* Complex */
        if (S < 4) goto done;
        mrb_float re = (mrb_float)D[0];
        mrb_float im = (mrb_float)D[1];
        struct RClass *complex_class = mrb_class_get(mrb, "Complex");
        if (complex_class) {
            mrb_value c = mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "Complex", 2, mrb_float_value(mrb, re), mrb_float_value(mrb, im));
            mrb_funcall(mrb, c, "abs", 0);
        }
    } else if (selector == 3) {
        /* Math functions */
        if (S < 8) goto done;
        double d1;
        memcpy(&d1, D, sizeof(double));
        struct RClass *math_module = mrb_module_get(mrb, "Math");
        if (math_module) {
            mrb_value m = mrb_obj_value(math_module);
            mrb_funcall(mrb, m, "sin", 1, mrb_float_value(mrb, d1));
            mrb_funcall(mrb, m, "log", 1, mrb_float_value(mrb, d1));
            mrb_funcall(mrb, m, "sqrt", 1, mrb_float_value(mrb, d1));
        }
    }

done:
    mrb_close(mrb);
    return 0;
}
