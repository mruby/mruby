#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/irep.h>
#include <mruby/dump.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < sizeof(struct rite_binary_header)) {
        return 0;
    }
    mrb_state *mrb = mrb_open();
    if (!mrb) {
        return 0;
    }

    /* mrb_load_irep_buf returns the result of the last expression */
    mrb_load_irep_buf(mrb, Data, size);

    mrb_close(mrb);
    return 0;
}
