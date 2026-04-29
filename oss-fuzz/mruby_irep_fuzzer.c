#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/irep.h>

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t size) {
    if (size < 1) {
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
