#include <mruby.h>
#include <stdio.h>

static 
void hook_vm_step(struct mrb_state* mrb, mrb_code *pc) {
  printf("pc[%p] = %p\n", mrb, pc);
}

static 
void hook_mrb_open(struct mrb_state* mrb) {
  printf("mrb_open(%p)\n", mrb);
}

static 
void hook_mrb_close(struct mrb_state* mrb) {
  printf("mrb_close(%p)\n", mrb);
}

static 
void hook_mrb_realloc(struct mrb_state* mrb, void *resultp, void* srcp, size_t size) {
  printf("realloc(%p, %ld, %p)\n", mrb, size, resultp);
}

static 
void hook_mrb_free(struct mrb_state* mrb, void *p) {
  printf("free(%p, %p)\n", mrb, p);
}

static
void hook_mrb_read_irep(struct mrb_state* mrb, int ret, const char *bin) {
  printf("mrb_read_irep(%p, %d, %p)\n", mrb, ret, bin);
}

static
void hook_mrb_run_enter(struct mrb_state* mrb, struct RProc *proc, mrb_value self) {
  printf(">> mrb_run(%p)\n", mrb);
}

static
void hook_mrb_run_exit(struct mrb_state* mrb, mrb_value result, struct RProc *proc, mrb_value self) {
  printf("<< mrb_run(%p)\n", mrb);
}

static
void hook_gc_start(struct mrb_state* mrb) {
  printf("gc_start(%p)\n", mrb);
}

static
void hook_gc_stop(struct mrb_state* mrb) {
  printf("gc_stop(%p)\n", mrb);
}

void
mrb_debug_gem_init(mrb_state* mrb) {
#ifdef ENABLE_HOOKS
  mrb->hook_mrb_open = hook_mrb_open;
  mrb->hook_mrb_close = hook_mrb_close;
  mrb->hook_mrb_realloc = hook_mrb_realloc;
  mrb->hook_mrb_free = hook_mrb_free;
  mrb->hook_mrb_read_irep = hook_mrb_read_irep;
  mrb->hook_mrb_run_enter = hook_mrb_run_enter;
  mrb->hook_mrb_run_exit = hook_mrb_run_exit;
  mrb->hook_vm_step = hook_vm_step;
  mrb->hook_gc_start = hook_gc_start;
  mrb->hook_gc_stop = hook_gc_stop;
#endif
}

void
mrb_debug_gem_final(mrb_state* mrb) {
#ifdef ENABLE_HOOKS
  mrb->hook_mrb_open = NULL;
  mrb->hook_mrb_close = NULL;
  mrb->hook_mrb_realloc = NULL;
  mrb->hook_mrb_free = NULL;
  mrb->hook_mrb_read_irep = NULL;
  mrb->hook_mrb_run_enter = NULL;
  mrb->hook_mrb_run_exit = NULL;
  mrb->hook_vm_step = NULL;
  mrb->hook_gc_start = NULL;
  mrb->hook_gc_stop = NULL;
#endif
}
