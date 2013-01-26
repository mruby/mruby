#ifdef ENABLE_HOOKS

# define HOOK_MRB_OPEN(mrb) ((mrb)->hook_mrb_open ? (mrb)->hook_mrb_open(mrb) : NULL)
# define HOOK_MRB_CLOSE(mrb) ((mrb)->hook_mrb_close ? (mrb)->hook_mrb_close(mrb) : NULL)

# define HOOK_MRB_REALLOC(mrb, resultp, srcp, size) ((mrb)->hook_mrb_realloc ? (mrb)->hook_mrb_realloc((mrb), (resultp), (srcp), (size)) : NULL) 
# define HOOK_MRB_FREE(mrb, p) ((mrb)->hook_mrb_free ? (mrb)->hook_mrb_free((mrb), (p)) : NULL)

# define HOOK_MRB_READ_IREP(mrb, ret, bin) ((mrb)->hook_mrb_read_irep ? (mrb)->hook_mrb_read_irep((mrb), (ret), (bin)) : NULL)
# define HOOK_MRB_RUN_ENTER(mrb, proc, self) ((mrb)->hook_mrb_run_enter ? (mrb)->hook_mrb_run_enter((mrb), (proc), (self)) : NULL)
# define HOOK_MRB_RUN_EXIT(mrb, result, proc, self) ((mrb)->hook_mrb_run_exit ? (mrb)->hook_mrb_run_exit((mrb), (result), (proc), (self)) : NULL)

# define HOOK_VM_STEP(mrb, pc) ((mrb)->hook_vm_step ? (mrb)->hook_vm_step((mrb), (pc)) : NULL)
# define HOOK_GC_START(mrb) ((mrb)->hook_gc_start ? (mrb)->hook_gc_start(mrb) : NULL)
# define HOOK_GC_STOP(mrb) ((mrb)->hook_gc_stop ? (mrb)->hook_gc_stop(mrb) : NULL)

#else

# define HOOK_MRB_OPEN(mrb)
# define HOOK_MRB_CLOSE(mrb)

# define HOOK_MRB_REALLOC(mrb, resultp, srcp, size)
# define HOOK_MRB_FREE(mrb, p)

# define HOOK_MRB_READ_IREP(mrb, ret, bin)
# define HOOK_MRB_RUN_ENTER(mrb, proc, self)
# define HOOK_MRB_RUN_EXIT(mrb, proc, self)

# define HOOK_MRB_VM_STEP(mrb)
# define HOOK_MRB_GC_START(mrb)
# define HOOK_MRB_GC_STOP(mrb)

#endif
