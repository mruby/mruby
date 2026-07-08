#ifndef MRC_PROC_H
#define MRC_PROC_H

MRC_BEGIN_DECL

#define MRC_OBJECT_HEADER \
  struct RClass *c;       \
  enum mrb_vtype tt:8;    \
  unsigned int gc_color:3; \
  unsigned int frozen:1;  \
  uint32_t flags:20

/* This struct mirrors RProc in mruby's <mruby/proc.h> (mrc_irep is
   layout-compatible with mrb_irep there). Guard it so both headers can
   coexist in one translation unit (e.g. the amalgamated build). */
#ifndef MRUBY_PROC_H
struct RProc {
  MRC_OBJECT_HEADER;
  union {
    const mrc_irep *irep;
    mrb_func_t func;
    mrb_sym mid;
  } body;
  const struct RProc *upper;
  union {
    struct RClass *target_class;
    struct REnv *env;
  } e;
};
#endif /* !MRUBY_PROC_H */

MRC_END_DECL

#endif // MRC_PROC_H
