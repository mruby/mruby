/*
** mruby/fiber.h - Fiber class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_FIBER_H
#define MRUBY_FIBER_H

#if defined(__cplusplus)
extern "C" {
#endif

mrb_value mrb_fiber_yield(mrb_state *mrb, int argc, mrb_value *argv);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif /* MRUBY_FIBER_H */
