#ifndef CARBUNCLE_AVL_H
#define CARBUNCLE_AVL_H

#include <mruby.h>
#include <mruby/data.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*mrb_avl_free_fn)(mrb_state *, void *p);

struct mrb_AVL;

struct mrb_AVL *
mrb_carbuncle_avl_new(mrb_state *mrb, mrb_avl_free_fn free_function);

void
mrb_carbuncle_avl_free(mrb_state *mrb, struct mrb_AVL *tree);

mrb_bool
mrb_carbuncle_avl_is_empty(struct mrb_AVL* tree);

void
mrb_carbuncle_avl_insert(mrb_state *mrb, struct mrb_AVL *tree, mrb_int value, void *data);

void
mrb_carbuncle_avl_remove(mrb_state *mrb, struct mrb_AVL *tree, mrb_int value);

mrb_bool
mrb_carbuncle_avl_contains(struct mrb_AVL *tree, mrb_int value);

void *
mrb_carbuncle_avl_data(struct mrb_AVL *tree, mrb_int value);

#ifdef __cplusplus
}
#endif

#endif
