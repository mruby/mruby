/*
** pool.h - memory pool
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include <stddef.h>

typedef struct mrb_pool {
  mrb_state *mrb;
  struct mrb_pool_page {
    struct mrb_pool_page *next;
    size_t offset;
    size_t len;
    void *last;
    char page[1];
  } *pages;
} mrb_pool;
