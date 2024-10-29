/*
** mempool.h - memory pool
**
** See Copyright Notice in mruby.h
*/

/* memory pool implementation */
typedef struct mrb_mempool mrb_mempool;
MRB_API struct mrb_mempool* mrb_mempool_open(mrb_state*);
MRB_API void mrb_mempool_close(struct mrb_mempool*);
MRB_API void* mrb_mempool_alloc(struct mrb_mempool*, size_t);
MRB_API void* mrb_mempool_realloc(struct mrb_mempool*, void*, size_t oldlen, size_t newlen);
MRB_API mrb_bool mrb_mempool_can_realloc(struct mrb_mempool*, void*, size_t);
