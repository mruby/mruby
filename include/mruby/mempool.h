/*
** mempool.h - memory pool
**
** See Copyright Notice in mruby.h
*/

/* memory pool implementation */
typedef struct mempool mempool;
MRB_API struct mempool* mempool_open(void);
MRB_API void mempool_close(struct mempool*);
MRB_API void* mempool_alloc(struct mempool*, size_t);
MRB_API void* mempool_realloc(struct mempool*, void*, size_t oldlen, size_t newlen);

/* compatibility layer */
typedef struct mempool mrb_mempool;
#define mrb_mempool_open(m) mempool_open()
#define mrb_mempool_close(m) mempool_close(m)
#define mrb_mempool_alloc(m, size) mempool_alloc((m),(size))
#define mrb_mempool_realloc(m, ptr, oldlen, newlen) mempool_realloc((m),(ptr),(oldlen),(newlen))
