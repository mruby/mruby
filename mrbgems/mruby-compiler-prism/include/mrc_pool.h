#ifndef MRC_POOL_H
#define MRC_POOL_H

#include <stddef.h>
#include "mrc_ccontext.h"

MRC_BEGIN_DECL

typedef struct mrc_pool {
  struct mrc_ccontext *c;
  struct mrc_pool_page *pages;
} mrc_pool;


MRC_API mrc_pool *mrc_pool_open(struct mrc_ccontext *c);
MRC_API void mrc_pool_close(mrc_pool *pool);
MRC_API void *mrc_pool_alloc(mrc_pool *pool, size_t len);
MRC_API void *mrc_pool_realloc(mrc_pool *pool, void *p, size_t oldlen, size_t newlen);

MRC_END_DECL

#endif // MRC_POOL_H

