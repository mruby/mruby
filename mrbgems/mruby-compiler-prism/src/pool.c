/*
** pool.c - memory pool
**
** See Copyright Notice in mruby.h
*/

#include "../include/mrc_ccontext.h"

/* configuration section */
/* allocated memory address should be multiple of POOL_ALIGNMENT */
/* or undef it if alignment does not matter */
#ifndef POOL_ALIGNMENT
#if INTPTR_MAX == INT64_MAX
#define POOL_ALIGNMENT 8
#else
#define POOL_ALIGNMENT 4
#endif
#endif
/* page size of memory pool */
#ifndef POOL_PAGE_SIZE
#define POOL_PAGE_SIZE 1024
#endif
/* end of configuration section */

/* Disable MSVC warning "C4200: nonstandard extension used: zero-sized array
 * in struct/union" when in C++ mode */
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4200)
#endif

struct mrc_pool_page {
  struct mrc_pool_page *next;
  size_t offset;
  size_t len;
  void *last;
  char page[];
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifdef POOL_ALIGNMENT
#  define ALIGN_PADDING(x) ((SIZE_MAX - (x) + 1) & (POOL_ALIGNMENT - 1))
#else
#  define ALIGN_PADDING(x) (0)
#endif

MRC_API mrc_pool*
mrc_pool_open(mrc_ccontext *c)
{
  mrc_pool *pool = (mrc_pool*)mrc_malloc(c, sizeof(mrc_pool));

  if (pool) {
    pool->c = c;
    pool->pages = NULL;
  }
  return pool;
}

MRC_API void
mrc_pool_close(mrc_pool *pool)
{
  struct mrc_pool_page *page;

  if (!pool) return;
  page = pool->pages;
  while (page) {
    struct mrc_pool_page *tmp = page;
    page = page->next;
    mrc_free(pool->c, tmp);
  }
  mrc_free(pool->c, pool);
}

static struct mrc_pool_page*
page_alloc(mrc_pool *pool, size_t len)
{
#if defined(MRC_TARGET_MRUBY)
  mrc_ccontext *c = pool->c;
#endif
  struct mrc_pool_page *page;

  if (len < POOL_PAGE_SIZE)
    len = POOL_PAGE_SIZE;
  page = (struct mrc_pool_page*)mrc_malloc(c, sizeof(struct mrc_pool_page)+len);
  if (page) {
    page->offset = 0;
    page->len = len;
  }

  return page;
}

MRC_API void*
mrc_pool_alloc(mrc_pool *pool, size_t len)
{
  struct mrc_pool_page *page;

  if (!pool) return NULL;
  len += ALIGN_PADDING(len);
  for (page = pool->pages; page; page = page->next) {
    if (page->offset + len <= page->len) {
      size_t n = page->offset;
      page->offset += len;
      page->last = (void*)(page->page+n);
      return page->last;
    }
  }
  page = page_alloc(pool, len);
  if (!page) return NULL;
  page->offset = len;
  page->next = pool->pages;
  pool->pages = page;

  page->last = (void*)page->page;
  return page->last;
}

MRC_API void*
mrc_pool_realloc(mrc_pool *pool, void *p, size_t oldlen, size_t newlen)
{
  if (!pool) return NULL;
  if (newlen < oldlen) return p;
  oldlen += ALIGN_PADDING(oldlen);
  newlen += ALIGN_PADDING(newlen);
  for (struct mrc_pool_page *page = pool->pages; page; page = page->next) {
    if (page->last == p) {
      /* if p is a last allocation from the page */
      size_t beg = (char*)p - page->page;
      /* check beg + oldlen points bottom */
      /* assert(beg + oldlen == page->offset) */
      if (beg + oldlen != page->offset) break;
      if (beg + newlen > page->len) {
        /* new allocation need more space */
        /* abandon this space */
        page->offset = beg;
        break;
      }
      page->offset = beg + newlen;
      return p;
    }
  }
  void *np = mrc_pool_alloc(pool, newlen);
  if (np == NULL) {
    return NULL;
  }
  memcpy(np, p, oldlen);
  return np;
}

