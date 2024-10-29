/*
** mempool.c - memory pool
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include <mruby.h>
#include <mruby/mempool.h>

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
#define POOL_PAGE_SIZE 16000
#endif
/* end of configuration section */

/* Disable MSVC warning "C4200: nonstandard extension used: zero-sized array
 * in struct/union" when in C++ mode */
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4200)
#endif

struct mrb_mempool_page {
  struct mrb_mempool_page *next;
  size_t offset;
  size_t len;
  void *last;
  char page[];
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

struct mrb_mempool {
  mrb_state *mrb;
  struct mrb_mempool_page *pages;
};

#undef TEST_POOL
#ifdef TEST_POOL

#define mrb_malloc_simple(m,s) malloc(s)
#define mrb_free(m,p) free(p)
#endif

#ifdef POOL_ALIGNMENT
#  define ALIGN_PADDING(x) ((SIZE_MAX - (x) + 1) & (POOL_ALIGNMENT - 1))
#else
#  define ALIGN_PADDING(x) (0)
#endif

MRB_API mrb_mempool*
mrb_mempool_open(mrb_state *mrb)
{
  mrb_mempool *pool = (mrb_mempool*)mrb_malloc_simple(mrb, sizeof(mrb_mempool));

  if (pool) {
    pool->mrb = mrb;
    pool->pages = NULL;
  }
  return pool;
}

MRB_API void
mrb_mempool_close(mrb_mempool *pool)
{
  struct mrb_mempool_page *page;

  if (!pool) return;
  page = pool->pages;
  while (page) {
    struct mrb_mempool_page *tmp = page;
    page = page->next;
    mrb_free(pool->mrb, tmp);
  }
  mrb_free(pool->mrb, pool);
}

static struct mrb_mempool_page*
page_alloc(mrb_mempool *pool, size_t len)
{
  struct mrb_mempool_page *page;

  if (len < POOL_PAGE_SIZE)
    len = POOL_PAGE_SIZE;
  page = (struct mrb_mempool_page*)mrb_malloc_simple(pool->mrb, sizeof(struct mrb_mempool_page)+len);
  if (page) {
    page->offset = 0;
    page->len = len;
  }

  return page;
}

MRB_API void*
mrb_mempool_alloc(mrb_mempool *pool, size_t len)
{
  struct mrb_mempool_page *page;

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

MRB_API void*
mrb_mempool_realloc(mrb_mempool *pool, void *p, size_t oldlen, size_t newlen)
{
  if (!pool) return NULL;
  if (newlen < oldlen) return p;
  oldlen += ALIGN_PADDING(oldlen);
  newlen += ALIGN_PADDING(newlen);
  for (struct mrb_mempool_page *page = pool->pages; page; page = page->next) {
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
  void *np = mrb_mempool_alloc(pool, newlen);
  if (np == NULL) {
    return NULL;
  }
  memcpy(np, p, oldlen);
  return np;
}

#ifdef TEST_POOL
int
main(void)
{
  int i, len = 250;
  mrb_mempool *pool;
  void *p;

  pool = mrb_mempool_open(NULL);
  p = mrb_mempool_alloc(pool, len);
  for (i=1; i<20; i++) {
    printf("%p (len=%d)\n", p, len);
    p = mrb_mempool_realloc(pool, p, len, len*2);
    len *= 2;
  }
  mrb_mempool_close(pool);
  return 0;
}
#endif
