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

struct mempool_page {
  struct mempool_page *next;
  size_t offset;
  size_t len;
  void *last;
  char page[];
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

struct mempool {
  struct mempool_page *pages;
};

#ifndef TEST_POOL

/* use mruby's memory allocator */
#define malloc(s) mrb_basic_alloc_func(NULL, (s))
#define free(p) mrb_basic_alloc_func((p), 0)

#endif

#ifdef POOL_ALIGNMENT
#  define ALIGN_PADDING(x) ((SIZE_MAX - (x) + 1) & (POOL_ALIGNMENT - 1))
#else
#  define ALIGN_PADDING(x) (0)
#endif

MRB_API mempool*
mempool_open(void)
{
  mempool *pool = (mempool*)malloc(sizeof(struct mempool));

  if (pool) {
    pool->pages = NULL;
  }
  return pool;
}

MRB_API void
mempool_close(mempool *pool)
{
  struct mempool_page *page;

  if (!pool) return;
  page = pool->pages;
  while (page) {
    struct mempool_page *tmp = page;
    page = page->next;
    free(tmp);
  }
  free(pool);
}

static struct mempool_page*
page_alloc(mempool *pool, size_t len)
{
  if (len < POOL_PAGE_SIZE)
    len = POOL_PAGE_SIZE;

  struct mempool_page *page = (struct mempool_page*)malloc(sizeof(struct mempool_page)+len);
  if (page) {
    page->offset = 0;
    page->len = len;
  }

  return page;
}

MRB_API void*
mempool_alloc(mempool *pool, size_t len)
{
  struct mempool_page *page;

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
mempool_realloc(mempool *pool, void *p, size_t oldlen, size_t newlen)
{
  if (!pool) return NULL;
  if (newlen < oldlen) return p;
  oldlen += ALIGN_PADDING(oldlen);
  newlen += ALIGN_PADDING(newlen);
  for (struct mempool_page *page = pool->pages; page; page = page->next) {
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
  void *np = mempool_alloc(pool, newlen);
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
  mempool *pool;
  void *p;

  pool = mempool_open();
  p = mempool_alloc(pool, len);
  for (i=1; i<20; i++) {
    printf("%p (len=%d)\n", p, len);
    p = mempool_realloc(pool, p, len, len*2);
    len *= 2;
  }
  mempool_close(pool);
  return 0;
}
#endif
