#include "pool.h"
#include <string.h>

#undef TEST_POOL
#ifdef TEST_POOL
#include <stdio.h>

#define mrb_malloc(m,s) malloc(s)
#define mrb_free(m,p) free(p)
#endif

#define POOL_PAGE_SIZE 16000

mrb_pool*
mrb_pool_open(mrb_state *mrb)
{
  mrb_pool *pool = mrb_malloc(mrb, sizeof(mrb_pool));

  if (pool) {
    pool->mrb = mrb;
    pool->pages = 0;
  }

  return pool;
}

void
mrb_pool_close(mrb_pool *pool)
{
  struct mrb_pool_page *page, *tmp;

  if (!pool) return;
  page = pool->pages;
  while (page) {
    tmp = page;
    page = page->next;
    mrb_free(pool->mrb, tmp);
  }
  mrb_free(pool->mrb, pool);
}

static struct mrb_pool_page*
page_alloc(mrb_pool *pool, size_t len)
{
  struct mrb_pool_page *page;

  if (len < POOL_PAGE_SIZE)
    len = POOL_PAGE_SIZE;
  page = mrb_malloc(pool->mrb, sizeof(struct mrb_pool_page)+len-1);
  if (page) {
    page->offset = 0;
    page->len = len;
  }

  return page;
}

void*
mrb_pool_alloc(mrb_pool *pool, size_t len)
{
  struct mrb_pool_page *page;
  size_t n;

  if (!pool) return 0;

  page = pool->pages;
  while (page) {
    if (page->offset + len <= page->len) {
      n = page->offset;
      page->offset += len;
      page->last = (char*)page->page+n;
      return page->last;
    }
    page = page->next;
  }
  page = page_alloc(pool, len);
  if (!page) return 0;
  page->offset = len;
  page->next = pool->pages;
  pool->pages = page;

  page->last = (void*)page->page;
  return page->last;
}

int
mrb_pool_can_realloc(mrb_pool *pool, void *p, size_t len)
{
  struct mrb_pool_page *page;

  if (!pool) return 0;
  page = pool->pages;
  while (page) {
    if (page->last == p) {
      size_t beg;

      beg = (char*)p - page->page;
      if (beg + len > page->len) return 0;
      return 1;
    }
    page = page->next;
  }
  return 0;
}

void*
mrb_pool_realloc(mrb_pool *pool, void *p, size_t oldlen, size_t newlen)
{
  struct mrb_pool_page *page;
  void *np;

  if (!pool) return 0;
  page = pool->pages;
  while (page) {
    if (page->last == p) {
      size_t beg;

      beg = (char*)p - page->page;
      if (beg + oldlen != page->offset) break;
      if (beg + newlen > page->len) {
	page->offset = beg;
	break;
      }
      page->offset = beg + newlen;
      return p;
    }
    page = page->next;
  }
  np = mrb_pool_alloc(pool, newlen);
  memcpy(np, p, oldlen);
  return np;
}

#ifdef TEST_POOL
int
main()
{
  int i, len = 250;
  mrb_pool *pool;
  void *p;

  pool = mrb_pool_open(0);
  p = mrb_pool_alloc(pool, len);
  for (i=1; i<20; i++) {
    printf("%p (len=%d) %d\n", p, len, mrb_pool_can_realloc(pool, p, len*2));
    p = mrb_pool_realloc(pool, p, len, len*2);
    len *= 2;
  }
  mrb_pool_close(pool);
  return 0;
}
#endif
