/*
** allocf.c - default memory allocation function
**
** See Copyright Notice in mruby.h
*/
#include <stdlib.h>
#include <mruby.h>

/* This function serves as the default memory allocation function and accepts two arguments:
 *
 * - `p`: The previous pointer to the memory region. For memory allocation, this parameter is NULL.
 * - `size`: The new size of the memory region to be returned. If size is 0, the memory region will be freed.
 *
 * All memory allocation from the inside of mruby uses this function.
 *
 * If you want to use your own memory allocator, you have two options:
 *
 *  - provide your own version of malloc() / realloc() / free()
 *
 *  - redefine mrb_basic_alloc_func() in your application.
 *
 * See doc/guides/memory.md for detail.
 */

void*
mrb_basic_alloc_func(void *p, size_t size)
{
  if (size == 0) {
    /* `free(NULL)` should be no-op */
    free(p);
    return NULL;
  }
  else {
    /* `realloc(NULL, size)` should work as `malloc(size)` */
    return realloc(p, size);
  }
}
