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
 * - `size`: The new size of the memory region to be returned.
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
    /* `ralloc(NULL, size)` works as `malloc(size)` */
    return realloc(p, size);
  }
}
